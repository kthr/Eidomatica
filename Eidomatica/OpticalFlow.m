(* Mathematica Package *)

BeginPackage["Eidomatica`OpticalFlow`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Internal`"]
Needs["Eidomatica`Utilities`"]

(* Exported symbols added here with SymbolName::usage *)

Omega::usage = "Omega is an option for eOpticalFlow and eTotalVariationFilter."

eOpticalFlow::usage = "eOpticalFlow[reference,sample] computes the optical flow between the two given images.
eOpticalFlow[imageSequence] computes the optical flows between consecutive images of the given image sequence."
	InitializationVectorField::usage = "InitializationVectorField is an option for various optical flow algorithms specifying the intial vector field."
	Procedure::usage = "Procedure is an option for eOpticalFlow[] specifying the method which is used for computing the optical flow."
	ReferencePattern::usage = "ReferencePattern is an option for various functions computing optical flows. The given reference pattern is distorted by the computed flow and returned."
	(*HornSchunkOpticalFlow*)
	WeightFactor::usage = "WeightFactor is an option for eHornSchunkOpticalFlow[]"
	
	(*Steinbruecker optical flow*)
	SLambda::usage = "SLambda is an option for eSteinbrueckerOpticalFlow[] weighting the influence of the image intensities."
	Smoothness::usage = "Smoothness is an option for eSteinbrueckerOpticalFlow[] specifying the number of iterations done by eTotalVariationFilter[] and thus controlling the smoothness of the resulting vector field."
eTotalVariationFilter::usage = "eTotalVariationFilter[image] computes denoised version of the given image."

Begin["`Private`"] (* Begin Private Context *)

Clear[eApplyDisplacementField];
eApplyDisplacementField::incdim = "Pattern and vector field have incompatible dimensions.";
eApplyDisplacementField[pattern_Image, vectorField_VectorField] := Block[
	{data = Transpose@Reverse@ImageData@pattern,field=eVectorFieldData@vectorField},
  	(*option validation*)
  	If[! (Dimensions@data === Drop[Dimensions[field],-1]),
   		Message[eApplyDisplacementField::incdim];
   		Abort[]
   	];
   	(*implementation*)
  	Check[Image[Reverse@Transpose@MapIndexed[data[[Sequence@@Round[#2 - #1]]] &, field, {2}]], $Failed]
]

Clear[eChessBoardPattern];
eChessBoardPattern[nx_Integer, ny_Integer, width_Integer, bdepth_Integer: 255] := Module[{i, ii, j, jj, bm},
	bm = Table[(ii = Floor[i/width]; jj = Floor[j/width]; 
	If[EvenQ[ii + jj], bdepth, 0]),
		{j, 0, ny - 1}, 
		{i, 0, nx - 1}
	];
	Image@bm
]
  


Clear[eOpticalFlow];
eOpticalFlow::wdim = "Dimensions of supplied images are not identical.";
eOpticalFlow::nomemb = nomemb;
Options[eOpticalFlow] = {Procedure->"Steinbruecker",Iterations -> 10, WeightFactor -> .1, ReferencePattern -> None, ReturnAll -> False, InitializationVectorField -> None, SLambda -> 10, Omega -> 10, Radius -> 10, Smoothness -> 5, StepSize -> .245};
eOpticalFlow[reference_Image,sample_Image,opts:OptionsPattern[]]:=Block[
	{procedure},
	(*options validation*)
	procedure = OptionValue[Procedure];
	If[! (ImageDimensions[reference] === ImageDimensions[sample]),
  		Message[eOpticalFlow::wdim];
  		Abort[]
  	];
  	MemberCheck[eOpticalFlow,"Procedure",procedure,{"Steinbruecker","HornSchunk"}];
  	Switch[procedure,
  		"HornSchunk",
  		eHornSchunkOpticalFlow[reference,sample,Sequence@@FilterRules[{opts},First/@Options[eHornSchunkOpticalFlow]]],
  		"Steinbruecker",
  		eSteinbrueckerOpticalFlow[reference,sample,Sequence@@FilterRules[{opts},First/@Options[eSteinbrueckerOpticalFlow]]]
  	]
]
eOpticalFlow[images:{__Image},opts:OptionsPattern[]]:=Map[eOpticalFlow[Sequence@@#,opts]&,Transpose@{Drop[images,-1],Drop[images,1]}]

Clear[eHornSchunkOpticalFlow];
eHornSchunkOpticalFlow::noint = noint;
eHornSchunkOpticalFlow::nobool = nobool;
eHornSchunkOpticalFlow::nopos = nopos;
eHornSchunkOpticalFlow::novf = novf;
eHornSchunkOpticalFlow::invpat = "Supplied pattern is not valid, option 'ReferencePattern' has to be either 'Automatic','None' or a gray scale image of dimension equal to the given images.";
Options[eHornSchunkOpticalFlow] = {Iterations -> 10, WeightFactor -> .1, ReferencePattern -> None, ReturnAll -> False, InitializationVectorField -> None};
eHornSchunkOpticalFlow[reference_Image, sample_Image, opts : OptionsPattern[]] := Module[
	{modName=eHornSchunkOpticalFlow,iterations, alpha, refPat, returnAll, init, createPattern, dimensions, paddedReference, paddedSample, referenceData, sampleData, Ex, Ey, Et, denominator, kernel, average, iterate, averageU, averageV, tmp, u, v, field},
  	
  	(*options validation*)
  	{iterations, alpha, refPat, returnAll, init} = OptionValue[{Iterations, WeightFactor, ReferencePattern, ReturnAll, InitializationVectorField}];
  		(*iterations*)
  		IntegerCheck[modName,"Iterations",iterations];
  		IsPositive[modName,"Iterations",iterations];
  		(*weigth factor*)
  		NumberCheck[modName,"WeightFactor",alpha];
  		(*return all*)
  		IsBoolean[modName,"ReturnAll",returnAll];
  		(*initialization vector field*)
  		If[!(init===None),
  			IsVectorField[modName,"InitializationVectorField",init]
  		]  		;
  		(*reference pattern*)
  		If[refPat === None,
   			createPattern = False,
   			createPattern = True;
   			If[refPat === Automatic,
    			refPat = eChessBoardPattern[Sequence @@ ImageDimensions@reference, 20],
    			If[! (Head[refPat] === Image && ImageDimensions[refPat] === ImageDimensions[reference]),
     				Message[eHornSchunkOpticalFlow::invpat];
     				Abort[]
     			]
    		]
   		];
  	(*implementation*)
  	{referenceData, sampleData} = ImageData[#] & /@ {reference, sample};
  	dimensions = Dimensions@referenceData;
  	alpha = Table[alpha^2, Evaluate[Sequence @@ (List /@ dimensions)]];
  	paddedReference = ArrayPad[referenceData, 1];
  	paddedSample = ArrayPad[sampleData, 1];
  	Ex = .25 (ListConvolve[{{0, -1, 1}, {0, -1, 1}, {0, 0, 0}}, paddedReference] + ListConvolve[{{0, -1, 1}, {0, -1, 1}, {0, 0, 0}}, paddedSample]);
  	Ey = .25 (ListConvolve[{{0, 1, 1}, {0, -1, -1}, {0, 0, 0}}, paddedReference] + ListConvolve[{{0, 1, 1}, {0, -1, -1}, {0, 0, 0}}, paddedSample]);
  	Et = .25 (ListConvolve[{{0, -1, -1}, {0, -1, -1}, {0, 0, 0}}, paddedReference] + ListConvolve[{{0, 1, 1}, {0, 1, 1}, {0, 0, 0}}, paddedSample]);
  	denominator = alpha + Ex^2 + Ey^2;(*alpha + Ex^2 + Ey^2*)
  
  	kernel = N@{{1/12, 1/6, 1/12}, {1/6, 0, 1/6}, {1/12, 1/6, 1/12}};
  	average[x_] := ListConvolve[kernel, ArrayPad[x, 1, "Fixed"]];(*computes local averages*)
  
  	iterate[{u_, v_}] := (
    	averageU = average[u];
   		averageV = average[v];
    	tmp = (Ex*averageU + Ey*averageV + Et)/denominator;
    	{averageU - Ex*tmp, averageV - Ey*tmp}
    );
  	If[! returnAll,
   		{u, v} = Nest[iterate, Table[0., {2}, Evaluate[Sequence @@ (List /@ dimensions)]], iterations];
   		(*field = MapThread[{-#2, #1} &, {u, v}, 2];*)
   		field = VectorField[Transpose[{u,v},{3,1,2}]];
   		If[createPattern,
    		{field, eApplyDisplacementField[refPat, field]},(*else createPattern*)
    		field
    	],
    	(*else returnAll*)
   		{u, v} = Transpose@NestList[iterate, 
      	Table[0., {2}, Evaluate[Sequence @@ (List /@ dimensions)]], iterations];
   		VectorField/@Transpose[{u, v}, {4, 1, 2, 3}]
   ]
]
	
Clear[eSteinbrueckerOpticalFlow];
eSteinbrueckerOpticalFlow::illss = "Illegal value for option 'StepSize'. Value has to be less or equal to 0.25.";
eSteinbrueckerOpticalFlow::noint = noint;
eSteinbrueckerOpticalFlow::nobool = nobool;
eSteinbrueckerOpticalFlow::nopos = nopos;
eSteinbrueckerOpticalFlow::nonum = nonum;
Options[eSteinbrueckerOpticalFlow] = {Iterations -> 10, SLambda -> 10, Omega -> 10, Radius -> 10, ReturnAll -> False, Smoothness -> 5, StepSize -> .245};
eSteinbrueckerOpticalFlow[reference_Image, sample_Image, opts : OptionsPattern[]] := Module[
  {modName=eSteinbrueckerOpticalFlow,lambda, omega, radius, stepSize, iterations, returnAll, smoothness, iteration = 1, referenceData, sampleData, indexedReference, 
   indexedSample, dimX, dimY, initVF, minimizeAfterU, minimizeAfterV},
  
  (*option validation*)
  {iterations, lambda, omega, radius, returnAll, smoothness, stepSize} = OptionValue[{Iterations, SLambda, Omega, Radius, ReturnAll, Smoothness, StepSize}];
  	
  	(*iterations*)
  	IntegerCheck[modName,"Iterations",iterations]; 
  	IsPositive[modName,"Iterations",iterations];
  	(*lambda*)
  	NumberCheck[modName,"SLambda",lambda];
  	(*omega*)
  	NumberCheck[modName,"Omega",omega];
  	(*radius*)
  	IntegerCheck[modName,"Radius",radius]; 
  	IsPositive[modName,"Radius",radius];
  	(*returnAll*)
  	IsBoolean[modName,"ReturnAll",returnAll];
  	(*smoothness*)
	IntegerCheck[modName,"Smoothness",smoothness];
	(*stepSize*)
	NumberCheck[modName,"StepSize",stepSize];
  	If[stepSize > .25,
   		Message[eSteinbrueckerOpticalFlow::illss];
   		Abort[]
   	];
  
  (*implementation*)
  referenceData = ImageData@reference;
  sampleData = ImageData@sample;
  indexedReference = MapIndexed[{#1, #2} &, referenceData, {2}];
  indexedSample = MapIndexed[{#1, #2} &, sampleData, {2}];
  {dimX, dimY} = Take[Dimensions@referenceData, 2];
  initVF = Table[{0, 0}, {dimX}, {dimY}];
  
  (*complete search for vector field v*)
  
	minimizeAfterV[vectorFieldU_] := Block[
		{compute, a, b, searchWindow},
		(*energy computed pixel-wise*)
		compute[intensity_, origin_, sW_] := 
			Last@sW[[First@Ordering[
		    	N@Map[
		        	(1/(2*omega))*Sqrt@Total[((Last@#-origin) - vectorFieldU[[Sequence @@ origin]])^2] + lambda*Norm[intensity - First@#] &,
		       	sW]
		       ]]]-origin;
		ParallelMap[
			(
			   {a, b} = Last@#;(*current pixel coordinates (x,y)*)
			   searchWindow = Flatten[Take[indexedSample, {Max[1, a - radius], Min[dimX, a + radius]}, {Max[1, b - radius], Min[dimY, b + radius]}], {1, 2}];(*extract pixels to compare with*)
			   compute[First@#, {a, b}, searchWindow]
			) &,
			indexedReference, {2}, DistributedContexts -> Automatic
    	]
    ];
  	(*minimization after u with v fixed*)
  	minimizeAfterU[vectorFieldV_] := Block[
	    {iterate, compute, pBar, p, field},
	    DistributeDefinitions["Eidomatica`*"];
		DistributeDefinitions[omega, smoothness, stepSize];
	    field = 
	     	Transpose[
	      		ParallelMap[
	       			eTotalVariationFilter[#, Omega -> omega, Iterations -> smoothness, StepSize -> stepSize] &, Transpose[vectorFieldV, {2, 3, 1}], 
	       		DistributedContexts -> Automatic], 
	       	{3, 1, 2}];
	    iteration += 1;
	    omega = N[OptionValue[Omega]*Exp[-(5 iteration + 1)/(iterations/2)]];(*make omega smaller*)
	    field
	];
	Monitor[
  		If[returnAll,
			VectorField[#, "Image"] & /@ Drop[NestList[minimizeAfterU[minimizeAfterV[#]] &, initVF, iterations], 1],
			VectorField[Nest[minimizeAfterU[minimizeAfterV[#]] &, initVF, iterations], "Image"]
		],
		"Iteration: " <> ToString[iteration]
	]
]

Clear[eTotalVariationFilter];
eTotalVariationFilter::illss = "Illegal value for option 'StepSize'. Value has to be less or equal to 0.25.";
eTotalVariationFilter::noint = noint;
eTotalVariationFilter::nonum = nonum;
eTotalVariationFilter::nopos = nopos;
Options[eTotalVariationFilter] = {Iterations -> 10, Omega -> 10, StepSize -> .1};
eTotalVariationFilter[input : {{__?NumericQ} ..}, opts : OptionsPattern[]] := Module[
	{modName=eTotalVariationFilter, iterations, omega, stepSize, p0, iterate, pn},
  	
  	(*option validation*)
  	{iterations, omega, stepSize} = OptionValue[{Iterations, Omega, StepSize}];
  		(*iterations*)
  		IntegerCheck[modName,"Iterations",iterations];
  		IsPositive[modName,"Iterations",iterations];
  		(*omega*)
  		NumberCheck[modName,"Omega",omega];
  		(*stepSize*)
  		NumberCheck[modName,"StepSize",stepSize];
	  	If[stepSize > .25,
	   		Message[eTotalVariationFilter::illss];
	   		Abort[]
	   	];
	   	
	(*implementation*)
  	p0 = Table[{0, 0}, Evaluate[Sequence @@ (List /@ Dimensions[input])]];
  	iterate[pn_]:= Block[
    	{pBar},
    	pBar = pn + stepSize/omega (eFiniteDifferences[input + omega* eDivergence[pn]]);
    	pBar/Map[Max[1, #] &, pBar, {2}]
    ];
  	pn = Nest[iterate, p0, iterations];
  	input + omega*eDivergence[pn]
]
eTotalVariationFilter[input : {{{_?NumericQ, _?NumericQ, _?NumericQ} ..} ..}, opts : OptionsPattern[]] := 
	Transpose[eTotalVariationFilter[#, opts] & /@ Transpose[input, {2, 3, 1}], {3, 1, 2}]
eTotalVariationFilter[input_Image, opts : OptionsPattern[]] := eTotalVariationFilter[ImageData@input, opts]

End[] (* End Private Context *)

EndPackage[]
