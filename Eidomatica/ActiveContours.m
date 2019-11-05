(* Mathematica Package *)

(* Created by the Wolfram Workbench Oct 27, 2010 *)

BeginPackage["Eidomatica`ActiveContours`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Internal`"]
(* Exported symbols added here with SymbolName::usage *) 

(*Options appearing in several functions*)
MinimalError::usage="MinimalError is an option for several functions defining the error when the iteration process stops."
Nodes::usage="Nodes is an option for various methods specifying the number of control points to use."
WithScalingInvariance::usage="WithScalingInvariance includes size into shape statistics."

(*Gradient Vector Field & Snake*)
GradientVectorField::usage="GradientVectorField[image] generates a gradient vector field from the edge map of the supplied image."
	(*GradientVectorField Options*)
	SmoothingCoefficient::usage="SmoothingCoefficient is a real number controlling the smoothness of the generated vector field."
	EdgeDetector::usage="EdgeDetector is a function used for edge detection in the image, e.g. GradientFilter[#,10]& ."
GradientVectorFieldSnake::usage="GradientVectorFieldSnake[vectorField,contour] leting the contour converge on the supplied gradient vector field."
	(*GradientVectorFieldSnake Options*)
	Rigidity::usage="Rigidity is an option for GradientVectorFieldSnake."
	
(*Simple Diffusion Snake *)
SimpleDiffusionSnake::usage="SimpleDiffusionSnake[image, initialization] iterates a simple diffusion snake, where image is a grayscale or binary image and initialization is a list of points defining the initial contour."
	(*Simple Diffusion Snake - Options*)
	ShapeEnergyFunction::usage="ShapeEnergyFunction is an option for SimpleDiffusionSnake[], when given then prior knowledge modeled by the ShapeEnergyFunction[] is introduced in iteration process."
	ShapeEnergyParameter::usage="ShapeEnergyParameter is an option for SimpleDiffusionSnake[], which controls the influence of the prior knowledge onto the iteration process."
	Tension::usage="Tension is an option for SimpleDiffusionSnake, which controls the influence of the tension term (length of the contour)."
	ReparameterizationDistance::usage="ReparameterizationDistance is an option for SimpleDiffusionSnake[] which defines the minimal distance between two control points at which reparameterization of the contour is done."
	ShapeWeight::usage="ShapeWeight defines strength of shape prior relative to image energy."

(*Shape Prior*)
ShapeEnergy::usage="ShapeEnergy[contours] computes a shape prior from supplied contours and returns a ShapeEnergyFunction[contour, <>] which can be used to introduce the shape prior into the SimpleDiffusionSnake[..., ShapeEnergyFunction->shapeEnergyFunction[#]&]"

ShapeEnergyFunction::usage="ShapeEnergyFunction[contour, <>] represents a shape prior computed by ShapeEnergy."

(*Utilities*)
AlignControlPoints::usage="AlignControlPoints returns list shift in right direction for control point alignment."
AlignContour::usage="AlignContour[reference, sample] aligns the contour sample with respect to contour reference."
AlignContours::usage="AlignContours[contours] aligns contours so that the procrustes distance between the contours and their mean is minimal. The function returns a mean contour and the aligned contours in a list {mean, alignedContours}."
ArcLengthReparameterization::usage="ArcLengthReparameterization[contour] reparameterizes the supplied contour so that the base points are equidistantly placed on the contour outline."
	ClosedContour::usage="ClosedContour is an option for ArcLengthReparameterization[] which specifies if the given contour shall be treated as closed or not."
CenterAndNormalize::usage="CenterAndNormalize[contour] centers and normalizes the given contour."
ComplexRepresentation::usage="ComplexRepresentation[contour] transforms contour into complex representation."
CounterClockwise::usage="CounterClockwise[contour] gives True if the supplied contour is counterclockwise orientated otherwise False."
eCreateContour::usage = "eCreateContour[center, radius] creates a circular contour at the given center and with given radius."
CreateMask::usage="CreateMask[height,width,contour] creates an binary image where the region specified by contour is masked out."
FiniteDifferences::usage="FiniteDifferences[contour, n] gives the nth finite difference of contour, assuming it is a CLOSED contour."
ListRepresentation::usage="ListRepresentation[contour] transforms contour into list representation."
PointwiseRepresentation::usage="PointwiseRepresentation[contour] transform contour into pointwise representation."
ProcrustesDistance::usage="ProcrustesDistance[contour1, contour2] gives the procrustes distance between the two contours."
Reparameterize::usage="Reparameterize[contour] reparameterizes the given contour, assuming an BSpline representation."
Resample::usage="Resample[contours] resamples contours such that they have the specified number of control points (all returned contours are orientated counter clockwise)."

Begin["`Private`"]
(* Implementation of the package *)

(*Gradient vector field*)
Clear[GradientVectorField];
Options[GradientVectorField] = {Iterations -> 250, StepSize -> .1,SmoothingCoefficient -> .5, MinimalError -> 0.0001,EdgeDetector -> (GradientFilter[#, 5] &)};
GradientVectorField::noint = "Option '`1`' has to be a positive integer";
GradientVectorField::noreal = "Option '`1`' has to be a real number";
GradientVectorField::noconvergence = "r=\[Mu]*\[CapitalDelta]t (SmoothingCoefficient*StepSize) has to be smaller than 0.25 to guarantee convergence";
GradientVectorField::itlimit = "Iteration limit of `1` reached";
GradientVectorField::noedge = "Function specified by 'EdgeDetector' is no valid function, it has to by either GradientFilter, LaplacianGaussianFilter or LaplacianFilter";
GradientVectorField::nograyscale="Supplied image has to be a grayscale or binary image";
GradientVectorField[image_Image, opts : OptionsPattern[]] := Module[
  {iterationLimitReached = True, \[CapitalDelta]x, \[CapitalDelta]y, \[CapitalDelta]t, \[Mu],iterations, minimalError, edgeDetector, r, imageMatrix, mask, dx, 
   dy, b, c1, c2, b1, iterate, terminationCondition, u, v},
  {\[Mu], \[CapitalDelta]t, iterations, minimalError,edgeDetector} = OptionValue[{SmoothingCoefficient, StepSize, Iterations,MinimalError, EdgeDetector}];
  
  (*Option validation*)
  {\[CapitalDelta]x, \[CapitalDelta]y} = {1,1};(*compute gradient vector field on coarser grid (not implemented)*)
  r = (\[Mu]*\[CapitalDelta]t)/(\[CapitalDelta]x*\[CapitalDelta]y);
  If[! NumericQ[\[Mu]],
   Message[GradientVectorField::noreal, "SmoothingCoefficient"];
   Return[$Failed]
   ];
  If[! NumericQ[\[CapitalDelta]t],
   Message[GradientVectorField::noreal, "StepSize"];
   Return[$Failed]
   ];
  If[! IntegerQ[iterations],
   Message[GradientVectorField::noint, "Iterations"];
   Return[$Failed]
   ];
  If[r > 1/4 ,
   Message[GradientVectorField::noconvergence];
   Return[$Failed]
   ];
  If[! NumericQ[minimalError],
   	Message[GradientVectorField::noreal, "MinimalError"];
   	Return[$Failed]
  ];
  If[! MemberQ[{"GradientFilter", "LaplacianGaussianFilter", 
  	"LaplacianFilter"}, 
   	ToString@Head@First[edgeDetector] && 
    	Head[edgeDetector] === Function],
   	Message[GradientVectorField::noedge];
   	Return[$Failed]
   ];
  If[ImageChannels[image]!=1,
	Message[GradientVectorField::nograyscale];
	Return[$Failed]
  ];	
  
  (*start implementation*)  
  imageMatrix = ImageData[edgeDetector[image]];
  mask = {{0, 0, 0}, {0, -1, 1}, {0, 0, 0}};
  {dx, dy} = ListConvolve[#, imageMatrix, 2] & /@ {mask, Transpose@mask};
  b = (Plus @@ (#*# & /@ {dx,dy}))*\[CapitalDelta]t;(*like coefficient b in the paper but multiplied by \[CapitalDelta]t*)
  {c1, c2} = b*# & /@ {dx,dy};(*like coefficients c^1 and c^2 in the paper but multiplied by \[CapitalDelta]t*)
  b1 = 1 - b;
  iterate[initial_, c_] := b1*initial + r*ListConvolve[{{0, 1, 0}, {1, -4, 1}, {0, 1, 0}}, initial, 2] + c;
  (*terminationCondition[last_, current_] := (*bad termination condition needs re-implementation*)
   If[Total[Abs@Flatten[(current - last)]] < minimalError,
    iterationLimitReached = False; False,
    True];*)
  {u, v} = 
  Nest[
  	{iterate[First@#, c1], iterate[Last@#, c2]} &,
    {dx, dy},
    iterations
  ];
  If[iterationLimitReached,Message[GradientVectorField::itlimit, iterations]];
  Reverse/@Transpose@MapThread[{-#1, #2} &, {u, v}, 2]
]

(*Gradient vector field snake*)
Clear[GradientVectorFieldSnake];
Options[GradientVectorFieldSnake] = {Nodes -> 100, Tension -> .3,Rigidity -> .3, StepSize -> .2, Iterations -> 50, MinimalError -> 0.000001, ReturnAll -> False};
GradientVectorFieldSnake::noint = "Option '`1`' has to be a positive integer";
GradientVectorFieldSnake::noreal = "Option '`1`' has to be a real number";
GradientVectorFieldSnake::itlimit = "Iteration limit `1` reached";
GradientVectorFieldSnake::nobool = "Option '`1`' has to be either 'True' or 'False'";
GradientVectorFieldSnake[vectorField : {{{_Real, _Real} ..} ..}, controlPoints : {{_?NumericQ, _?NumericQ} ..}, opts : OptionsPattern[]] := 
 Module[
  {iterationLimitReached = True, bSpline, x, y, mask, dimensions, fx, fy, \[Alpha], \[Beta], snaxels, \[CapitalDelta]t, iterations, error, returnAll, iterate, next, 
   terminationCondition, a, b, c, A, result},
  {\[Alpha], \[Beta], snaxels, \[CapitalDelta]t, iterations, error, returnAll} = OptionValue[{Tension, Rigidity, Nodes, StepSize, Iterations, MinimalError, ReturnAll}];
  
  (*option validation*)
  If[! Head[\[Alpha]] === Real,
   Message[GradientVectorFieldSnake::noreal, "Tension"];
   Return[$Failed]
   ];
  If[! Head[\[Beta]] === Real,
   Message[GradientVectorFieldSnake::noreal, "Rigidity"];
   Return[$Failed]
   ];
  If[! IntegerQ[snaxels],
   Message[GradientVectorFieldSnake::noint, "Nodes"];
   Return[$Failed]
   ];
  If[! Head[\[CapitalDelta]t] === Real,
   Message[GradientVectorFieldSnake::noreal, "StepSize"];
   Return[$Failed]
   ];
  If[! IntegerQ[iterations],
   Message[GradientVectorFieldSnake::noint, "Iterations"];
   Return[$Failed]
   ];
  If[! NumericQ[error],
   Message[GradientVectorFieldSnake::noreal, "MinimalError"];
   Return[$Failed]
   ];
  If[! (returnAll === True || returnAll === False),
   Message[GradientVectorFieldSnake::nobool, ReturnAll];
   Return[$Failed]
   ];
  
  (*start implementation*)  
  bSpline = BSplineFunction[controlPoints, SplineDegree -> 2, SplineClosed -> True, SplineKnots -> "Unclamped"];
  {x, y} = Transpose@Table[bSpline[s], {s, 0, 1 - 1/snaxels, 1/snaxels}];
  mask = {{0, 0, 0}, {0, -1, 1}, {0, 0, 0}};
  dimensions = Dimensions[vectorField];
  fx = ListInterpolation[Map[First@# &,vectorField, {2}], {{0, dimensions[[1]]}, {0, dimensions[[2]]}}];
  fy = ListInterpolation[Map[Last@# &, vectorField, {2}], {{0, dimensions[[1]]}, {0,dimensions[[2]]}}];
  {a, b, c} = {\[Alpha], -(4 \[Beta] + \[Alpha]), 6 \[Beta] + 2 \[Alpha]};
  A = Table[RotateRight[PadLeft[{a, b, c, b, a}, snaxels, 0], 3 + i], {i, 0, snaxels - 1}];
  terminationCondition[last_, current_] := 
  If[areaCriterion[Transpose@last, Transpose@current] < error, 
   		iterationLimitReached = False; False, 
   		True
  ];
  iterate[v_] := (
    next = {First[v] - \[CapitalDelta]t*(A.First[v] - fx @@@ Transpose[v]), Last[v] - \[CapitalDelta]t*(A .Last[v] - fy @@@ Transpose[v])};
  	Transpose@ArcLengthReparameterization[Transpose@next]
	);
  If[returnAll,
   	result = Transpose /@ NestWhileList[iterate, {x, y}, terminationCondition, 2, iterations],
   	result = Transpose@NestWhile[iterate, {x, y}, terminationCondition, 2, iterations]
   ];
  If[iterationLimitReached, Message[GradientVectorFieldSnake::itlimit, iterations]];
  result
]

(*Procrustes Distance*)
Clear[ProcrustesDistance];
ProcrustesDistance[contour1 : {{_Real, _Real} ..}, contour2 : {{_Real, _Real} ..}] := Module[
  {normalizedCentered},
  normalizedCentered = CenterAndNormalize[{contour1, contour2}] // ComplexRepresentation;
  1 - (Conjugate[First@normalizedCentered].AlignContour[First@normalizedCentered, Last@normalizedCentered])^2 // Chop
]

(*Simple diffusion snake*)
Clear[SimpleDiffusionSnake];
Options[SimpleDiffusionSnake]:={Nodes->100,Iterations->500,Tension->0.3,ReturnAll->False,StepSize->1.,MinimalError->0.00001,ShapeEnergyFunction->None,ShapeEnergyParameter->.3,ReparameterizationDistance->.5,ShapeWeight->0.5};
SimpleDiffusionSnake::noint="Option '`1`' has to be a positive, non-zero integer";
SimpleDiffusionSnake::nocc="Polygon(s) spanned by control points have to be orientated counterclockwise, check with CounterClockwise[]";
SimpleDiffusionSnake::nograyscale="Supplied image has to be a grayscale or binary image";
SimpleDiffusionSnake::nobool="Option '`1`' has to be either 'True' or 'False'";
SimpleDiffusionSnake::noposn="Option '`1`' has to be a positive, non-zero number";
SimpleDiffusionSnake::noreal="Option '`1`' has to be a positive number";
SimpleDiffusionSnake::itlimit="Snake `1` reached teration limit at `2`";
SimpleDiffusionSnake::invshape="Option 'ShapeEnergyFunction' has to be either 'None' or a 'ShapeEnergyFunction'";
SimpleDiffusionSnake::panic="Woaah, snake ain't gonna converge";
SimpleDiffusionSnake[image_Image,controlPoints:{{_Real,_Real}..},opts:OptionsPattern[]]:=SimpleDiffusionSnake[image,{controlPoints},opts]
SimpleDiffusionSnake[image_Image,controlPoints:{{{_Real,_Real}..}..},opts:OptionsPattern[]]:=Module[
{iterationLimitReached=Table[True,{Length@controlPoints}],withPrior=False,nNodes,iterations,\[Alpha],returnAll,stepsize,error,shapeEnergy,shapeEnergyParameter,reparameterizationDistance,dimensions,data,basis,inverseB,
	iterate,terminationCondition,finished=Table[False,{Length@controlPoints}],bSpline,nodes,rotationMatrix,curvePts,normalVectors,labelInside,labelOutside,nInside,nOutside,inside,outside,meanInside,meanOutside,indices,
	eMinus,ePlus,derivative,fidelityTerm,intermediate,status,result,beta},
	
	{nNodes,iterations,\[Alpha],returnAll,stepsize,error,shapeEnergyParameter,reparameterizationDistance,beta}=OptionValue[{Nodes,Iterations,Tension,ReturnAll,StepSize,MinimalError,ShapeEnergyParameter,ReparameterizationDistance,ShapeWeight}];
	shapeEnergy=OptionValue[ShapeEnergyFunction];
	(*option validation*)
	If[!IntegerQ[nNodes]||!nNodes>0,
		Message[SimpleDiffusionSnake::noint,Nodes];
		Return[$Failed]
	];
	If[!IntegerQ[iterations]||!iterations>0,
		Message[SimpleDiffusionSnake::noint,Iterations];
		Return[$Failed]
	];
	If[!(returnAll===True||returnAll===False),
		Message[SimpleDiffusionSnake::nobool,ReturnAll];
		Return[$Failed]
	];
	If[!CounterClockwise[controlPoints],
		Message[SimpleDiffusionSnake::nocc];
		Return[$Failed]
	];
	If[ImageChannels[image]!=1,
		Message[SimpleDiffusionSnake::nograyscale];
		Return[$Failed]
	];
	If[!(NumericQ[stepsize]&&stepsize>0),
		Message[SimpleDiffusionSnake::noposn,StepSize];
		Return[$Failed]
	];
	If[!(NumericQ[error]&&error>0),
	Message[SimpleDiffusionSnake::noreal,MinimalError];
	Return[$Failed]
	];
	If[!(shapeEnergy===None||Head[shapeEnergy]===Function),
		Message[SimpleDiffusionSnake::invshape];
		Return[$Failed],
		If[Head[shapeEnergy]===Function&&Head[First@shapeEnergy]===ShapeEnergyFunction,
			withPrior=True
		]
	];
	If[!(NumericQ[shapeEnergyParameter]&&shapeEnergyParameter>0),
		Message[SimpleDiffusionSnake::noposn,ShapeEnergyParameter];
		Return[$Failed]
	];
	If[!(NumericQ[reparameterizationDistance]&&reparameterizationDistance>0),
		Message[SimpleDiffusionSnake::noposn,ReparameterizationDistance];
		Return[$Failed]
	];
	
	(*start implementation*)
	dimensions=ImageDimensions@image;
	data=ImageData[image];
	basis=Table[RotateRight[PadRight[Table[BSplineBasis[2,i,5/6],{i,0,2}],nNodes],i-1],{i,0,nNodes-1}];
	inverseB=Inverse@basis;
	bSpline=BSplineFunction[#,SplineDegree->2,SplineClosed->True,SplineKnots->"Unclamped"]&/@controlPoints;
	nodes=LinearSolve[basis,Table[#[(s+.5)/nNodes],{s,0,nNodes-1}]]&/@bSpline;
	rotationMatrix=RotationMatrix[90Degree];
	
	iterate[ctrPoints_]:=(
		bSpline=BSplineFunction[#,SplineClosed->True,SplineKnots->"Unclamped",SplineDegree->2]&/@ctrPoints;
		curvePts=Table[#[(s+.5)/nNodes],{s,0,nNodes-1}]&/@bSpline;
		normalVectors=Map[Normalize[#.rotationMatrix]&,(Table[#'[(s+.5)/nNodes],{s,0,nNodes-1}]&/@bSpline),{2}];(*list of normal vectors for each contour*)
		labelInside=CreateMask[Sequence@@dimensions,#,ReturnType->"Matrix"]&/@curvePts;(*list of binary matrices labeling inner area for each contour*)
		labelOutside=Map[-(#-1)&,Total[labelInside],{2}];(*single binary matrix labeling outer area of ALL contours*)
		nInside=Total@Flatten@#&/@labelInside;(*list of integers counting pixels inside of each contour*)
		nOutside=Times@@dimensions-Total[nInside];(*integer value giving count of pixels outside of ALL contours*)
		inside=#*data&/@labelInside;(*image multiplied by label for each contour*)
		outside=labelOutside*data;(*image multiplied by label for outer area*)
		meanInside=MapThread[Total@Flatten[#1]/#2&,{inside,nInside}];
		meanOutside=Total@Flatten@outside/nOutside;
		indices=Map[{Last@dimensions-Last@#,First@#}&,(Round[#]&/@curvePts),{2}];(*list of integer coordinates specifying the pixels on which contours are 'lying' (for each contour)*)
		
		(*
		If[
			(Or@@(# < 1 & /@ (First@#)) || (*tests if minimum of both iIndeces and oIndices is less than 1 (outside of image) *)
   			Or@@GreaterEqual@@@Transpose@{Last@#, dimensions}) &@ (*tests if maximum of both iIndeces and oIndices is greater than image coordinates*)
 			Transpose[{Min@#, Max@#} &/@ (Table[#[[All, i]], {i, 2}] &@indices)],(*returns a list with {{xmin,ymin},{xmax,ymax}} from the list of points achieved by joining iIndices and oIndices*)
 			Message[SimpleDiffusionSnake::panic];
 			Abort[]
		];
		*)
		
		eMinus=MapThread[(Extract[data,#1]-#2)^2&,{indices,meanInside}];(*summed squared distance of inside pixels to meanInside for each contour*)
		ePlus=Map[(Extract[data,#1]-meanOutside)^2&,indices];
		
		(*
		eMinus=#*#&@((data[[Last@dimensions-Last@#,First@#]]&/@iIndices)-meanInside);(*inside*)
		ePlus=#*#&@((data[[Last@dimensions-Last@#,First@#]]&/@oIndices)-meanOutside);(*outside*)
		*)
		derivative=FiniteDifferences[ctrPoints,2];
		fidelityTerm=MapThread[#1-#2&,{ePlus,eMinus}];(*list of fidelity terms*)
		intermediate=
		If[!withPrior,
			MapThread[(*without prior*)
				If[#5,(*if finished do not iterate further*)
					#1,
					#1+(stepsize*inverseB.(#2*#3+\[Alpha]*#4))
				]&,
				{ctrPoints,fidelityTerm,normalVectors,derivative,finished}
			],
			MapThread[(*with prior*)
				If[#5,(*if finished do not iterate further*)
					#1,
					#1+stepsize*(inverseB.((#2*#3+\[Alpha]*#4)+shapeEnergyParameter*CheckAbort[shapeEnergy[#1,Size->size],$Aborted]))
				]&,
				{ctrPoints,fidelityTerm,normalVectors,derivative,finished}
			]
		];	
      	Map[
			If[Min[Norm/@FiniteDifferences[#]]<reparameterizationDistance,
				Monitor[
					status="Reparameterizing...";
					Reparameterize[#],
					status
				],
				#
			]&,
			intermediate
		]	
	);
	terminationCondition[last_, current_] := (
	finished=#<error&/@areaCriterion[last,current];
	If[And@@finished, 
   		iterationLimitReached = Not/@finished; False, True]);
	If[returnAll,
		result=Transpose@NestWhileList[iterate,nodes,terminationCondition,2,iterations],
		result=NestWhile[iterate,nodes,terminationCondition,2,iterations]
	];
	MapIndexed[If[#,Message[SimpleDiffusionSnake::itlimit,First@#2,iterations]]&,iterationLimitReached];
	result
]

(*Area stopping criterion*)
Clear[areaCriterion];
areaCriterion[a:{{{_Real, _Real}..}..}, b : {{{_Real, _Real}..}..}]:=MapThread[areaCriterion[#1,#2]&,{a,b}]
areaCriterion[a : {{_Real, _Real} ..}, b : {{_Real, _Real} ..}] := 
Block[
  {tmp},
  (*area[poly_] := .5 Abs@Total@MapThread[(#1[[1]] + #2[[1]])*(#1[[2]]-#2[[2]]) &, {poly, RotateLeft[poly, 1]}];*)
  tmp=area[a];
  Abs[(tmp - area[b])/tmp]
]

Clear[area]
area=Compile[{{poly, _Real, 2}}, .5 Abs@Total@MapThread[(#1[[1]] + #2[[1]])*(#1[[2]] - #2[[2]]) &, {poly,RotateLeft[poly, 1]}]];

(*Arc length reparameterization*)
Clear[ArcLengthReparameterization];
Options[ArcLengthReparameterization]:={ClosedContour->True};
ArcLengthReparameterization::nobool = "Option '`1`' has to be either 'True' or 'False'";
ArcLengthReparameterization[pts:{{_Real,_Real}..},opts:OptionsPattern[]]:=Module[
{closedContour,closed,parts,length,currentSpan=1,traversedDistance=0,stepSize,test,newPts={}},

	closedContour=OptionValue[ClosedContour];
	
	(*option validation*)
	If[! (closedContour === True || closedContour === False),
   		Message[ArcLengthReparameterization::nobool, ClosedContour];
   		Return[$Failed]
   	];
   	
   	(*start implementation*)
   	closed=If[closedContour,
		Append[pts,First@pts],
		pts
   	];
	parts=MapThread[Norm[#2-#1]&,{closed,RotateLeft[closed,1]}];
	length=Total@parts;
	stepSize=length/Length[pts];
	While[traversedDistance<length,
		test=(traversedDistance+stepSize)/Total@Take[parts,currentSpan];
		If[test<=1.,
			AppendTo[newPts,closed[[currentSpan]]+(stepSize+traversedDistance-Total@Take[parts,currentSpan-1])/parts[[currentSpan]]*(closed[[currentSpan+1]]-closed[[currentSpan]])];
			traversedDistance+=stepSize,
		currentSpan+=1
		]
	];
	newPts
]

(*Counter clockwise*)
Clear[CounterClockwise];
CounterClockwise[curves:{{{_Real,_Real}..}..}]:=And@@(CounterClockwise/@curves);
CounterClockwise[curve:{{_Real,_Real}..}]:=Module[
  {start, position, points},
  start = First@Sort@curve;
  position = First@Flatten@Position[curve, start];
  points = curve[[Mod[{# - 1, #, # + 1} &@position, Length@curve, 1]]];
  If[Det[PadLeft[points, {3, 3}, 1]] > 0, True, False]
]

(*Create mask*)
Clear["CreateMask"]
Options[CreateMask]:={ReturnType->"Image"};
CreateMask::invret="Invalid value for option ReturnType";
CreateMask[height_Integer,width_Integer,contour:{{_?NumericQ,_?NumericQ}..},opts:OptionsPattern[]]:=Module[
	{returnType,matrix},
	returnType=OptionValue[ReturnType];
	If[!MemberQ[{"Image","Matrix"},returnType],
		Message[CreateMask::invret];
		Return[$Failed]
	];
	matrix=Partition[Global`rawCreateMask[height,width,Flatten@contour],height];
	If[returnType==="Image",
		Image[matrix],
		matrix
	]
]

(*Finite differences*)
Clear[FiniteDifferences];
FiniteDifferences[contours:{{{_Real,_Real}..}..},n_Integer:1]:=FiniteDifferences[#,n]&/@contours
FiniteDifferences[contour:{{_Real,_Real}..},n_Integer:1]:=Module[
{kernel},
	kernel=Table[(-1)^i Binomial[n,i],{i,0,n}];
	Transpose[ListCorrelate[kernel,#,{Length@kernel-1}]&/@Transpose[contour]]
]
	
(*Align contours*)
Clear[AlignContours];
Options[AlignContours]={Iterations->100,MinimalError->0.0001,WithScalingInvariance->True};
AlignContours::nocc="The following polygons (index) `1` aren't orientated counterclockwise or are self intersecting. Alignment fails if they're not oriented counter clockwise!";
AlignContours::noposint="Option '`1`' has to be a positive integer";
AlignContours::noreal="Option '`1`' has to be a real number";
AlignContours::itlimit="Iteration limit of '`1`' reached";
AlignContours::nobool="Option '`1`' has to be either 'True' or 'False'";
AlignContours[contours:{Repeated[{{_Real,_Real}..},{2,Infinity}]},opts:OptionsPattern[]]:=Module[
{iterationLimitReached=True,size,ccs,iterations,error,centeredContours,normalizedCenteredContours,meanEstimate,iterate,terminationCondition,alignedContours,updatedMean},

	(*validating options*)
	{iterations,error,size}=OptionValue[{Iterations,MinimalError,WithScalingInvariance}];
	If[!IntegerQ[iterations]&&iterations>0,
		Message[AlignContours::noposint,"Iterations"];
		Return[$Failed]
	];
	If[!NumericQ[error],
		Message[AlignContours::noreal,"MinimalError"];
		Return[$Failed]
	];
	ccs=CounterClockwise/@contours;
	If[!(And@@(ccs)),
		Message[AlignContours::nocc, Flatten@Position[ccs, False]]
	];
	If[!(size===True||size===False),
		Message[AlignContours::nobool,WithScalingInvariance];
		Return[$Failed]
	];

	(*start implementation*)
	centeredContours=#-Mean[#]&/@ComplexRepresentation[contours];
	normalizedCenteredContours=If[size,Normalize[N[#]]&/@centeredContours,centeredContours];
	meanEstimate=First@normalizedCenteredContours;
	iterate[reference_,samples_]:=
	(
		alignedContours=Map[AlignContour[reference,#,WithScalingInvariance->size]&,samples,{1}];
		(*updatedMean=First@Last@Eigensystem[Total[Transpose[{#}].Conjugate[{#}]&/@alignedContours,{1}],1];*)
		updatedMean=Mean/@Transpose[ListRepresentation/@alignedContours];
		{ComplexRepresentation@updatedMean,alignedContours}
	);
	terminationCondition[last_,current_]:=
	If[(1-Norm[First@current.Conjugate[First@last]]^2)<error,(*procrustes distance*)
		iterationLimitReached=False;False,
		True
	];
	{meanEstimate,alignedContours}=
	NestWhile[
		iterate[First@#,Last@#]&,
		{meanEstimate,normalizedCenteredContours},
		terminationCondition,
		2,
		iterations
	];
	alignedContours=Map[AlignContour[meanEstimate,#,WithScalingInvariance->size]&,alignedContours,{1}];
	meanEstimate=updatedMean;	
	If[iterationLimitReached,Message[AlignContours::itlimit,iterations]];
	{PointwiseRepresentation[meanEstimate],PointwiseRepresentation[alignedContours]}
]

(*Align contour*)
Clear[AlignContour];
Options[AlignContour]={WithScalingInvariance->True};
AlignContour::nocc="Supplied polygons aren't orientated counterclockwise";
AlignContour::nobool="Option '`1`' has to be either 'True' or 'False'";
AlignContour[reference:{{_Real,_Real}..},sample:{{_Real,_Real}..},opts:OptionsPattern[]]:=AlignContour[ComplexRepresentation[reference],ComplexRepresentation[sample],opts];
AlignContour[reference:{__?NumericQ},sample:{__?NumericQ},opts:OptionsPattern[]]:=Module[
{size,cyclicPermutation,alpha,procrustesDistance,bestFit},
	
	size=OptionValue[WithScalingInvariance];
	
	(*option validation*)
	If[!(CounterClockwise[reference]&&CounterClockwise[sample]),
		Message[AlignContour::nocc];
		Return[$Failed]
	];
	If[!(size===True||size===False),
		Message[AlignContour::nobool,WithScalingInvariance];
		Return[$Failed]
	];
	
	(*implementation*)
	cyclicPermutation=Table[RotateRight[sample,i],{i,0,Length@sample-1}];
	alpha=Conjugate[#].reference &/@ cyclicPermutation;
	procrustesDistance=(1-Norm[Conjugate[reference].#]^2)&/@(alpha*cyclicPermutation);
	bestFit=Sequence@@First@Position[procrustesDistance,Min[procrustesDistance]];
	If[size,
		Normalize[alpha[[bestFit]]*cyclicPermutation[[bestFit]]],
		ComplexRepresentation[ListRepresentation[Normalize[alpha[[bestFit]]*cyclicPermutation[[bestFit]]]]*Norm[ListRepresentation[sample]]]
	]
]

(*Align control points*)
Clear[AlignControlPoints];
AlignControlPoints::nocc="Supplied polygons aren't orientated counterclockwise";
AlignControlPoints[reference:{{_Real,_Real}..},sample:{{_Real,_Real}..}]:=AlignControlPoints[ComplexRepresentation[reference],ComplexRepresentation[sample]];
AlignControlPoints[reference:{__Complex},sample:{__Complex}]:=Module[
{centeredNormalized,cyclicPermutation,alpha,procrustesDistance,bestFit},

	If[!(CounterClockwise[reference]&&CounterClockwise[sample]),
		Message[AlignContour::nocc];
		Return[$Failed]
	];
	centeredNormalized=ComplexRepresentation@First@CenterAndNormalize[sample];
	cyclicPermutation=Table[RotateRight[centeredNormalized,i],{i,0,Length@centeredNormalized-1}];
	alpha=Conjugate[#].reference &/@ cyclicPermutation;
	procrustesDistance=(1-Norm[Conjugate[reference].#]^2)&/@(alpha*cyclicPermutation);
	bestFit=Sequence@@First@Position[procrustesDistance,Min[procrustesDistance]];
	bestFit-1
]

Clear[eCreateContour];
eCreateContour::noint = noint;
Options[eCreateContour] = {Nodes->50}
eCreateContour[center:{_?NumericQ,_?NumericQ},radius_?NumericQ,opts:OptionsPattern[]]:=Module[
	{nodes},
	nodes = OptionValue[Nodes];
	IntegerCheck[eCreateContour,"Nodes",nodes];
	N@Table[center + {0,radius}.RotationMatrix[alpha],{alpha,2Pi-2Pi/nodes,0,-2Pi/nodes}]
]

(*Center and normalize*)
Clear[CenterAndNormalize];
CenterAndNormalize[contours:{{_Real,_Real}..}]:=CenterAndNormalize[{ComplexRepresentation@contours}];
CenterAndNormalize[contours:{__Complex}]:=CenterAndNormalize[{contours}];
CenterAndNormalize[contours:{{{_Real,_Real}..}..}]:=CenterAndNormalize[ComplexRepresentation[contours]];
CenterAndNormalize[contours:{{__Complex}..}]:=Module[
{},
	PointwiseRepresentation[Normalize[#-Mean[#]]&/@contours]
]

(*Complex representation*)
Clear[ComplexRepresentation];
ComplexRepresentation[contour:{{_Real,_Real}..}]:=Map[Complex[Sequence@@N[#]]&,contour,{1}];(*pointwise to complex - single contour*)
ComplexRepresentation[contour:{{{_Real,_Real}..}..}]:=Map[Complex[Sequence@@N[#]]&,contour,{2}];(*pointwise to complex - multiple contours*)
ComplexRepresentation[contour:{__Complex}]:=contour;(*returns same contour*)
ComplexRepresentation[contours:{{__Complex}..}]:=contours;(*returns same contours*)
ComplexRepresentation[contour:{__Real}]:=N[Complex[Sequence@@#]&/@Table[Extract[contour,{{i},{i+1}}],{i,1,Length@contour-1,2}]];(*list to complex - single contour*)
ComplexRepresentation[contours:{{__Real}..}]:=(*list to complex - multiple contours*)
N[Map[Complex[Sequence@@#]&,(Table[Extract[#,{{i},{i+1}}],{i,1,Length@#-1,2}]&/@contours),{2}]];

(*Pointwise representation*)
Clear[PointwiseRepresentation];
PointwiseRepresentation[contour:{{_Real,_Real}..}]:=contour;(*returns same contour*)
PointwiseRepresentation[contours:{{{_Real,_Real}..}..}]:=contours;(*returns same contours*)
PointwiseRepresentation[contour:{__Real}]:=Table[Extract[contour,{{i},{i+1}}],{i,1,Length@contour-1,2}];(*list to pointwise - single contour*)
PointwiseRepresentation[contours:{{__Real}..}]:=Table[Extract[#,{{i},{i+1}}],{i,1,Length@#-1,2}]&/@contours;(*list to pointwise - multiple contours*)
PointwiseRepresentation[contour:{__?NumericQ}]:=Map[N@{Re[#],Im[#]}&,contour,{1}];(*complex to pointwise - single contour*)
PointwiseRepresentation[contour:{{__?NumericQ}..}]:=Map[N@{Re[#],Im[#]}&,contour,{2}];(*complex to pointwise - multiple contours*)

(*List representation*)
Clear[ListRepresentation];
ListRepresentation[contour:{__Real}]:=contour;(*returns same contour*)
ListRepresentation[contours:{{__Real}..}]:=contours;(*returns same contours*)
ListRepresentation[contour:{__Complex}]:=Map[Sequence@@{Re[#],Im[#]}&,contour];(*converts single contour from complex to list representation*)
ListRepresentation[contours:{{__Complex}..}]:=Map[Sequence@@{Re[#],Im[#]}&,contours,{2}];(*converts multiple contours from complex to list representation*)
ListRepresentation[contour:{{_Real,_Real}..}]:=Flatten[contour];(*converts single contour from pointwise to list representation*)
ListRepresentation[contours:{{{_Real,_Real}..}..}]:=Flatten/@contours;(*converts multiple contour from pointwise to list representation*)

(*Shape energy*)
Clear[ShapeEnergy];
Options[ShapeEnergy]={Nodes->100,WithScalingInvariance->True};
ShapeEnergy::noint="Option '`1`' has to be a positive, non-zero integer";
ShapeEnergy::nobool="Option '`1`' has to be either 'True' or 'False'";
ShapeEnergy[samples:{{{_Real,_Real}..}..},opts:OptionsPattern[]]:=Module[
{nodes,size,contours,mean,alignedContours,covariance,rank,eigenvalues,eigenvectors,maxEig,smallestNonZero,inverseRegularizedCovariance,dimension,row,t,mj,m1,rotationMatrix,centeringTerm},
	{nodes,size} = OptionValue[{Nodes,WithScalingInvariance}]; 	
	
	(*option validation*)
	If[!IntegerQ[nodes]||!nodes>0,
		Message[ShapeEnergy::noint,Nodes];
		Return[$Failed]
	];
	If[!(size===True||size===False),
		Message[ShapeEnergy::nobool,WithScalingInvariance];
		Return[$Failed]
	];
	
	(*implementation*)
	contours=Resample[samples,Nodes->nodes];
	{mean,alignedContours}=CheckAbort[AlignContours[contours, WithScalingInvariance->size],Abort[]];
	mean=ListRepresentation[mean];
	covariance=1/(Length@contours-1) Total[Outer[Times,#,#]&/@(#-mean&/@ListRepresentation[alignedContours])];
	rank = MatrixRank[covariance];
	{eigenvalues,eigenvectors}=Take[#,rank]&/@Eigensystem[covariance];
	(*
	maxEig=Length[Select[#/Total@eigenvalues&/@eigenvalues,#>0.01 &]];
	{eigenvalues,eigenvectors}=Take[#,maxEig]&/@{eigenvalues,eigenvectors};
	*)
	smallestNonZero=Last@eigenvalues;
	inverseRegularizedCovariance=Inverse[ Transpose[eigenvectors].DiagonalMatrix[eigenvalues].eigenvectors + Last@eigenvalues/2*(IdentityMatrix[Length@mean] - Transpose[eigenvectors].eigenvectors)];
	dimension=Length@mean/2;
	row=Table[Mod[i,2],{i,2dimension}];
	t=Table[RotateLeft[row,Mod[i+1,2]],{i,2dimension}];
	(*m[i_,j_,k_,meanContour_]:=\[Piecewise]{
		{meanContour[[j]], i==k},
	 	{meanContour[[j+1]], i==k+1&&EvenQ[i]&&OddQ[j]},
	 	{-meanContour[[j-1]], i==k+1&&EvenQ[i]&&EvenQ[j]},
	 	{-meanContour[[j+1]], i==k-1&&OddQ[i]&&OddQ[j]},
		{meanContour[[j-1]], i==k-1&&OddQ[i]&&EvenQ[j]},
	 	{0, True}
	};
	m1=SparseArray[Flatten[Table[{i,j,k}->m[i,j,k,mean],{i,2dimension},{j,2dimension},{k,2dimension}]]];*)
	mj[j_, meanContour_] := Table[{{k, k} -> meanContour[[j]], {k + 1, k}->If[EvenQ[k + 1],If[EvenQ[j], -meanContour[[j - 1]], meanContour[[j + 1]]],0],
	 {k - 1, k}->If[OddQ[k - 1],If[EvenQ[j], meanContour[[j - 1]], -meanContour[[j + 1]]],0]}, {k, 1, 2 dimension}];
	(*DistributeDefinitions[m,mean,dimension];*)
	m1=Table[SparseArray[DeleteCases[Flatten[mj[j, mean]], _ -> 0]], {j,2 dimension}];
	
	rotationMatrix=KroneckerProduct[IdentityMatrix[dimension],RotationMatrix[Pi/2]];
	centeringTerm=IdentityMatrix[2dimension]-1/dimension t;
	ShapeEnergyFunction[#,inverseRegularizedCovariance,mean,dimension,rotationMatrix,m1,centeringTerm,WithScalingInvariance->size]&
]

(*Reparameterize*) (*dirty implementation: curve points are used as new control points*)
Clear[Reparameterize];
Reparameterize[contour : {{_Real, _Real} ..}] := Module[
  {g, length, params},
  g = BSplineFunction[contour, SplineDegree -> 2, 
    SplineClosed -> True, SplineKnots -> "Unclamped"];
  length = Quiet[NIntegrate[Norm[g'[s]], {s, 0, 1}]];
   
  params = 
   Map[
    Quiet[x /. 
       FindRoot[NIntegrate[Norm[g'[s]], {s, 0, x},AccuracyGoal -> 6,PrecisionGoal->6] - #, {x, .5},AccuracyGoal->4,PrecisionGoal->4]] &, 
    Table[i, {i, length/Length@contour, length, length/Length@contour}]];
  Table[g[s], {s, params}]
 ]

(*Resample*)
Clear[Resample];
Options[Resample]={Nodes->100};
Resample::noint="Option '`1`' has to be a positive, non-zero integer";
Resample[contours:{{{_Real,_Real}..}..},opts:OptionsPattern[]]:=Block[
	{nodes},
	nodes = OptionValue[Nodes]; 
	(*option validation*)
	If[!IntegerQ[nodes]||!nodes>0,
		Message[Resample::noint,Nodes];
		Return[$Failed]
	];
	
	(*implementation*)
	If[CounterClockwise[#],#,Reverse@#]&/@(Table[#[s],{s,0,1-1/nodes,1/nodes}]&/@(BSplineFunction[#,SplineClosed->True,SplineDegree->2,SplineKnots->"Unclamped"]&/@contours))
]
Resample[contour:{{_Real,_Real}..},opts:OptionsPattern[]]:=First@Resample[{contour},opts]

(*Shape energy function*)
Clear[ShapeEnergyFunction];
Options[ShapeEnergyFunction]={WithScalingInvariance->True};
ShapeEnergyFunction::wdim="Wrong dimension, contour has to have `1` nodes, current number of nodes is `2`";
ShapeEnergyFunction::nobool="Option '`1`' has to be either 'True' or 'False'";
ShapeEnergyFunction[contour:{{_Real,_Real}..},inverseRegularizedCovariance_?MatrixQ,meanShape:{__Real},dimension_Integer,rotationMatrix_?MatrixQ,m1:{__SparseArray},centeringTerm_?MatrixQ,opts:OptionsPattern[]]:=Module[
{size,shift,alignedContour,mean,centeredContour,tmp,tmp2,tmp3,tmp4,tmp5,centeredAligned,m,energy},

	size=OptionValue[WithScalingInvariance];
	
	(*option validation*)
	If[Length@contour!= dimension,
		Message[ShapeEnergyFunction::wdim,dimension,Length@contour];
		Abort[]
	];
	If[!(size===True||size===False),
		Message[ShapeEnergyFunction::nobool,WithScalingInvariance];
		Return[$Failed]
	];
	
	(*implementation*)
	shift=AlignControlPoints[ComplexRepresentation@meanShape,ComplexRepresentation@contour];
	mean=Mean@ComplexRepresentation@contour;
	centeredContour=ListRepresentation[#-mean&/@RotateRight[ComplexRepresentation@contour,shift]];
	alignedContour=
	If[size,
		ListRepresentation@AlignContour[ComplexRepresentation@meanShape,ComplexRepresentation@First@CenterAndNormalize@contour,WithScalingInvariance->size],
		ListRepresentation@AlignContour[ComplexRepresentation@meanShape,ComplexRepresentation@First@CenterAndNormalize@contour,WithScalingInvariance->size]*Norm[ListRepresentation[centeredContour]]
	];
	tmp=meanShape.centeredContour;
	tmp2=meanShape.rotationMatrix.centeredContour;
	m=KroneckerProduct[IdentityMatrix[dimension],{{tmp,-tmp2},{tmp2,tmp}}];
	tmp3=m1.centeredContour+m;
	tmp4=m.centeredContour;
	tmp5=Norm[tmp4];
	centeredAligned=
	If[size,
		tmp3/tmp5-(Outer[Times,tmp4,tmp4].tmp3)/tmp5^3,
		(tmp3/tmp5 - (Outer[Times, tmp4, tmp4].tmp3)/tmp5^3) Norm[centeredContour] + tmp4.(centeredContour/Norm[centeredContour])
	];
	energy=-(inverseRegularizedCovariance.(alignedContour-meanShape)).centeredAligned.centeringTerm;
	PointwiseRepresentation[RotateLeft[ComplexRepresentation@energy,shift]]
]
End[];
EndPackage[];
