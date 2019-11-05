(* Mathematica Package *)

BeginPackage["Eidomatica`Tracking`"]
Needs["GraphUtilities`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Internal`"]
Needs["Eidomatica`Utilities`"]
Needs["Eidomatica`TrackingUtilities`"]

(* Exported symbols added here with SymbolName::usage *)  

eDistanceTracking::usage="eDistanceTracking[] computes tracks by shortest distance tracking.";
eFluidTracking::usage="eFluidTracking[binaryImagePath, flowFieldPath, exportPath] compute fluid tracks over all images."
	InitialLabelMatrix::usage = "InitialLabelMatrix uses the given intial label matrix instead of the first binary image."
	Overlap::usage = "Overlap the overlap constraint (percentage of overlap)."
	IncludeAppearingCells::usage = "IncludeAppearingCells if True than appearing objects are included in the tracking process."
	FileNameDivision::usage="FileNameDivision the file name for the file where the divisions are saved in."
eLikelihoodTracking::usage = "eLikelihoodTracking[features, mu, sigma] tracks objects with respect to their distances in features, with prior distribution of distances given by mu and sigma."
labelPropagate::usage="labelPropagate do not use this function!!"
eSelectTracks::usage="eSelectTracks[tracks] selects and cut tracks according to the given options. Input have the form of the tracks returned by eExtractTrackingData."
	From::usage="From option for eSelectTracks, if set to an specific number only tracks from this timepoint are returned."
	To::usage="To option for eSelectTracks, if set to an specific number only tracks until this timepoint are returned."
	MinimalLength::usage="MinimalLength option for eSelectTracks, if set to an specific number only tracks longer than this number are returned."
	MaximalDisplacement::usage="MaximalDisplacement option for eSelectTracks, if set to an specific number only tracks with jumbs between timepoints smaller than this number are returned."
	ClipAtStartEnd::usage="ClipAtStartEnd clips tracks at start and end according to the specified From and To options."
	ReturnPointsOnly::usage="ReturnPointsOnly retruns only the track coordinates."

Begin["`Private`"] (* Begin Private Context *)

(*
(*Distance based tracking*)
Clear[eDistanceTracking];
eDistanceTracking::nobool = nobool;
Options[eDistanceTracking]={AbortWhenOverride->False};
eDistanceTracking::override = override;
eDistanceTracking[project_TrackingProject,opts:OptionsPattern[]]:=Module[
	{metaFiles,objectives,tracks,largestID,fileName,newProject},
	(*option validation*)
	IsBoolean[eDistanceTracking,"AskWhenOverride",OptionValue[AbortWhenOverride]];
	
	(*implementation*)
	metaFiles = eGetFileNames[project, "MetaFiles"];
	objectives = {"ObjectID", "ObjectCenter"};
	tracks = ParallelTable[
  		distanceBasedMapping[objectives /. Import[metaFiles[[i]]], objectives /. Import[metaFiles[[i + 1]]]]
  		,{i,Length@metaFiles - 1}, DistributedContexts -> Automatic
  	];
	tracks=WeakComponents[Flatten@tracks];
	
	(*add single tracks*)
	largestID=Max["ObjectID"/.Import[Last@metaFiles]];
	tracks=Sort[Join[tracks,List/@Complement[Table[i, {i, largestID}], Flatten[tracks]]], Length@#1 > Length@#2 &];
	
	fileName = FileNameJoin[{("ProjectPath" /. Level[project, 1]),"tracks.m"}];
	If[FileExistsQ[fileName] && OptionValue[AbortWhenOverride],
		Message[eDistanceTracking::override];
		Abort[]
	];
	Export[fileName,tracks];
	eLogString["eDistanceTracking : Track file saved at " <> fileName];
	newProject=eUpdateProject[project,"TrackFile"];
	eUpdateMetaData[newProject];
	eLogString["eDistanceTracking : Tracking [successful]."];
	newProject
]*)

Clear[eDistanceTracking]
eDistanceTracking[meta : {({Rule[_Integer, {__?NumericQ}] ..} | {}) ..}] := Block[
  {tracks,components},
  tracks = MapThread[distanceBasedMapping[#1, #2] &, {Most@meta, Rest@meta}];
	tracks=MapIndexed[{First@Flatten@#2,First@#1}->{First@Flatten@#2+1,Last@#1}&,tracks,{2}];
  components=WeakComponents[Flatten@tracks]
]

Clear[distanceBasedMapping];
distanceBasedMapping[meta1:({Rule[_Integer, {__?NumericQ}] ..} | {}), meta2:({Rule[_Integer, {__?NumericQ}] ..} | {})] := Module[
	{meta1IDs, meta1Centers, meta2IDs, meta2Centers, distancesFrom, distancesTo, minimalDistancesFrom, minimalDistancesTo, from, to},
	If[Or[SameQ[meta1,{}],SameQ[meta2,{}]], Return[{}]];
	{meta1IDs, meta1Centers} = {First /@ meta1, Last /@ meta1};
	{meta2IDs, meta2Centers} = {First /@ meta2, Last /@ meta2};
	distancesFrom = Outer[Norm[#1 - #2] &, meta1Centers, meta2Centers, 1];
	distancesTo = Outer[Norm[#1 - #2] &, meta2Centers, meta1Centers, 1];
	minimalDistancesFrom = Flatten[Ordering[#, 1] & /@ distancesFrom];
	minimalDistancesTo = Flatten[Ordering[#, 1] & /@ distancesTo];
	MapIndexed[(
		from = First[#2];
		to = #1;
		If[minimalDistancesTo[[to]] == from,
			meta1IDs[[from]] -> meta2IDs[[to]],
			Nothing[]
		]) &,
		minimalDistancesFrom]
]

Clear[labelPropagate]
Options[labelPropagate]={LabelSize->"32Bit",Overlap->.5,IncludeAppearingCells->True,SizeConstraint->None,ConstraintType->"Standard"};
labelPropagate::nonum = nonum;
labelPropagate::nobool = nobool;
labelPropagate::wdim = "Label matrices have different dimensions, the dimensionality has to be equal.";
labelPropagate[displacedLabelMatrix:{{__Integer}..},segmentedLabelMatrix:{{__Integer}..},opts:OptionsPattern[]]:=Module[
	{overlap,labelSize,includeAppearing,pack,unpack,associations,forward,backward,union,intersection,divisions,mergers,divisionMergers,cellIDoffsets,unique,displacedMasks,segmentedMasks,mask,cloneID,offset,tmp,newCellIDOffsets},

	{overlap,labelSize,includeAppearing}=OptionValue[{Overlap,LabelSize,IncludeAppearingCells}];
	
	(*option validation*)
	NumberCheck[labelPropagate,Overlap,overlap];
	MemberCheck[labelPropagate,LabelSize,labelSize];
	IsBoolean[labelPropagate,IncludeAppearingCells,includeAppearing];
	If[!SameQ[Dimensions@displacedLabelMatrix,Dimensions@segmentedLabelMatrix],
		Message[labelPropagate::wdim];
		Abort[]
	]; 
	
	(*implementation*)
	If[labelSize=="32Bit",
		pack=ePackLabel32;
		unpack=eUnpackLabel32,
		pack=ePackLabel64;
		unpack=eUnpackLabel64
	];
	associations=Tally[Transpose@{Flatten@displacedLabelMatrix,Flatten@segmentedLabelMatrix}];
	forward=associate[associations,overlap,"forward"];
	backward=associate[associations,overlap,"backward"];
	union=DeleteCases[Union[forward,backward],Rule[x_,y_]/;(x==0||y==0)];
	intersection=DeleteCases[Intersection[forward,backward],Rule[x_,y_]/;(x==0||y==0)];
	divisions=Cases[GatherBy[intersection,First],x_/;Length@x>1];
	mergers=Flatten@Cases[GatherBy[union,Last],x_/;Length@x>1];
	divisionMergers=Intersection[Flatten@divisions,Flatten@mergers];(*cell divides an instantly merges with other cell*)
	mergers=Complement[mergers,divisionMergers];(*remove division-mergers from mergers*)
	unique=Complement[intersection,Flatten@divisions,mergers];
	
	Print[++$counter];
	cellIDoffsets=Dispatch@Map[First@First@#->Max[Last/@#]&,GatherBy[Map[unpack,$assignedIDs],First]];
	displacedMasks=getMasks[displacedLabelMatrix];(*components with the same label, not necessarily connected*)
	segmentedMasks=ComponentMeasurements[segmentedLabelMatrix,"Mask"];(*a mask for each connected component*)
	mask=SparseArray[ConstantArray[0,Dimensions@displacedLabelMatrix]];
	mask=Fold[#1+(First@#2*(Last@#2/.segmentedMasks))&,mask,unique];(*take masks from segmentation for unique mappings*)
	If[divisions!={},
		tmp={};
		mask+=Total@
			Flatten[
				MapIndexed[(
					cloneID=First@unpack@First@#1;
					newCellIDOffsets=Dispatch@Join[Map[First@First@# -> Max[Last /@ #] &, GatherBy[Map[unpack, Last/@Flatten[tmp]],First]],{cloneID->-1}];
					offset=Max[cloneID/.cellIDoffsets,cloneID/.newCellIDOffsets];
					AppendTo[tmp,First@#1->(pack[cloneID,offset+1])];
					If[!MemberQ[divisionMergers,#1],
						(pack[cloneID,offset+1])*(Last@#1/.segmentedMasks),(*normal division*)
						(pack[cloneID,offset+1])*(First@#1/.displacedMasks)*(Last@#1/.segmentedMasks)(*division-merger*)
					])&,
				divisions,{2}],(*take masks from segmentation for divisions*)
				{2,1}
			];
		AppendTo[$divisionsList,tmp]
	];
	mask=Fold[
		(tmp=ComponentMeasurements[MorphologicalComponents[((First@#2/.displacedMasks)*(Last@#2/.segmentedMasks))],{"Count","Mask"}];
		#1+(First@#2)*Last@Last@First@Cases[tmp, x_ /; First@Last@x == Max[First /@ Last /@ tmp]])&
		,mask,mergers];(*take intersection between displacement and segmentation for mergers*)
	If[includeAppearing,
		mask+=Total@Map[(
			++$largestCloneID;
			pack[$largestCloneID,1]*(Last@#1/.segmentedMasks))&,
			appearing[associations]
		]
	];
	mask
]

(*Fluid based tracking*)
Clear[eFluidTracking];
Options[eFluidTracking]:=Union@Flatten[{Options[labelPropagate], Options[eRelabel], Options[eDisplaceByVectorField], Options[eDeleteObjects], {AbortWhenOverride -> True, InitialLabelMatrix -> None,FileNameDivision->"./divisions.m"}}];
eFluidTracking::nobool = nobool;
eFluidTracking::nomemb = nomemb;
eFluidTracking::invinit = "Invalid option value for InitialLabelMatrix. The option has te be either 'None' or a matrix of integers.";
eFluidTracking::invdir = "Given export directory `1` does not exist or is not empty.";
eFluidTracking::override = "There are files in the given directory `1` and override option is not turned on."
eFluidTracking::wfc = "Wrong file count. If image path contains n images there have to be n-1 flows."
eFluidTracking::twf="No vector field files found. Tracking without displacement!"
eFluidTracking[binPath_String,flowPath_String,path_String,opts:OptionsPattern[]]:=Module[
	{imageNames,flowNames},
	imageNames = FileNames["Images" /. Eidomatica`TrackingUtilities`Private`$searchPattern, {binPath}, IgnoreCase -> True];
	If[flowPath==="",
		flowNames={},
		flowNames = FileNames["Flows" /. Eidomatica`TrackingUtilities`Private`$searchPattern, {flowPath}, IgnoreCase -> True]
	];
	eFluidTracking[imageNames,flowNames,path,opts];
]
eFluidTracking[imageNames:{___String},flowNames:{___String},path_String,opts:OptionsPattern[]]:=Module[
	{index=1,opts1,opts2,opts3,opts4,opts5,override,initialLabelMatrix,labelSize,divisions,digits,initial,pack,unpack,iterate, withOutFlows=False},
	
	opts1=Sequence@@FilterRules[{opts},First/@Options[labelPropagate]];
	opts2=Sequence@@Join[DeleteCases[FilterRules[{opts},First/@Options[eDisplaceByVectorField]],ReturnType->_],{ReturnType->"Matrix"}];
	opts3=Sequence@@FilterRules[{opts},First/@Options[eRelabel]];
	opts4=Sequence@@FilterRules[{opts},First/@Options[eDeleteObjects]];
	opts5=Sequence@@Join[DeleteCases[{opts4},SizeConstraint->_],{SizeConstraint->(
			Switch[OptionValue[SizeConstraint],
				None|{None,None},
				None,
				{_Integer,None},
				{IntegerPart@N[First[OptionValue[SizeConstraint]]/3],None},
				{None,_Integer},
				{None,IntegerPart@N[Last[OptionValue[SizeConstraint]]/3]},
				{_Integer,_Integer},
				IntegerPart@N[OptionValue[SizeConstraint]/3]
			]
		)}];
	
	eLogString["labelPropagate: " <> ToString[DeleteDuplicates[Join[{opts1},Options[labelPropagate]], First@#1 === First@#2 &]]];
	eLogString["eDisplaceByVectorField: " <> ToString[DeleteDuplicates[Join[{opts2},Options[eDisplaceByVectorField]], First@#1 === First@#2 &]]];
	eLogString["eRelabel: " <> ToString[DeleteDuplicates[Join[{opts3},Options[eRelabel]], First@#1 === First@#2 &]]];
	eLogString["eDeleteObjects: " <> ToString[DeleteDuplicates[Join[{opts4},Options[eDeleteObjects]], First@#1 === First@#2 &]]];
	eLogString["eDeleteObjects for displaced masks: " <> ToString[DeleteDuplicates[Join[{opts5},Options[eDeleteObjects]], First@#1 === First@#2 &]]];
	eLogString["Found " <> ToString[Length@imageNames] <> " images."];
	eLogString["Found " <> ToString[Length@flowNames] <> " flows."];
	
	{override, initialLabelMatrix, labelSize,divisions} = OptionValue[{AbortWhenOverride,InitialLabelMatrix,LabelSize,FileNameDivision}];
	
	(*option validation and parameter check*)
	IsBoolean[eFluidTracking,AbortWhenOverride,override];
	If[!(MatrixQ[initialLabelMatrix,IntegerQ] || SameQ[initialLabelMatrix,None]),
		Message[eFluidTracking::invinit];
		Abort[]
	];
	MemberCheck[eFluidTracking,LabelSize,labelSize,{"32Bit","64Bit"}];
	If[!DirectoryQ[path],
		Message[eFluidTracking::invdir,path];
		Abort[]
	];
	If[(!override &&Length@FileNames["*.mat",{path}]>0&&FileExistsQ[divisions]),
		Message[eFluidTracking::override,path];
		Abort[]
	];
	If[Length@flowNames == 0,
		withOutFlows=True;
		Message[eFluidTracking::twf],
		If[(Length@imageNames-1) != Length@flowNames,
			Message[eFluidTracking::wfc];
			Abort[]
		];
	];
	
	(*implementation*)
	digits=Length[IntegerDigits[Length[imageNames]+1]];
	If[initialLabelMatrix===None,
		initial=eRelabel[MorphologicalComponents[Import[First@imageNames]],opts3],
		initial=eRelabel[initialLabelMatrix,opts3]
	];
	If[labelSize=="32Bit",
		pack=ePackLabel32;
		unpack=eUnpackLabel32,
		pack=ePackLabel64;
		unpack=eUnpackLabel64
	];
	
	$counter = 1;
	Print[$counter];
	$divisionsList = {};
	$largestCloneID=Max[First/@unpack/@Union@Normal@Flatten[initial]];
	$assignedIDs=DeleteCases[Union@Flatten[Normal[initial]],0];
	iterate[initialLabel_,{imageName_String,flowName_String}]:=Block[
		{label},
		label=labelPropagate[eDeleteObjects[eDisplaceByVectorField[initialLabel,eBinaryImport[flowName],opts2],opts5],eDeleteObjects[MorphologicalComponents[Import[imageName]],opts4],opts1];
		$assignedIDs=Union[$assignedIDs,DeleteCases[Union@Flatten[Normal[label]],0]];
		Export[eComposeFileName[FileNameJoin[{path,"label"}],".mat",index,digits],label];
		++index;
		label
	];
	iterate[initialLabel_,imageName_String]:=Block[
		{label},
		label=labelPropagate[eDeleteObjects[initialLabel,opts5],eDeleteObjects[MorphologicalComponents[Import[imageName]],opts4],opts1];
		$assignedIDs=Union[$assignedIDs,DeleteCases[Union@Flatten[Normal[label]],0]];
		Export[eComposeFileName[FileNameJoin[{path,"label"}],".mat",index,digits],label];
		++index;
		Normal@label
	];
	Export[eComposeFileName[FileNameJoin[{path,"label"}],".mat",index,digits],initial];
	++index;
	Fold[iterate[#1,#2]&,
		initial,If[withOutFlows,Drop[imageNames,1],Transpose@{Drop[imageNames,1],flowNames}]
	];
	Export[divisions,$divisionsList];
]

Clear[eLikelihoodTracking];
eLikelihoodTracking[features:{{Rule[_Integer, {__?NumericQ}] ..} ..},mu:{__?NumericQ}, sigma : {{__?NumericQ} ..}]:=Block[
	{normCoeff, invSigma, pMove, f, likelihoods, selectedLikelihoods, positions, hypotheses, matA, value, matB, v, optimalAssignment, selectedHypotheses},
    normCoeff = 1/Sqrt[(2 \[Pi])^Length[mu]*Det[sigma]];
    invSigma = Inverse[sigma];
    pMove[distance_] :=
        normCoeff*Exp[-(1/2) (distance - mu).invSigma.(distance - mu)];
    f[f1 : Rule[_Integer, {__?NumericQ}], 
      f2 : Rule[_Integer, {__?NumericQ}]] :=
        pMove[Last@f1 - Last@f2];
    likelihoods = 
     MapThread[
      SparseArray@Chop@Outer[f, #1, #2] &, {Most@features, 
       Rest@features}];
    (*selectedLikelihoods=#/.x_/;x<likelihoodThreshold\[Rule]0&/@
    likelihoodValues;*)(*selects likelihoods greater than <
    likelihoodThreshold>*)
    hypotheses = 
     MapIndexed[{First@#2, First@#1} -> {First@#2 + 1, 
         Last@#} &, (Position[Normal@#, x_ /; x > 0] & /@ 
        likelihoods), {2}];
    Table[
     If[ Length@hypotheses[[i]] != 0,
         matA = 
          Table[0, {Length@
             hypotheses[[i]]}, {Total@{Length@features[[i]], 
              Length@features[[i + 1]]}}];
         matA = MapIndexed[
           (value = likelihoods[[i, #1[[1, 2]], #1[[2, 2]]]]/2;
            ReplacePart[
             matA[[First[#2]]], {#1[[1, 2]] -> 
               value, (#1[[2, 2]] + Length@features[[i]]) -> value}]) &
           , hypotheses[[i]]
           ];
         matB = Map[Ceiling, matA, {2}];
         v = Table[1, {Length@features[[i]] + Length@features[[i + 1]]}];
         optimalAssignment = 
          LinearProgramming[-matA.v, -Transpose[matB], -v, 
           Table[{0, 1}, {Length[matA]}], Integers];
         selectedHypotheses = 
          Cases[hypotheses[[i]], 
           x_ /; optimalAssignment[[Position[hypotheses[[i]], x][[1, 
                 1]]]] == 1];
         selectedHypotheses,
         {}
     ], {i, Length@hypotheses}
     ]
]

Clear[getMasks];
getMasks[matrix:{{__Integer}..}]:=Last[First[#]]->SparseArray[#,Dimensions[matrix]]/Last[First[#]]&/@GatherBy[DeleteCases[Flatten[MapIndexed[#2->#1&,matrix,{2}]],_->0],Last]

Clear[associate]
associate[association:{{{_Integer,_Integer},_Integer}..},overlap_?NumericQ,direction_String:"backward"]:=Block[
	{f,same,g,total,rules},
	f[x_]:=Sort[x,Last@#1>= Last@#2&];
	Switch[direction,
		"backward",
		same=f/@GatherBy[association,Last@First@#&],
		"forward",
		same=f/@GatherBy[association,First@First@#&]
	];
	g[x_]:=(
		total=Total[Last/@x];
		Cases[x,y_/;(Last@y/total)>= overlap/Length[x]]
	);
	rules=Flatten[Rule@@@#&/@Map[First,DeleteCases[(g/@same),{}],{2}]];
	Switch[direction,
		"backward",
		DeleteCases[rules,0->_],
		"forward",
		DeleteCases[rules,_->0]
	]
]

Clear[appearing]
appearing[association:{{{_Integer,_Integer},_Integer}..}]:=Block[
	{appearingCells},
	appearingCells=Cases[association,{{0,y_},_}/;!MemberQ[Last[First@#]&/@DeleteCases[association,{{0,_},_}],y]];
	(First@#->Last@#)&/@(First/@appearingCells)
]       

Clear[eSelectTracks]
Options[eSelectTracks]={From->"Start",To->"End",MinimalLength->20,MaximalDisplacement->35,ClipAtStartEnd->False, ReturnPointsOnly->False};
eSelectTracks[tracks:{Rule[_Integer,{{__?NumericQ}..}]..},opts:OptionsPattern[]]:=eSelectTracks[Map[First@# -> Transpose[{Last@#, Table[i, {i, First@#, Length@Last@# + First@# - 1}]}] &, tracks],opts]
eSelectTracks[tracks:{Rule[_Integer,{{{__?NumericQ},_Integer}..}]..},opts:OptionsPattern[]]:=Block[
	{from,to,minLength,maxDisplacement,clip,tmp},
	{from,to,minLength,maxDisplacement,clip,points} = OptionValue[{From,To,MinimalLength,MaximalDisplacement,ClipAtStartEnd,ReturnPointsOnly}];
	tmp=tracks;
	If[!SameQ[to,"End"] && MatchQ[to,_Integer],
		tmp=DeleteCases[Map[First@#1->DeleteCases[Last@#,x_/;Last@x>to]&,tmp],x_/;Last@x=={}]
	];
	If[!SameQ[from,"Start"] && MatchQ[from,_Integer],
		tmp=DeleteCases[tmp,x_/;Last@Last@Last@x<from];
		If[clip,
			tmp=DeleteCases[Map[First@#1->DeleteCases[Last@#,x_/;Last@x<from]&,tmp],x_/;Last@x=={}]
			(*tmp=Map[First@#->(Last@#-ConstantArray[{{0,0},from-1},Length@Last@#])&,tmp]*)
		]
	];
	If[MatchQ[minLength,_Integer]&&minLength>0,
		tmp=DeleteCases[tmp,x_/;Length@Last@x<minLength]
	];
	If[MatchQ[maxDisplacement,_?NumericQ]&&maxDisplacement>0,
		tmp=Cases[tmp,x_/;And@@(#<maxDisplacement&/@(Norm/@Differences[x[[2,All,1]]]))]
	];
	If[points,
		Map[First/@Last@#&,tmp],
		tmp
	]
]

End[] (* End Private Context *)

EndPackage[]