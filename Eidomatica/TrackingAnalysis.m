(* Mathematica Package *)

BeginPackage["Eidomatica`TrackingAnalysis`"]
Needs["Eidomatica`Internal`"]
Needs["Eidomatica`Import`"]
Needs["Eidomatica`TrackingImport`"]
Needs["Eidomatica`Utilities`"]
Needs["Eidomatica`ZebrafishAnalysis`"]
Needs["Eidomatica`Transformation`"]

(* Exported symbols added here with SymbolName::usage *)  
eCellCycleTime::usage="eCellCycleTime[fileName] returns the cell cycle time of all the complete tracks in the given file. Complete tracks are tracks which have a mother aswell as daugther cells associated."
	FrameRate::usage="FrameRate is an option for eCellCycleTime. The frame rate is given in minutes and returned values are recalculated acordingly."
	MinimalCellCycleTime::usage="MinimalCellCycleTime is an option for eCellCycleTime. The time is given in minutes. Tracks with length shorter than MinimalCellCycleTime will not be returned."
	InHours::usage="InHours is an option for eCellCycleTime. If 'True' the cell cycle times are transformed to hours (remember to set the option FrameRate to the correct number in minutes)."
eSpatioTemporalSeparation::usage="SpatioTemporalSeparation[]"
	CoordinateOrigin::usage="CoordinateOrigin"
eAssignTracklets::usage="eAssignTracklets[]"
	Truncation::usage="Truncation is an option for several functions from Eidomatica`TrackingAnalysis`. This option enables rejection of flow vectors of a certain length. It can either be None (no vectors are rejected) or a list with a minimum and maximum length such as {.0001, Infintiy}. Vectors that do not satisfy the length constraint are rejected and will not be included in the computation of the circular statistics."
	SlidingWindow::usage="SlidingWindow is an option for eAssignTracklets."
  ReturnByPosition::usage="ReturnByPosition if 'True' the position of the tracklet is returned together with the directional vector, else the index of the spot is returned."
eComputeAverageField::usage="eComputeAverageField[]"
	ConvertToImageCS::usage="ConvertToImageCS"
	ReplaceMissing::usage="ReplaceMissing is an option for eComputeMeanField, when True missing values will be replaced by {0,0}."
eComputeDirectionalMoments::usage="eComputeDirectionalMoments[]"

Begin["`Private`"] (* Begin Private Context *)

Clear[eCellCycleTime]
Options[eCellCycleTime] = {FrameRate->1, MinimalCellCycleTime ->-1, InHours -> False};
eCellCycleTime[fileName_String, opts : OptionsPattern[]]:=Block[ 
	{frameRate, minimal, hours, trackNames, tracks},
  	(*options validation*)
    {frameRate,minimal,hours} = OptionValue[{FrameRate, MinimalCellCycleTime, InHours}];
    NumberCheck[eCellCycleTime, "FrameRate", frameRate];
    NumberCheck[eCellCycleTime, "MinimalCellCycleTime", minimal];
    IsBoolean[eCellCycleTime, "InHours", hours];
   
   	(*implementation*)
    If[ hours,
        minimal = minimal/60;
        hours = 60,
        hours = 1
    ];
    (*code*)
    trackNames = eImportHDF5[fileName, "Names", "/tracks", 1];
    tracks = eImportHDF5[fileName, "Annotations", trackNames];
    Cases[Flatten[
    	If[ ("hasMother" /. #)==1 && ("hasDaughters" /. #)==1,
           ("end" - "start") /. #,
           {}
       	] &/@tracks]*frameRate/hours,
      x_ /; x > minimal
    ]
]

Clear[eAssignTracklets]
Options[eAssignTracklets]=Evaluate@Join[{CoordinateOrigin -> "Cartesian", PositionalCorrection->None, SlidingWindow->False, ReturnByPosition->False},Options[ToSpherical]]
eAssignTracklets::nobool = nobool;
eAssignTracklets[tracks:{{{_?NumericQ, _?NumericQ, _Integer} ..} ..}, t_Integer, spatialWindow_Integer, temporalWindow_Integer, dimensions : {dimX_Integer, dimY_Integer}, opts:OptionsPattern[]] :=
    eAssignTracklets[tracks, t, {spatialWindow, spatialWindow}, temporalWindow, dimensions, opts]
eAssignTracklets[tracks:{{{_?NumericQ, _?NumericQ, _Integer} ..} ..}, t_Integer, spatialWindow : {spX_Integer, spY_Integer}, temporalWindow_Integer, dimensions : {dimX_Integer, dimY_Integer}, opts:OptionsPattern[]] :=
    Flatten[eAssignTracklets[#, t, spatialWindow, temporalWindow, dimensions,opts] & /@ tracks]
eAssignTracklets[track:{{_?NumericQ, _?NumericQ, _Integer} ..}, t_Integer, spatialWindow_Integer, temporalWindow_Integer, {dimX_Integer, dimY_Integer}, opts:OptionsPattern[]] :=
    eAssignTracklets[track, t, {spatialWindow, spatialWindow}, temporalWindow, {dimX, dimY}, opts]
eAssignTracklets[tracks:{{{_?NumericQ, _?NumericQ, _Integer} ..} ..}, t:{__Integer}, spatialWindow_Integer, temporalWindow_Integer, dimensions:{dimX_Integer, dimY_Integer}, opts:OptionsPattern[]]:=
    eAssignTracklets[tracks, #, spatialWindow, temporalWindow, dimensions, opts] &/@ t
eAssignTracklets[track:{{_?NumericQ, _?NumericQ, _Integer} ..}, t_Integer, {spX_Integer, spY_Integer}, temporalWindow_Integer, {dimX_Integer, dimY_Integer}, opts:OptionsPattern[]]:=Block[ 
	{origin, correction, sliding, positional, polarOpts, positions, uniquePositions, getWindow, window, tracklets, differences, rules, dispatch},
	(*option validation*)
	{origin, correction,sliding, positional}=OptionValue[{CoordinateOrigin, PositionalCorrection, SlidingWindow, ReturnByPosition}];
	polarOpts = Sequence@@FilterRules[{opts},First/@Options[ToSpherical]];
	IsBoolean[eAssignTracklets, "SlidingWindow", sliding];
  IsBoolean[eAssignTracklets, "ReturnByPosition", positional];
	MemberCheck[eAssignTracklets, "CoordinateOrigin", origin, {"Cartesian", "Mercator"}];
	If[!MatchQ[correction, (None | {_?NumericQ, _?NumericQ})],
		Message[eAssignTracklets::invo];
		Abort[]
	];
	(*implementation*)

    getWindow[x:{_?NumericQ, _?NumericQ, _Integer}]:=#[[1]] + #[[2]]*spX &@ IntegerPart[x/{dimX/spX, dimY/spY, temporalWindow}];
    window = Cases[track, {_, _, time_} /; t <= time <= t + temporalWindow];
    If[Length@window>0,
      differences = Drop[Differences@window, None, -1];
      differences=If[
        SameQ[origin,"Mercator"],
        eGreatCircleDistance[ToSpherical[Drop[window,None,-1],polarOpts]]*(Normalize/@differences),
        differences
      ];
      If[
        !SameQ[correction,None],
        correction=Join[correction,{0}];
        window=#+correction&/@window
      ];
      If[positional,
        MapThread[Most@#1 -> #2 &, {Drop[window, -1], differences}],
        rules = MapThread[getWindow[#1] -> #2 &, {Drop[window, -1], differences}];
        dispatch=Dispatch[rules];
        rules=Flatten[(#->ReplaceList[#, dispatch]) &/@ Union@Keys[rules]]
      ],
      {}
    ]
]

Clear[eSpatioTemporalSeparation]
eSpatioTemporalSeparation::nomemb=nomemb;
eSpatioTemporalSeparation::nonum=nonum;
Options[eSpatioTemporalSeparation]:=Join[Options[eAssignTracklets], Options[ToSpherical]];
eSpatioTemporalSeparation[track:{{_?NumericQ, _?NumericQ, _Integer}..}, t_, spatialWindow_Integer, temporalWindow_Integer, dimensions:{dimX_Integer, dimY_Integer}, opts:OptionsPattern[]]:=
	eSpatioTemporalSeparation[{track}, t, {spatialWindow, spatialWindow}, temporalWindow, dimensions, opts]
eSpatioTemporalSeparation[track:{{_?NumericQ, _?NumericQ, _Integer}..}, t_, spatialWindow:{spX_Integer, spY_Integer}, temporalWindow_Integer, dimensions:{dimX_Integer, dimY_Integer}, opts:OptionsPattern[]]:=
	eSpatioTemporalSeparation[{track}, t, spatialWindow, temporalWindow, dimensions, opts]
eSpatioTemporalSeparation[tracks:{{{_?NumericQ, _?NumericQ, _Integer} ..} ..}, t_, spatialWindow_Integer, temporalWindow_Integer, dimensions:{dimX_Integer, dimY_Integer}, opts:OptionsPattern[]]:=
	eSpatioTemporalSeparation[tracks, t, {spatialWindow, spatialWindow}, temporalWindow, dimensions, opts]
eSpatioTemporalSeparation[tracks:{{{_?NumericQ, _?NumericQ, _Integer} ..} ..}, t:{__Integer}, spatialWindow:{spX_Integer, spY_Integer}, temporalWindow_Integer, dimensions:{dimX_Integer, dimY_Integer}, opts:OptionsPattern[]]:=
	eSpatioTemporalSeparation[tracks, #, spatialWindow, temporalWindow, dimensions, opts] &/@ t
eSpatioTemporalSeparation[tracks:{{{_?NumericQ, _?NumericQ, _Integer} ..} ..}, t_Integer, spatialWindow : {spX_Integer, spY_Integer}, temporalWindow_Integer, 
  dimensions:{dimX_Integer, dimY_Integer}, opts:OptionsPattern[]]:=Block[
  	{origin, sliding, assignTrackletsOpts, polarOpts, rules, dispatch, components},
	(*option validation*)
	{origin, sliding} = OptionValue[{CoordinateOrigin, SlidingWindow}];
	assignTrackletsOpts = Sequence@@FilterRules[{opts},First/@Options[eAssignTracklets]];
	polarOpts = Sequence@@FilterRules[{opts},First/@Options[ToSpherical]];
	MemberCheck[eSpatioTemporalSeparation, "CoordinateOrigin", origin, {"Cartesian", "Mercator"}];
  	(*implementation*)
    rules = DeleteCases[eAssignTracklets[tracks, t, spatialWindow, temporalWindow, dimensions, assignTrackletsOpts],{}];
    dispatch=Dispatch@rules;
    rules=Flatten[(#->Join@@ReplaceList[#, dispatch]) &/@ Union@Keys[rules]];
    components = Join[rules, #->{} & /@ Complement[Table[i, {i, 0, Times@@spatialWindow-1}], Union@Keys[rules]]];
    components=Reverse/@Partition[Last /@ Sort[components, First@#1 > First@#2 &], spX]/.{}->Missing[]
]

Clear[eComputeAverageField]
Options[eComputeAverageField] = {Truncation->None, ConvertToImageCS -> False, ReplaceMissing->False, Method->"Mean"};
eComputeAverageField::nobool = nobool;
eComputeAverageField::invo=invo="The option Truncation must match either 'None' or {_?NumericQ,_?NumericQ}"
eComputeAverageField::nomemb = nomemb;
eComputeAverageField::info = "eComputeAverageField is used two times, when method is \"Median\", \"NonNormalizedMedian\" or \"MedianNorm\", first the Median is applied and afterwards the Mean!"
eComputeAverageField[matrix:{{({{_?NumericQ, _?NumericQ} ..} | _Missing) ..} ..}, opts:OptionsPattern[]]:=Block[
  {convert, replace, truncate, method, dimensions, result, f},
  {convert, replace,truncate,method} = OptionValue[{ConvertToImageCS, ReplaceMissing, Truncation,Method}];
  If[!MatchQ[truncate, (None | {_?NumericQ, _?NumericQ}| {-Infinity, _?NumericQ} | {_?NumericQ, Infinity})],
    Message[eComputeAverageField::invo];
    Abort[]
  ];
  If[truncate===None,
    truncate={-1,Infinity}
  ];
  IsBoolean[eComputeAverageField, "ConvertToImageCS", convert];
  IsBoolean[eComputeAverageField, "ReplaceMissing", replace];
  MemberCheck[eComputeAverageField, "Method", method, {"Mean", "Median", "NonNormalizedMean", "NonNormalizedMedian", "MeanNorm", "MedianNorm"}];
  (*implementation*)
  dimensions = Dimensions@First@matrix;
  result=Switch[method,
    "Mean",
    Map[truncatedMean[#, truncate]&, matrix /. Missing[]->{}, {2}],
    "Median",
    Map[truncatedMedian[#, truncate]&, matrix /. Missing[]->{}, {2}],
		"NonNormalizedMean",
    Map[truncatedNNMean[#, truncate]&, matrix /. Missing[]->{}, {2}],
    "NonNormalizedMedian",
    Map[truncatedNNMedian[#, truncate]&, matrix /. Missing[]->{}, {2}],
    "MeanNorm",
    Map[truncatedMeanNorm[#, truncate]&, matrix /. Missing[]->{}, {2}],
    "MedianNorm",
    Map[truncatedMedianNorm[#, truncate]&, matrix /. Missing[]->{}, {2}]
  ];
  result=If[convert,
    f[x:{}]={};
    f[{x_?NumericQ,y_?NumericQ}]:={x,-y};
    Transpose@Map[f@# &, result, {2}],
    result
  ];
  If[replace,
		If[!MemberQ[{"MeanNorm", "MedianNorm"}, method],
    	result /. {} -> {0, 0},
      result /. {} -> 0
		],
    result /. {} -> Missing[]
  ]
]
eComputeAverageField[matrices_, opts:OptionsPattern[]]:= Block[
	{convert, replace, truncate, method, dimensions, tmp},
  {convert, replace,truncate,method} = OptionValue[{ConvertToImageCS, ReplaceMissing, Truncation,Method}];
  If[!MatchQ[truncate, (None | {_?NumericQ, _?NumericQ}| {-Infinity, _?NumericQ} | {_?NumericQ, Infinity})],
    Message[eComputeAverageField::invo];
    Abort[]
  ];
  If[truncate===None,
    truncate={-1,Infinity}
  ];
  IsBoolean[eComputeAverageField, "ConvertToImageCS", convert];
  IsBoolean[eComputeAverageField, "ReplaceMissing", replace];
  If[MemberQ[{"Median", "NonNormalizedMedian", "MedianNorm"}, method],
    Message[eComputeAverageField::info]
  ];
  tmp = eComputeAverageField[#, Sequence@@DeleteCases[{opts}, (ConvertToImageCS -> _) | (ReplaceMissing -> _)], ReplaceMissing->False, ConvertToImageCS->False] &/@ matrices;
	If[!MemberQ[{"MeanNorm", "MedianNorm"}, method],
		result=eComputeAverageField[DeleteMissing[Transpose[tmp, {3,1,2}], Infinity]/.{}->Missing[], Sequence@@DeleteCases[{opts}, (Method->_) | (Truncation->_)], Truncation->None, Method->"Mean"],
    result=Map[Mean,DeleteMissing[Transpose[tmp, {3,1,2}],Infinity]/.{}->Missing[] ,{2}];
    If[replace,
      result/.Mean[_]->0,
      result/.Mean[_]->Missing[]
    ]
	]
]

Clear[truncatedMean]
truncatedMean[differences:{{__?NumericQ}...}, {min_?NumericQ | -Infinity, max_?NumericQ | Infinity}]:=
	(Mean[Normalize/@Cases[differences, x_ /; min<=Norm[x]<=max]])/.Mean[{}]->{}

Clear[truncatedMedian]
truncatedMedian[differences:{{__?NumericQ}...}, {min_?NumericQ | -Infinity, max_?NumericQ | Infinity}]:=
    (Median[Normalize/@Cases[differences, x_ /; min<=Norm[x]<=max]])/.Median[{}]->{}

Clear[truncatedNNMean]
truncatedNNMean[differences:{{__?NumericQ}...}, {min_?NumericQ | -Infinity, max_?NumericQ | Infinity}]:=
    (Mean[Cases[differences, x_ /; min<=Norm[x]<=max]])/.Mean[{}]->{}

Clear[truncatedNNMedian]
truncatedNNMedian[differences:{{__?NumericQ}...}, {min_?NumericQ | -Infinity, max_?NumericQ | Infinity}]:=
    (Median[Cases[differences, x_ /; min<=Norm[x]<=max]])/.Median[{}]->{}

Clear[truncatedMeanNorm]
truncatedMeanNorm[differences:{{__?NumericQ}...}, {min_?NumericQ | -Infinity, max_?NumericQ | Infinity}]:=
    (Mean[Norm/@Cases[differences, x_ /; min<=Norm[x]<=max]])/.Mean[{}]->{}

Clear[truncatedMedianNorm]
truncatedMedianNorm[differences:{{__?NumericQ}...}, {min_?NumericQ | -Infinity, max_?NumericQ | Infinity}]:=
    (Median[Norm/@Cases[differences, x_ /; min<=Norm[x]<=max]])/.Median[{}]->{}

Clear[eComputeDirectionalMoments]
Options[eComputeDirectionalMoments]=Evaluate@Join[{},Options[eComputeAverageField]];
eComputeDirectionalMoments[data:{{({_?NumericQ,_?NumericQ}|_Missing)..}..}, property_String, opts:OptionsPattern[]]:=Block[
	{f},
  MemberCheck[eComputeDirectionalMoments, "Property", property, {"Angle", "Variance", "StandardDeviation"}];
	Switch[property,
		"Angle",
		f[x:{__?NumericQ}]:=Arg[Complex[Sequence@@x]];
		f[x_Missing]=Missing[],
		"Variance",
		f[x:{__?NumericQ}]:=1-Norm[x];
		f[x_Missing]=Missing[],
		"StandardDeviation",
		f[x:{__?NumericQ}]:=Sqrt[-2*Log[Norm[x]]];
		f[x_Missing]=Missing[]
	];
  Map[f, data, {2}]
]

End[] (* End Private Context *)

EndPackage[]