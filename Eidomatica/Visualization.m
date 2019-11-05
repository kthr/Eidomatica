(* Mathematica Package *)

BeginPackage["Eidomatica`Visualization`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Internal`"]
Needs["Eidomatica`Utilities`"]

(* Exported symbols added here with SymbolName::usage *)
ePlotContours::usage="ePlotContours[image, contours] plots contours into the image. PlotContours[contours] plots simply the contours."
eVectorFieldPlot::usage = "eVectorFieldPlot[field] visualizes the given vector field in the specified manner.
eVectorFieldPlot[field,image] plot the given vector field as arrows onto the given image."
	Chunks::usage = "Chunks is an option for eVectorFieldPlot[field] specifying the number of chunks the color wheel is divided to. This option affects the plot only, if the option 'VisualizationsStyle' is set to \"Angle\"."
	MinimalRange::usage = "MinimalRange is an option for eVectorFieldPlot[] specifying the minimal size of vectors to show in the plot, vectors smaller than this value are omitted."
	ShowColorWheel::usage = "ShowColorWheel is an option for eVectorFieldPlot[field] specifying if the color wheel corresponding to the resulting plot is shown too."
	VisualizationStyle::usage = "VisualizationStyle is an option for eVectorFieldPlot[field] specifying the style for the visualization. Possible values are \"Angle\", \"Arrows\", \"Plain\" and \"Surface\"."
CircularHistogram::usage="CircularHistogram[data, inter, nbins] creates a nbins-binned circular histogram from darta in the interval inter."
	PlotMean::usage="PlotMean is ann option for CircularHistogram[], if True"
eColorData::usage="eColorData[] returns all types of available colors."
Begin["`Private`"] (* Begin Private Context *) 

(*Plot contours*)
Clear[ePlotContours];
ePlotContours[image_Image,contours:{{{_?NumericQ,_?NumericQ}..}..},opts:OptionsPattern[Plot]]:=Module[
{closed},
	closed=Join[#,{First@#}]&/@contours;
	Show[{
		image,
		ListLinePlot[closed,opts]
	}]
]
ePlotContours[image_Image,contour:{{_?NumericQ,_?NumericQ}..},opts:OptionsPattern[Plot]]:=ePlotContours[image,{contour},opts]
ePlotContours[contours:{{{_?NumericQ,_?NumericQ}..}..},opts:OptionsPattern[Plot]]:=Module[
	{closed},
	closed=Join[#,{First@#}]&/@contours;
	ListLinePlot[closed,opts]
]
ePlotContours[contour:{{_?NumericQ,_?NumericQ}..},opts:OptionsPattern[Plot]]:=ePlotContours[{contour},opts]

Clear[eVectorFieldPlot];
eVectorFieldPlot::noint = noint;
eVectorFieldPlot::nonum = nonum;
eVectorFieldPlot::nobool = nobool;
eVectorFieldPlot::nopos = nopos;
eVectorFieldPlot::nomemb = nomemb;
Options[eVectorFieldPlot] = {ShowColorWheel -> False, VisualizationStyle -> "Plain", MinimalRange -> .01, Chunks -> 12, InterpolationMethod -> "Linear"};
eVectorFieldPlot[field:{{{_?NumericQ, _?NumericQ} ..} ..}, opts:OptionsPattern[]] := Module[
	{modName=eVectorFieldPlot, showWheel, style, interpolationMethod, range, normalizedField, polarField, visualization, color, remap, chunks, inside, ydim},
  
  	(*options validation*)
  	{chunks, range, showWheel, style,interpolationMethod} = OptionValue[{Chunks, MinimalRange, ShowColorWheel, VisualizationStyle,InterpolationMethod}];
  		(*chunks*)
  		IntegerCheck[modName,"Chunks",chunks];
  		(*range*)
  		NumberCheck[modName,"MinimalRange",range];
  		IsPositive[modName,"MinimalRange",range];
  		(*color wheel*)
  		IsBoolean[modName,"ShowColorWheel",showWheel];
  		(*style*)
  		MemberCheck[modName,"VisualizationStyle",style,{"Angle","Arrows","Plain","Surface"}];
  		(*interpolation method*)
  		MemberCheck[modName,"InterpolationMethod",interpolationMethod,{"Linear","Cubic"}];
  	
  	(*implementation*)
  	visualization = 
  	Switch[style,
    	"Angle",
		polarField = Map[{First@#, Last@#/Pi} &, eToPolarCS[Chop[field]], {2}];(*transform to polar coordinates and map angle to range -1..1*)
    	inside = Nearest[Range[-1, 1, 2/chunks]];
    	color[length_, angle_] := 
		If[length <= range, 
			{0., 0., 0.}, 
			eRYBtoRGB[eRYBColor[First@inside[angle]], InterpolationMethod -> interpolationMethod]
		];
		remap = Compile[{{x, _Real}},If[Sign[x] == -1, -(2 + x - 1), -(x - 1)]];
    	Image[ParallelMap[color[First@#, remap@Last@#] &, polarField, {2}, DistributedContexts->Automatic], ColorSpace -> "RGB"],
    	"Arrows",
    		ydim = First@Dimensions[field];
    		Graphics[{Arrowheads[Small], 
    			MapIndexed[Arrow[{{Last@#2, ydim - First@#2} + {-.5, .5}, {Last@#2, ydim - First@#2} + {First@#1, Last@#1} + {-.5, .5}}] &, 
       			field, {2}]}
       		],
    	"Plain",
    		If[$librariesFound,
    			Image[llVectorFieldPlot[field],ColorSpace->"HSB"],
    			normalizedField = field/Max[Max[Norm /@ Flatten[field, {1, 2}]],1];
    			Image[Map[{Mod[-.5Last@#/Pi,1], First@#, .9} &, eToPolarCS[Chop[normalizedField]], {2}], ColorSpace -> "HSB"]
    		],
    	"Surface",
    		color = Function[{x, y, z}, Hue[Sequence @@ ({Last@#, First@#} &@(polarField[[Max[1, Round@x], Max[1, Round@y]]])), .8]];
    		ListPlot3D[Map[Norm, Transpose@field, {2}], PlotRange -> All, ColorFunction -> color, ColorFunctionScaling -> False, 
     			Mesh -> None, AspectRatio -> Divide @@ Reverse@Take[Dimensions[field], 2]
     		]
	];
  	If[showWheel,
   		If[SameQ[style, "Angle"],
    		{visualization, Image[DiskMatrix[99, 201]*Table[color[First@#, Last@#/Pi] &@eToPolarCS[{x, y}], {y, 100, -100, -1}, {x, 100, -100, -1}], ColorSpace -> "RGB", ImageSize -> 200]},
    		{visualization, Image[DiskMatrix[99, 201]*Table[{((Last@#)/Pi + 1)/2, First@#/99, .9} &@eToPolarCS[{x, y}], {y, 100, -100, -1}, {x, 100, -100, -1}], ColorSpace -> "HSB", ImageSize -> 200]}
    	],
   		visualization
	]
]
eVectorFieldPlot[field_VectorField, opts : OptionsPattern[]] := eVectorFieldPlot[eVectorFieldData[eToCartesianCS[field]], opts]
eVectorFieldPlot[field_VectorField, image_Image, opts : OptionsPattern[]] := Show[{Graphics[{Opacity[.6], Raster[Reverse@ImageData@image]}], eVectorFieldPlot[field, VisualizationStyle -> "Arrows"]}]

Clear[CircularHistogram]
eVectorFieldPlot::nobool = nobool;
Options[CircularHistogram] := Join[Options[SectorChart],{PlotMean->False}];
CircularHistogram[{}, interval : {_?NumericQ, _?NumericQ}, nbins_Integer, opts : OptionsPattern[]]=Missing[];
CircularHistogram[data : {__?NumberQ}, interval : {_?NumericQ, _?NumericQ}, nbins_Integer, opts : OptionsPattern[]] := Block[
  {mean, tmp, scOpts, counts},
	mean=OptionValue[PlotMean];
	SetOptions[SectorChart, PolarAxes -> True, PolarTicks -> {"Direction", Automatic}, PolarGridLines -> {Table[2 Pi k/nbins, {k, 0, nbins - 1}], Automatic}, SectorOrigin -> {Pi, "Counterclockwise"}];
  scOpts = Sequence @@ FilterRules[{opts}, First /@ Options[SectorChart]];
  IsBoolean[CircularHistogram,"PlotMean",mean];

	(*implementation*)
  counts = BinCounts[data, {Sequence @@ interval, First@Differences[interval]/nbins}];
	If[mean,
		tmp=If[Length@data>1,
      Mean[{Cos[#], Sin[#]} & /@ data],
			{Cos[#], Sin[#]} & @ First@data
		];
  	Show[SectorChart[Transpose[{ConstantArray[1, nbins], counts}], scOpts],Graphics[{Red, Thick, Arrow[{{0, 0}, tmp}]}]],
    SectorChart[Transpose[{ConstantArray[1, nbins], counts}], scOpts]
	]
]

$brewerColorData=Import@FindFile["Eidomatica/color_maps/brewer.wdx"];
$types=Union[$brewerColorData[[All,3]]];
$names=Union[$brewerColorData[[All,1]]];
Clear[eColorData]
eColorData[]=TableForm@{	{"Diverging", "Diverging schemes put equal emphasis on mid-range critical values and \rextremes at both ends of the data range. The critical class or break \rin the middle of the legend is emphasized with light colors and low and \rhigh extremes are emphasized with dark colors that have contrasting hues.\r"},
	{"Qualitative", "Qualitative schemes do not imply magnitude differences between legend classes, and \rhues are used to create the primary visual differences between classes. Qualitative \rschemes are best suited to representing nominal or categorical data.\r"},
  {"Sequential", "Sequential schemes are suited to ordered data that progress from low to high. Lightness \rsteps dominate the look of these schemes, with light colors for low data values to dark \rcolors for high data values."}};
eColorData[name_String]:=Block[
	{colors,num,type,colorList},
	If[MemberQ[$types, name],
    Union[name/.(#[[1,3]]->#[[All,1]]&/@GatherBy[$brewerColorData,#[[3]]&])],
		If[MemberQ[$names, name],
      colors=First@Flatten[#] -> # & /@ (GatherBy[#,#[[2]]&]&/@GatherBy[$brewerColorData,#[[1]]&]);
			TableForm@Partition[Map[
				({num,type}=Flatten[#][[{2,3}]];
					colorList=RGBColor[Sequence@@Take[#,-3]]&/@#;
					{ToString@IntegerPart[num]<>":",Graphics[Table[{colorList[[i]], Rectangle[{i - 1, 0}, {i, 1}]}, {i, 1, Length@colorList}], ImageSize -> {Automatic,25}]}
				)&
				,name/.colors],4, 4, 1, {}]
		]
	]
]
eColorData[name_String, ncolors_Integer]:=Block[
  {schemes,colors},
  If[MemberQ[$types, name],
    Union[(ncolors /. Union[IntegerPart@Flatten[#][[2]] -> #[[All,1]] &/@ (GatherBy[name/.(Flatten[#][[3]] -> # &/@GatherBy[$brewerColorData,#[[3]]&]), #[[2]]&])]) /. _Integer -> {}],
    If[MemberQ[$names, name],
      colors={First[#], IntegerPart[#[[2]]]} -> # & /@ $brewerColorData;
      colorList=RGBColor[Sequence@@Take[#,-3]]&/@ (Last/@ Cases[colors, x_ /; Last@First@x == ncolors])
    ]
  ]
]

End[] (* End Private Context *)

EndPackage[]
