(* Mathematica Package *)

BeginPackage["Eidomatica`ZebrafishAnalysis`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Tracking`"]
Needs["Eidomatica`Utilities`"]
Needs["Eidomatica`Internal`"]
Needs["Eidomatica`Transformation`"]

(* Exported symbols added here with SymbolName::usage *)  
GetExperimentProperty::usage="GetExperimentProperty[metaFile, experimentName, property] given the .xls file containing the meta data, the experiment name this script extracts the selected properties for the experiment.
GetExperimentProperty[metaFile,experimentName,{properties..}] extracts several properties at once."
CellDensityMatrix::usage = "CellDensityMatrix[tracks] creates cell density matrices from all the given time points. 
CellDensityMatrix[tracks, frameID] creates the cell density matrix for the given frame. 
CellDensityMatrix[tracks, {frameIDs..}] creates the cell density matrices for the given frame ids.
CellDensityMatrix[points] creates the cell density matrix for the given points."
	BandWidth::usage = "BandWidth controls the bandwidth for the density plot."
	MapProjection::usage = "MapProjection defines the kind of projection, which is used for the density plot. This can be either \"Mercator\" or \"Bonne\"."
	PlotDimensions::usage = "PlotDimensions defines the size of the computed density plot in pixels (the bigger the longer it takes to compute)."
	PositionalCorrection::usage = "PositionalCorrection either None or a correction in x and y direction of the given points. Each point p={p1,p2} is corrected by p={p1,p2}+{x,y}."
CellFeatureMap::usage = "CellFeatureMap[points, features] creates the feature map for the given points and their associated features."
CellDensityMatrixBandwidth::usage="CellDensityMatrixBandwidth[A, R] computes the bandwidth for a spherical cap with the given area A, given a sphere with of radius R."
(*eGreatCircleDistance::usage = "eGreatCircleDistance[{longitudeS,latitudeS,radiusS},{longitudeE,latitudeE,radiusE}] computes the great circle distance a starting and an end point.*)
(*eGreatCircleDistance[{{longitude,latitude,radius}..}] computes the great circle distance between consecutive points in the given list."*)
(*ToPolar::usage = "ToPolar[{{x,y}..}] transforms the given coordinates from the Mercator projection to polar coordinates."*)
	(*OriginalDimensions::usage = "OriginalDimension defines the dimensions of the original image, where the coordinates where computed from."*)
	(*LateralProjectionRange::usage = "LateralProjectionRange is the projection range of the Mercator projection."*)
	(*Radius::usage="Radius the radius of the resulting polar coordinate."*)
(*eComputeGeodesic::usage="eComputeGeodesic[p1:{latitude1,longitude1,radius1}, p2:{latitude2,longitude2,radius2}] returns the parameterized geodesic from p1 to p2."*)
(*ToCartesian::usage="ToCartesian[{{longitude,latitude,radius}..}] converts list of polar coordinates to cartesian coordinates.*)
(*ToCartesian[{longitud,latitude,radius}] converts a single polar point to cartesian coordinate system."*)
(*ToMercator::usage="ToMercator[{{longitude,latitude,radius}..}] converts list of polar coordinates to the respective positions on a mercator projection.*)
(*ToMercator[{longitude,latitude,radius}] converts a polar coordinate to the respective position on a mercator projection."*)
(*CentralMeridian::usage = "CentralMeridian is an option for all functions concerning Bonne projections. The option defines the central meridian of the projection."*)
(*StandardParallel::usage = "StandardParallel is an option for all functions concerning Bonne projections. The option defines the standard parallel of the projection."*)

FishyPlot::usage="FishyPlot[tracks] plots the tracks."
	Feature::usage="Feature is an option for FishyPlot[]."
	GlobalBounds::usage="GlobalBounds is an option for FishyPlot[]."
	LineThickness::usage="LineThickness is an option for FishyPlot[]."
	LowerQuantile::usage="LowerQuantile is an option for FishyPlot[]."
	MinMax::usage="MinMax is an option for FishyPlot[]."
	MovingAverageBlockLength::usage="MovingAverageBlockLength is an option for FishyPlot[]."
	Scheme::usage="Scheme is an option for FishyPlot[]."
	UpperQuantile::usage="UpperQuantile is an option for FishyPlot[]."
	
MedianDeviationPlot::usage="MedianDeviationPlot[samples] given a list of samples (row-wise) this function computes the median and median deviation and plots them."
StandardDeviationPlot::usage="StandardDeviationPlot[samples] given a list of samples (row-wise) this function computes the mean and standard deviation and plots them."
StandardErrorPlot::usage="StandardErrorPlot[samples] given a list of samples (row-wise) this function computes the mean and standard error and plots them."
ConfidencePlot::usage="ConfidencePlot[samples] given a list of samples (row-wise) this function computes the mean and standard error times 1.96 and plots them."
	Color::usage="Color is an option for DeviationPlots which specifies the color of the lines and the area."
	FillingOpacity::usage="FillingOpacity is an option for DeviationPlots which specifies the opacity of the area around the mean."
	NormalizeRange::usage="NormalizeRange is an option for DeviationPlots, when turned True plot will be made between 0 and 1."
	LineStyle::usage="LineStyle defines the style of the plotted lines, when Automatic the lines are solid. Other options are Dashed, etc."
	DeviationScaling::usage="DeviationScaling scales the width of the deviation region for visual purposes in MedianDeviationPlot, StandardDeviationPlot and StandardErrorPlot."
colorData::usage="colorData[\"Scheme\"][t] gives the color value at t for the selected scheme."

Begin["`Private`"] (* Begin Private Context *)

Clear[GetExperimentProperty]
GetExperimentProperty::nomf="Meta data file '`1`' not found!";
GetExperimentProperty::enf="Experiment '`1`' not found in meta data.";
GetExperimentProperty::pnf="Property '`1`' not found!"
GetExperimentProperty[metaFile_String, experimentName_String, property_String]:=First@GetExperimentProperty[metaFile,experimentName,{property}]
GetExperimentProperty[metaFile_String, experimentName_String, properties:{__String}]:=Block[
	{metaData,names,experimentData,positions,pos},
	If[!FileExistsQ[metaFile],
		Message[GetExperimentProperty::nomf, metaFile];
		Abort[]
	];
	metaData=First@Import[metaFile];
	names=First@metaData;
	experimentData=Cases[metaData, x_/;First@x===experimentName];
	If[experimentData==={},
		Message[GetExperimentProperty::enf,experimentName];
		Abort[],
		experimentData=First@experimentData
	];
	positions=(
		pos=Position[names,#];
		If[pos==={},
			Message[GetExperimentProperty::pnf, #];
			"",
			ToExpression[experimentData[[First@First@pos]]]
		]
		) & /@properties
]

Clear[CellDensityMatrix];
CellDensityMatrix::nomemb = nomemb
CellDensityMatrix::nonum = nonum;
CellDensityMatrix::noint = noint;
CellDensityMatrix::rad = "Radius is not 1, remember setting the option 'BandWidth' accordingly in order to get a nice plot!";
CellDensityMatrix::range = "Parameter \'frames\' contains values <1, this might lead to errors.";
Options[CellDensityMatrix]:=Union@Join[{BandWidth->Pi/30,PlotDimensions->{50,50},MapProjection->"Mercator",PositionalCorrection->None},Options[Bonne],Options[ToSpherical]];
CellDensityMatrix[tracks:{Rule[_Integer,{{{_?NumericQ,_?NumericQ},_Integer}..}]..},opts:OptionsPattern[]]:=Block[
	{lastFrame,numberFrames},
	{lastFrame,numberFrames}={Max@Flatten[tracks[[All,2,All,2]]],Length@Union@Flatten[tracks[[All,2,All,2]]]};
	If[numberFrames<lastFrame,
		Print["[WARNING] Some frames don't seem to contain any cells!"];
	];
	CellDensityMatrix[tracks,Range[1,Min[lastFrame,numberFrames]],opts]
]
CellDensityMatrix[tracks:{Rule[_Integer,{{{_?NumericQ,_?NumericQ},_Integer}..}]..},frame_Integer,opts:OptionsPattern[]]:=First@CellDensityMatrix[tracks,{frame},opts]
CellDensityMatrix[tracks:{Rule[_Integer,{{{_?NumericQ,_?NumericQ},_Integer}..}]..},frames:{__Integer},opts:OptionsPattern[]]:=Block[
	{trackFrames},
  If[Min[frames]<1, Message[CellDensityMatrix::range]];
  trackFrames = Last@First@# -> #[[All, 1]] & /@ GatherBy[Flatten[Last/@tracks,{1,2}],Last];
  trackFrames = Join[trackFrames,{_->{}}];
  Table[CellDensityMatrix[i/.trackFrames,opts],{i,frames}]
]
CellDensityMatrix[{}, opts:OptionsPattern[]]:=Block[
  {plotDim},
  plotDim=OptionValue[PlotDimensions];
  IntegerCheck[CellDensityMatrix,"PlotDimensions",#]&/@plotDim;
  ConstantArray[0, plotDim]
]
CellDensityMatrix[points:{{_?NumericQ,_?NumericQ}..}, opts:OptionsPattern[]]:=Block[
	{bandWidth,dim,plotDim,projection,radius,lateralProjectionRange,standardParallel, centralMeridian, correction, pointList},
	{bandWidth,dim,plotDim,projection,radius,lateralProjectionRange, standardParallel, centralMeridian, correction} = OptionValue[{BandWidth,OriginalDimensions,PlotDimensions,MapProjection,Radius,LateralProjectionRange, StandardParallel, CentralMeridian,PositionalCorrection}];

	(*options validation*)
	NumberCheck[CellDensityMatrix,"BandWidth",bandWidth];
	IntegerCheck[CellDensityMatrix,"OriginalDimensions",#]&/@dim;
	IntegerCheck[CellDensityMatrix,"PlotDimensions",#]&/@plotDim;
	MemberCheck[CellDensityMatrix,"MapProjection",projection,{"Bonne","Cartesian","Mercator"}];
	NumberCheck[CellDensityMatrix,"Radius",radius];
	NumberCheck[CellDensityMatrix,"LateralProjectionRange",lateralProjectionRange];
	NumberCheck[CellDensityMatrix,"StandardParallel",standardParallel];
	NumberCheck[CellDensityMatrix,"CentralMeridian",centralMeridian];

	If[radius!=1,
		Message[CellDensityMatrix::rad]
	];

	If[!SameQ[correction,None],
		NumberCheck[CellDensityMatrix,"PositionalCorrection",#]&/@correction;
		pointList=#+correction&/@points,
		pointList=points;
	];

	(*implementation*)

	Switch[projection,
		"Bonne",
		llDensity[Flatten[pointList],plotDim,dim,radius,lateralProjectionRange,bandWidth, 0, centralMeridian, standardParallel],
		"Cartesian",
		llDensity[Flatten[pointList],plotDim,dim,radius,lateralProjectionRange,bandWidth, 1, centralMeridian, standardParallel],
		"Mercator",
		Reverse@llDensity[Flatten[pointList],plotDim,dim,radius,lateralProjectionRange,bandWidth, 2, centralMeridian, standardParallel]
	]
]

Clear[CellFeatureMap];
Options[CellFeatureMap]:=Options[CellDensityMatrix]
CellFeatureMap[points:{{_?NumericQ,_?NumericQ}..}, features:{__?NumericQ}, opts:OptionsPattern[]]:=Block[
  {bandWidth,dim,plotDim,projection,radius,lateralProjectionRange,standardParallel, centralMeridian, correction, pointList},
  If[Length@points != Length@features,
		Message[CellFeatureMap:ueql];
		Abort[]
	];
  {bandWidth,dim,plotDim,projection,radius,lateralProjectionRange, standardParallel, centralMeridian, correction} = OptionValue[{BandWidth,OriginalDimensions,PlotDimensions,MapProjection,Radius,LateralProjectionRange, StandardParallel, CentralMeridian,PositionalCorrection}];

  (*options validation*)
  NumberCheck[CellDensityMatrix,"BandWidth",bandWidth];
  IntegerCheck[CellDensityMatrix,"OriginalDimensions",#]&/@dim;
  IntegerCheck[CellDensityMatrix,"PlotDimensions",#]&/@plotDim;
  MemberCheck[CellDensityMatrix,"MapProjection",projection,{"Bonne","Cartesian","Mercator"}];
  NumberCheck[CellDensityMatrix,"Radius",radius];
  NumberCheck[CellDensityMatrix,"LateralProjectionRange",lateralProjectionRange];
  NumberCheck[CellDensityMatrix,"StandardParallel",standardParallel];
  NumberCheck[CellDensityMatrix,"CentralMeridian",centralMeridian];

  If[radius!=1,
    Message[CellDensityMatrix::rad]
  ];

  If[!SameQ[correction,None],
    NumberCheck[CellDensityMatrix,"PositionalCorrection",#]&/@correction;
    pointList=#+correction&/@points,
    pointList=points;
  ];

  (*implementation*)
	Switch[projection,
		"Bonne",
		llFeatureMap[Flatten[pointList],features, plotDim,dim,radius,lateralProjectionRange,bandWidth, 0, centralMeridian, standardParallel],
		"Cartesian",
		llFeatureMap[Flatten[pointList],features, plotDim,dim,radius,lateralProjectionRange,bandWidth, 1, centralMeridian, standardParallel],
		"Mercator",
		Reverse@llFeatureMap[Flatten[pointList],features, plotDim,dim,radius,lateralProjectionRange,bandWidth, 2, centralMeridian, standardParallel]
	]
]

Clear[CellDensityMatrixBandwidth];
CellDensityMatrixBandwidth[area_,radius_]:=Block[
	{bandwidth},
	Off[Solve::ifun];
	Last[bandwidth /. NSolve[2*Pi*radius^2 (1 - Cos[bandwidth/radius]) == area, bandwidth]]
]

(**)
(*computes great circle distance for a sphere of radius 1 (radius is omitted), latitude has to be in the range of 0..Pi and longitude in the range from 0..2Pi*)
(*distance is computed for latitude between -Pi/2..Pi/2 (latitude-Pi/2)*)
(**)
(*Clear[eGreatCircleDistance,gcd]*)
(*eGreatCircleDistance[{longitudeS_,latitudeS_,radiusS_}, list:{{_?NumericQ,_?NumericQ,_?NumericQ}..}]:=Map[gcd[longitudeS,latitudeS,radiusS,Sequence@@#]&,list]*)
(*eGreatCircleDistance[list:{{_?NumericQ,_?NumericQ,_?NumericQ}..}]:=MapThread[eGreatCircleDistance[#1,#2]&,{Drop[list,1],Drop[list,-1]}]*)
(*eGreatCircleDistance[{longitudeS_,latitudeS_,radiusS_},{longitudeE_,latitudeE_,radiusE_}]:=gcd[longitudeS,latitudeS,radiusS,longitudeE,latitudeE,radiusE]*)
(*gcd=Compile[{{longitudeS,_Real},{latitudeS,_Real},{radiusS,_Real},{longitudeE,_Real},{latitudeE,_Real},{radiusE,_Real}},radiusS*2ArcSin[Sqrt[Haversine[latitudeE-latitudeS]+Sin[latitudeS]*Sin[latitudeE]*Haversine[longitudeE-longitudeS]]]];*)

(*Clear[eComputeGeodesic, geodesic];*)
(*eComputeGeodesic::same="The given points from=`1` and to=`2` are identical!";*)
(*eComputeGeodesic[from:{_?NumericQ, _?NumericQ, _?NumericQ}, to:{_?NumericQ, _?NumericQ, _?NumericQ}] := geodesic[from - {Pi, Pi / 2, 0}, to - {Pi, Pi / 2, 0}, None]*)
(*eComputeGeodesic[from:{_?NumericQ, _?NumericQ, _?NumericQ}, to:{_?NumericQ, _?NumericQ, _?NumericQ}, distance_?NumericQ] := geodesic[from - {Pi, Pi / 2, 0}, to - {Pi, Pi / 2, 0}, distance]*)
(*eComputeGeodesic[from:{_?NumericQ, _?NumericQ, _?NumericQ}, azimuth_?NumericQ, distance_?NumericQ] := geodesic[from - {Pi, Pi / 2, 0}, 2 Pi - (azimuth - Pi/2), distance]*)
(*eComputeGeodesic[from:{_?NumericQ, _?NumericQ, _?NumericQ}, azimuth_?NumericQ] := geodesic[from - {Pi, Pi / 2, 0}, 2 Pi - (azimuth - Pi/2), None]*)
(*geodesic[p1 : {lambda1_, theta1_, r1_}, p2 : {lambda2_, theta2_, r1_}, distance_] := Block[*)
  (*{f, lambda12, alpha1, alpha2, sigma12, alpha0, sigma01, sigma02, lambda01, lambda0, sign, position, pos, correction},*)
  (*lambda=longitude, theta=latitude*)

	(*If[p1===p2,*)
		(*Message[eComputeGeodesic::same,p1,p2];*)
		(*Return[BSplineFunction[{p1,p2},SplineDegree->1]]*)
	(*];*)
  (*f=Compile[{{delta, _Real}}, Piecewise[{{Sign[-delta] Pi + Sign[delta] Mod[Abs@delta, Pi], Abs@delta > Pi}, {delta, Abs@delta <= Pi}}]];*)
  (*lambda12 = f[lambda2 - lambda1];*)
  (*alpha1 = ArcTan[Cos[theta1] * Tan[theta2] - Sin[theta1] * Cos[lambda12], Sin[lambda12]];*)
  (*alpha2 = ArcTan[-Cos[theta2] * Tan[theta1] + Sin[theta2] * Cos[lambda12], Sin[lambda12]];*)
  (*sigma12 = ArcCos[Sin[theta1] Sin[theta2] + Cos[theta1] Cos[theta2] Cos[ lambda12]];*)
  (*alpha0 = ArcTan[Sqrt[Cos[alpha1]^2 + Sin[alpha1]^2 * Sin[theta1]^2], Sin[alpha1] Cos[theta1]];*)
  (*sigma01 = Piecewise[{{0, alpha1 == Pi / 2 && theta1 == 0}, {ArcTan[Cos[alpha1], Tan[theta1]], True}}];*)
  (*sigma02 = Piecewise[{{0, alpha2 == Pi / 2 && theta2 == 0}, {ArcTan[Cos[alpha2], Tan[theta2]], True}}];*)
  (*lambda01 = ArcTan[Cos[sigma01], Sin[alpha0] Sin[sigma01]];*)
  (*lambda0 = f[lambda1 - lambda01];*)
  (*position = Compile[{{sigma,_Real}}, {ArcTan[Cos[sigma], Sin[alpha0] Sin[sigma]] + lambda0, ArcTan[Sqrt[Cos[sigma]^2 + Sin[alpha0]^2 * Sin[sigma]^2], Cos[alpha0] Sin[sigma]], r1}];*)
  (*correction = Compile[{{longitude, _Real}, {latitude, _Real}, {radius, _Real}}, {Sawtooth[longitude, 2 Pi, Pi], Sawtooth[latitude, Pi, Pi/2], radius}];*)
	(*If[distance === None,*)
		(*BSplineFunction[Table[correction[Sequence@@position[f[sigma]]] + {Pi, Pi / 2, 0}, {sigma, sigma01, sigma01 + sigma12, sigma12 / 500}], SplineDegree -> 1],*)
    (*correction[Sequence@@position[f[sigma01+distance]]] + {Pi, Pi / 2, 0}*)
	(*]*)
(*]*)
(*geodesic[p1 : {lambda1_, theta1_, r1_}, azimuth_?NumericQ, distance_]:=Block[*)
	(*{sigma01, alpha0, lambda01, lambda0, f, position, correction},*)
  (*sigma01 = Piecewise[{{0, azimuth == Pi / 2 && theta1 == 0}, {ArcTan[Cos[azimuth], Tan[theta1]], True}}];*)
  (*alpha0 = ArcTan[Sqrt[Cos[azimuth]^2 + Sin[azimuth]^2 * Sin[theta1]^2], Sin[azimuth] Cos[theta1]];*)
  (*lambda01 = ArcTan[Cos[sigma01], Sin[alpha0] Sin[sigma01]];*)
  (*lambda0 = f[lambda1 - lambda01];*)
  (*f=Compile[{{delta, _Real}}, Piecewise[{{Sign[-delta] Pi + Sign[delta] Mod[Abs@delta, Pi], Abs@delta > Pi}, {delta, Abs@delta <= Pi}}]];*)
  (*position = Compile[{{sigma,_Real}}, {ArcTan[Cos[sigma], Sin[alpha0] Sin[sigma]] + lambda0, ArcTan[Sqrt[Cos[sigma]^2 + Sin[alpha0]^2 * Sin[sigma]^2], Cos[alpha0] Sin[sigma]], r1}];*)
  (*correction = Compile[{{longitude, _Real}, {latitude, _Real}, {radius, _Real}}, {-2 ArcTan[Cot[(longitude - Pi)/2]], -ArcTan[Cot[(latitude - Pi/2)]], radius}];*)
  (*correction = Compile[{{longitude, _Real}, {latitude, _Real}, {radius, _Real}}, {Sawtooth[longitude, 2 Pi, Pi], Sawtooth[latitude, Pi, Pi/2], radius}];*)
	(*If[distance===None,*)
    (*BSplineFunction[Table[correction[Sequence@@position[f[sigma01+distance]]] + {Pi, Pi / 2, 0}, {distance, 0, 2Pi, 2Pi/500}], SplineDegree -> 1],*)
		(*correction[Sequence@@position[f[sigma01+distance]]] + {Pi, Pi / 2, 0}*)
	(*]*)
(*]*)

(*returns a vector with {longitude,latitude,radius} with ranges {0..2Pi, 0..Pi, 0..}*)
(*Clear[ToPolar,top]*)
(*Options[ToPolar]={Radius->1,OriginalDimensions->{1000,1000},LateralProjectionRange->17Pi/18};*)
(*ToPolar[track:{{{_?NumericQ,_?NumericQ},_?NumericQ}..},opts:OptionsPattern[]]:=Map[{ToPolar[#[[1]],opts],#[[2]]}&,track]*)
(*ToPolar[track:{{_?NumericQ,_?NumericQ}..},opts:OptionsPattern[]]:=Block[*)
	(*{tmp},*)
	(*tmp=Sequence@@Flatten@OptionValue[{OriginalDimensions,Radius,LateralProjectionRange}];*)
	(*Map[top[Sequence@@#,tmp]&,track]*)
(*]*)
(*ToPolar[{x_?NumericQ,y_?NumericQ},opts:OptionsPattern[]]:=top[x,y,Sequence@@Flatten@OptionValue[{OriginalDimensions,Radius,LateralProjectionRange}]]*)
(*ToPolar[{},opts:OptionsPattern[]]={}*)
(*top=Compile[{{x,_Real},{y,_Real},{xdim,_Real},{ydim,_Real},{radius,_Real},{lpr,_Real}},{(x*2Pi)/xdim,ArcSin[Tanh[lpr*(-0.5+y/ydim)]]+Pi/2,radius}];*)
(*ToPolar for 3D cartesian points*)
(*ToPolar[p:{x_?NumericQ,y_?NumericQ,z_?NumericQ}]:=Block[*)
	(*{radius,latitude,longitude},*)
	(*radius=Norm[p];*)
	(*latitude=ArcCos[z/radius]-Pi/2;*)
	(*longitude=ArcTan[x,y];*)
	(*{longitude, latitude, radius}*)
(*]*)
(*ToPolar[list:{{_?NumericQ,_?NumericQ,_?NumericQ}..}]:=ToPolar/@list*)

(*Clear[ToCartesian]*)
(*ToCartesian[track:{{_?NumericQ,_?NumericQ,_?NumericQ}..}]:=Map[ToCartesian[#]&,track]*)
(*ToCartesian[coord:{longitude_?NumericQ,latitude_?NumericQ,radius_?NumericQ}]:=tcart[Sequence@@coord]*)
(*tcart=Compile[{{longitude,_Real},{latitude,_Real},{radius,_Real}}, {radius*Sin[latitude+Pi/2]*Cos[longitude],radius*Sin[latitude+Pi/2]*Sin[longitude],radius*Cos[latitude+Pi/2]}]*)

(*Clear[ToMercator];*)
(*Options[ToMercator]:=Options[ToPolar];*)
(*ToMercator[line:{{_?NumericQ,_?NumericQ,_?NumericQ}..},opts:OptionsPattern[]]:=ToMercator[#, opts]&/@line*)
(*ToMercator[{longitude_,latitude_,radius_},opts:OptionsPattern[]]:=tom[longitude,latitude,Sequence@@Flatten@OptionValue[{OriginalDimensions,LateralProjectionRange}]]*)
(*tom=Compile[{{longitude,_Real},{latitude,_Real},{xdim,_Real},{ydim,_Real},{lpr,_Real}},{(longitude)/(2Pi)*xdim,(ArcTanh[Sin[latitude-Pi/2]]/lpr+.5)*ydim}]*)

(*Clear[Bonne]*)
(*Options[Bonne]={CentralMeridian->0,StandardParallel->Pi/64,OriginalDimensions->{50,50}};*)
(*Bonne[{longitude_,latitude_},opts:OptionsPattern[]]:=Block[*)
	(*{longitude0,latitude0,rho,e},*)
	(*{longitude0,latitude0}=OptionValue[{CentralMeridian,StandardParallel}];*)
	(*rho[latitude1_]:=Cot[latitude0]+latitude0-latitude1;*)
	(*e[longitude1_,latitude1_]:=((longitude1-longitude0)*Cos[latitude1])/rho[latitude1];*)
	(*{rho[latitude]*Sin[e[longitude,latitude]],Cot[latitude0]-rho[latitude]*Cos[e[longitude,latitude]]}*)
(*]*)

(*returns {longitude,latitude,radius}*)
(*Clear[InverseBonne]*)
(*Options[InverseBonne]:=Union@Join[Options[Bonne],{Radius->100}];*)
(*InverseBonne[{x_,y_},opts:OptionsPattern[]]:=Block[*)
	(*{centralMeridian,standardParallel,rho,latitude},*)
	(*{centralMeridian,standardParallel}=OptionValue[{CentralMeridian,StandardParallel}];*)
	(*rho=Sign[standardParallel]*Sqrt[x^2+(Cot[standardParallel]-y)^2];*)
	(*latitude=Cot[standardParallel]+standardParallel-rho;*)
	(*N@{centralMeridian+rho*ArcTan[(Cot[standardParallel]-y),x]/Cos[latitude],latitude,OptionValue[Radius]}*)
(*]*)

(*Clear[BonneRegionFunction];*)
(*Options[BonneRegionFunction]:=Options[Bonne];*)
(*BonneRegionFunction[{x_,y_},opts:OptionsPattern[]]:=Block[*)
	(*{dims,tmp},*)
	(*dims=OptionValue[OriginalDimensions];*)
	(*tmp=InverseBonne[{x/dims[[1]]*2Pi-Pi,y/dims[[2]]*2Pi-Pi},opts];*)
	(*Abs[tmp[[2]]]<=Pi/2&&Abs[tmp[[1]]]<=Pi*)
(*]*)

Clear[thinkenedLine]
thinkenedLine[line:{{{_?NumericQ,_?NumericQ},_?NumericQ}..},color:{__RGBColor},thickness_?NumericQ]:=
	Table[{Thickness[thickness*Sqrt[line[[i,2]]]],Line[{line[[i,1]],line[[i+1,1]]},VertexColors->{color[[i]],color[[i+1]]}]},{i,1,Length@line-1}]
thinkenedLine[line:{{_?NumericQ,_?NumericQ}..},color:{__RGBColor},thickness_?NumericQ]:=
	Table[{Thickness[thickness*i/Length@line],Line[{line[[i]],line[[i+1]]},VertexColors->{color[[i]],color[[i+1]]}]},{i,1,Length@line-1}]

Clear[FishyPlot]
Options[FishyPlot]:=Union@Join[{Background->Automatic,Feature->"Time", ImageSize->Automatic, GlobalBounds->True, LineThickness->0.005, LowerQuantile->0, MinMax->Automatic, MovingAverageBlockLength->5, PlotRange->All,Scheme->"TemperatureMap", UpperQuantile->1},Options[ToSpherical],Options[eSelectTracks],Options[Graphics]];
FishyPlot[tracks:{Rule[_Integer,{{{_?NumericQ,_?NumericQ},_Integer}..}]..},range:{__Integer},opts:OptionsPattern[]]:=Block[
	{blockLength,dimension,feature,global,lower,max,min,minMax,opts1,opts2,opts3,thickness,tmp,upper},
	{dimension, feature, blockLength, lower, upper, global, thickness, minMax} = OptionValue[{OriginalDimensions,Feature, MovingAverageBlockLength, LowerQuantile, UpperQuantile, GlobalBounds, LineThickness, MinMax}];
	opts1=Sequence@@FilterRules[{opts},First/@Options[eSelectTracks]];
	opts2=Sequence@@FilterRules[{opts},First/@Options[ToSpherical]];
	If[global,
		Switch[feature,
			"Speed",
			tmp = eSelectTracks[tracks,Sequence@@DeleteCases[{opts1},x_/;MemberQ[{From,To,MinimalLength},First@x]]];
			tmp = ToSpherical[#[[2]],opts2]&/@tmp;
			tmp = MovingAverage[#[[All,1]],blockLength]&/@tmp;
			tmp = Prepend[#,0]&/@Table[MapThread[eGreatCircleDistance[#1,#2]&,{Drop[tmp[[i]],1],Drop[tmp[[i]],-1]}],{i,1,Length@tmp}];
			{min,max}={Quantile[#,lower],Quantile[#,upper]}&@Flatten@tmp,
			"Time",
			tmp = eSelectTracks[tracks,Sequence@@DeleteCases[{opts1},x_/;MemberQ[{From,To},First@x]]];
			{min,max}={0,Max[Flatten@tmp[[All,2,All,2]]]}
		];
		opts3=DeleteCases[{opts},x_/;MemberQ[{From,To,MinMax},First@x]];
		Table[FishyPlot[tracks,Sequence@@Join[{Rule[From,i],Rule[To,i],Rule[MinMax,{min,max}]},{opts3}]],{i,range}],
		Table[FishyPlot[tracks,opts],{i,range}]
	]
]
FishyPlot[tracks:{Rule[_Integer,{{{_?NumericQ,_?NumericQ},_Integer}..}]..},opts:OptionsPattern[]]:=Block[
	{average,blockLength,colors,dimension,directness,feature,global,lines,orientation,lower,max,min,minMax,opts1,opts2,opts3,opts4,opts5,polar,scale,selected,speeds,thickness,tmp,upper},
	{dimension, feature, blockLength, lower, upper, global, thickness, minMax} = OptionValue[{OriginalDimensions,Feature, MovingAverageBlockLength, LowerQuantile, UpperQuantile, GlobalBounds, LineThickness, MinMax}];
	opts1=Sequence@@FilterRules[{opts},First/@Options[eSelectTracks]];
	opts2=Sequence@@FilterRules[{opts},First/@Options[ToSpherical]];
	opts3=Sequence@@FilterRules[{opts},First/@Options[Graphics]];
	opts4=Sequence@@FilterRules[{opts},First/@Options[Image]];
	opts5=Sequence@@FilterRules[{opts},First/@Options[color]];
	selected = eSelectTracks[tracks,opts1];
	Switch[feature,
		"Directness",
		polar = ToSpherical[#[[2]],opts2]&/@selected;
		directness=1-Map[(eGreatCircleDistance[First@#,Last@#])/Total[eGreatCircleDistance[#]]&,polar[[All,All,1]]];
		{min,max}={0,1};
		lines=MapThread[thinkenedLine[#1,ConstantArray[color[#2,min,max,opts5],{Length@#1}],thickness]&,{selected[[All,2,All,1]],directness}],
		"MidlineOrientation",
		orientation=Pi/2-Abs[ArcCos[Normalize[Last@#-First@#].{1,0}]-Pi/2]&/@selected[[All,2,All,1]];
		lines=MapThread[thinkenedLine[#1,ConstantArray[color[#2,0,Pi/2,opts5],{Length@#1}],thickness]&,{selected[[All,2,All,1]],orientation}],
		"Orientation",
		average = MovingAverage[#,blockLength]&/@selected[[All,2,All,1]];
		polar=Map[eToPolarCS[#]&,Differences/@average,{2}];
		colors=Map[Hue[#]&,Map[{Mod[-.5Last@#/Pi,1],.9,.9}&,polar,{2}],{2}];
		colors=Map[ColorConvert[#,"RGB"]&,Prepend[#,Hue[{0.,0.,0.}]]&/@colors,{2}];
		lines=MapThread[thinkenedLine[#1,#2,thickness]&,{average,colors}],
		"Speed",
		polar = ToSpherical[#[[2]],opts2]&/@selected;
		average = MovingAverage[#[[All,1]],blockLength]&/@polar;
		speeds = Prepend[#,0]&/@Table[MapThread[eGreatCircleDistance[#1,#2]&,{Drop[average[[i]],1],Drop[average[[i]],-1]}],{i,1,Length@average}];
		If[SameQ[minMax,Automatic],
			If[global,
				tmp = eSelectTracks[tracks,Sequence@@DeleteCases[{opts1},x_/;MemberQ[{From,To},First@x]]];
				tmp = ToSpherical[#[[2]],opts2]&/@tmp;
				tmp = MovingAverage[#[[All,1]],blockLength]&/@tmp;
				tmp = Prepend[#,0]&/@Table[MapThread[eGreatCircleDistance[#1,#2]&,{Drop[tmp[[i]],1],Drop[tmp[[i]],-1]}],{i,1,Length@tmp}];
				{min,max}={Quantile[#,lower],Quantile[#,upper]}&@Flatten@tmp,
				{min,max}={Quantile[#,lower],Quantile[#,upper]}&@Flatten@speeds
			],
			{min,max} = minMax
		];
		colors=color[#,min,max,opts5]&/@speeds;
		lines = MapThread[thinkenedLine[#1,#2,thickness]&,{ToMercator/@average,colors}],
		"Time",
		polar = ToSpherical[#[[2]],opts2]&/@selected;
		If[SameQ[minMax,Automatic],
			If[global,
				tmp = eSelectTracks[tracks,Sequence@@DeleteCases[{opts1},x_/;MemberQ[{From,To},First@x]]];
				max=Max[Flatten@tmp[[All,2,All,2]]];
				scale = max/10,
				max=Max[Flatten@selected[[All,2,All,2]]];
				scale = max/10
			],
			{min,max} = minMax;
			scale = max/10
		];
		colors=color[#/scale,0,max/scale,opts5]&/@selected[[All,2]];
		lines=MapThread[thinkenedLine[#1,#2,thickness]&,{selected[[All,2,All,1]],colors}]
	];
	Graphics[lines, opts3]
]

Clear[color]
Options[color]={Scheme->"TemperatureMap"};
color[line:{{_?NumericQ,_?NumericQ,_?NumericQ}..},min_,max_,opts:OptionsPattern[]]:=Block[
	{scheme,colorFunction},
	scheme = OptionValue[Scheme];
	If[MemberQ[ColorData["Gradients"],scheme],
	colorFunction[i_]:=ColorData[scheme][i],
	colorFunction[i_]:=colorData[scheme][i]
	];
	colorFunction[Rescale[Norm[#],{min,max}]]&/@line
]
color[line:{{{_?NumericQ,_?NumericQ},_?NumericQ}..},min_,max_,opts:OptionsPattern[]]:=Block[
	{scheme,colorFunction},
	scheme = OptionValue[Scheme];
	If[MemberQ[ColorData["Gradients"],scheme],
	colorFunction[i_]:=ColorData[scheme][i],
	colorFunction[i_]:=colorData[scheme][i]
	];
	colorFunction[Rescale[Last@#,{min,max}]]&/@line
]
color[speeds:{__?NumericQ},min_,max_,opts:OptionsPattern[]]:=Block[
	{scheme,colorFunction},
	scheme = OptionValue[Scheme];
	If[MemberQ[ColorData["Gradients"],scheme],
	colorFunction[i_]:=ColorData[scheme][i],
	colorFunction[i_]:=colorData[scheme][i]
	];
	colorFunction[Rescale[#,{min,max}]]&/@speeds
]
color[directedness_?NumericQ,min_,max_,opts:OptionsPattern[]]:=Block[
	{scheme,colorFunction},
	scheme = OptionValue[Scheme];
	If[MemberQ[ColorData["Gradients"],scheme],
	colorFunction[i_]:=ColorData[scheme][i],
	colorFunction[i_]:=colorData[scheme][i]
	];
	colorFunction[Rescale[directedness,{min,max}]]
]

Clear[colorData]
colorData[]={"zfNature"}
colorData["zfBlueRed"][t_]:=Blend[{RGBColor[42/255.,76/255., 149/255.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfLightBlueRed"][t_]:=Blend[{RGBColor[162/255.,198/255., 231/255.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfYellowRed"][t_]:=Blend[{RGBColor[241/255.,183/255., 51/255.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfYellowBlue"][t_]:=Blend[{RGBColor[241/255.,183/255., 51/255.],RGBColor[42/255.,76/255., 149/255.]},t]
colorData["zfYellowLightBlue"][t_]:=Blend[{RGBColor[241/255.,183/255., 51/255.],RGBColor[162/255.,198/255., 231/255.]},t]
colorData["zfBlueLightBlueRed"][t_]:=Blend[{RGBColor[42/255.,76/255., 149/255.],RGBColor[162/255.,198/255., 231/255.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfBlueYellowRed"][t_]:=Blend[{RGBColor[42/255.,76/255., 149/255.],RGBColor[241/255.,183/255., 51/255.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfLightBlueYellowRed"][t_]:=Blend[{RGBColor[162/255.,198/255., 231/255.],RGBColor[241/255.,183/255., 51/255.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfBlackBlueLightBlue"][t_]:=Blend[{RGBColor[0.,0.,0.],RGBColor[42/255.,76/255., 149/255.],RGBColor[162/255.,198/255., 231/255.]},t]
colorData["zfBlackBlueLightBlueRed"][t_]:=Blend[{RGBColor[0.,0.,0.],RGBColor[42/255.,76/255., 149/255.],RGBColor[162/255.,198/255., 231/255.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfBlackBlueLightBlueWhite"][t_]:=Blend[{RGBColor[0.,0.,0.],RGBColor[42/255.,76/255., 149/255.],RGBColor[162/255.,198/255., 231/255.],RGBColor[1.,1.,1.]},t]
colorData["zfBlueLightBlueWhite"][t_]:=Blend[{RGBColor[42/255.,76/255., 149/255.],RGBColor[162/255.,198/255., 231/255.],RGBColor[1.,1.,1.]},t]
colorData["zfBlackBlueLightBlueWhiteRed"][t_]:=Blend[{RGBColor[0.,0.,0.],RGBColor[42/255.,76/255., 149/255.],RGBColor[162/255.,198/255., 231/255.],RGBColor[1.,1.,1.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfBlackYellowRed"][t_]:=Blend[{RGBColor[0.,0.,0.],RGBColor[241/255.,183/255., 51/255.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfBlackYellowRedWhite"][t_]:=Blend[{RGBColor[0.,0.,0.],RGBColor[241/255.,183/255., 51/255.],RGBColor[206/255.,47/255., 42/255.],RGBColor[1.,1.,1.]},t]
colorData["zfBlackRedYellowWhite"][t_]:=Blend[{RGBColor[0.,0.,0.],RGBColor[206/255.,47/255., 42/255.],RGBColor[241/255.,183/255., 51/255.],RGBColor[1.,1.,1.]},t]
colorData["zfWhiteYellowRed"][t_]:=Blend[{RGBColor[1.,1.,1.],RGBColor[241/255.,183/255., 51/255.],RGBColor[206/255.,47/255., 42/255.]},t]
colorData["zfWhiteYellowRed"][t_]:=Blend[{RGBColor[1.,1.,1.],RGBColor[241/255.,183/255., 51/255.],RGBColor[206/255.,47/255., 42/255.]},t]

$natureColor=RGBColor[Sequence @@ #] & /@ Import[FileNameJoin[{$UserBaseDirectory, "Applications/Eidomatica/color_maps/nature_color.csv"}]];
colorData["zfNature"][t_]:=Blend[$natureColor,t]

Clear[DeviationPlot];
Options[DeviationPlot] := Join[DeleteCases[Options[ListLinePlot],PlotStyle->_],{Color -> Blue, FillingOpacity -> .2, NormalizeRange->False, LineStyle->Automatic, DeviationScaling->1.}];
DeviationPlot::um="Unknown method use \"Mean\", \"StandardError\", \"Confidence\" or \"Median\"";
DeviationPlot[samples:{{__?NumericQ}..}, opts:OptionsPattern[], type_String] :=Block[
	{color, fillingOpacity, normalize, lineStyle, scaling, otherOpts, duplicate, plotStyle, center, deviation1, deviation2},
    {color, fillingOpacity,normalize, lineStyle, scaling} = OptionValue[{Color, FillingOpacity, NormalizeRange, LineStyle, DeviationScaling}];
    otherOpts=FilterRules[{opts},First/@DeleteCases[Options[ListLinePlot],PlotStyle->_]];
		duplicate[x_]:=Sequence@@{x,x};
    
    Switch[type,
    	"Mean",
			{center, deviation1, deviation2} = Transpose[{Mean@#, duplicate[StandardDeviation@#*scaling]} & /@ samples],
      "StandardError",
			{center, deviation1, deviation2} = Transpose[{Mean@#, duplicate[StandardDeviation@#/Sqrt[Length@#]*scaling]} & /@ samples],
      "Confidence",
      {center, deviation1, deviation2} = Transpose[{Mean@#, duplicate[(StandardDeviation@#/Sqrt[Length@#])*1.96]} & /@ samples],
			"Median",
			{center, deviation1, deviation2} = Transpose[{Median@#, Sequence@@(Quantile[#,{.05,.95}]*scaling)} & /@ samples],
			_,
			Message[DeviationPlot::um];
			Abort[]
    ];
		If[lineStyle===Automatic,
      plotStyle={color, {color,Thin,Opacity[.7]}, {color,Thin,Opacity[.7]}},
      plotStyle={{lineStyle,color}, {lineStyle,color,Thin,Opacity[.7]}, {lineStyle,color,Thin,Opacity[.7]}}
		];
    ListLinePlot[
    	If[normalize,
    		Map[Transpose[{Table[i,{i,0,1,1/(Length@#-1)}],#}]&,{center, center - deviation1, center + deviation2}],
    		{center, center - deviation1, center + deviation2}
    	], PlotStyle ->plotStyle,
         Filling -> {1 -> {{2}, Directive[{color, Opacity[fillingOpacity]}]}, 3 -> {{1}, Directive[{color, Opacity[fillingOpacity]}]}},otherOpts]
]
Clear[MedianDeviationPlot];
Options[MedianDeviationPlot]:=Options[DeviationPlot];
MedianDeviationPlot[samples:{{__?NumericQ}..}, opts:OptionsPattern[]]:=DeviationPlot[samples,Sequence@@Join[{opts},
  Options[MedianDeviationPlot]],"Median"]

Clear[StandardDeviationPlot];
Options[StandardDeviationPlot]:=Options[DeviationPlot];
StandardDeviationPlot[samples:{{__?NumericQ}..}, opts:OptionsPattern[]]:=DeviationPlot[samples,Sequence@@Join[{opts},
  Options[StandardDeviationPlot]],"Mean"]

Clean[StandardErrorPlot];
Options[StandardErrorPlot]:=Options[DeviationPlot];
StandardErrorPlot[samples:{{__?NumericQ}..}, opts:OptionsPattern[]]:=DeviationPlot[samples,Sequence@@Join[{opts},
  Options[StandardErrorPlot]],"StandardError"]

Clean[ConfidencePlot];
Options[ConfidencePlot]:=Options[DeviationPlot];
ConfidencePlot[samples:{{__?NumericQ}..}, opts:OptionsPattern[]]:=DeviationPlot[samples,Sequence@@Join[{opts},
  Options[ConfidencePlot]],"Confidence"]

  
End[] (* End Private Context *)

EndPackage[]
