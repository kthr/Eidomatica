(* Mathematica Package *)

BeginPackage["Eidomatica`TrackingUtilities`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Utilities`"]
Needs["Eidomatica`Internal`"]
(* Exported symbols added here with SymbolName::usage *)

(*fluid tracking utilities*)
ePackLabel32::usage="ePackLabel32[cloneID,cellID]"
eUnpackLabel32::usage="eUnpackLabel32[id]"
ePackLabel64::usage="ePackLabel64[cloneID,cellID]"
eUnpackLabel64::usage="eUnpackLabel64[id]"
eRelabel::usage="eRelabel[labelMatrix]"
eDeleteObjects::usage="eDeleteObjects[labelMatrix, min, max] or eDeleteObjects[image, min, max]"
	ConstraintType::usage="ConstraintType set the type of the contraint function to either 'Standard' where the size constraint is equal at every image position or to 'Mercator' where the size is adjusted according to distortion in the Mercator projection."
eUnpackLabelMatrix::usage="eUnpackLabelMatrix[labelMatrix]"
	Target::usage="Target is an option for eUnpackLabelMatrix, which is either \"CloneIDs\" or \"CellIDs\"."
eExtractTracksFromLabels::usage = "eExtractTracksFromLabels[filenames] given a list of label file names tracks without division etc. are extracted."


(*options found in several functions*)
LabelSize::usage="LabelSize defines the label size, which is either \"32Bit\" or \"64Bit\"."


Begin["`Private`"] (* Begin Private Context *) 
$searchPattern = {
	"Images" -> {"*.tif", "*.tiff", "*.png", "*.jpg", "*.jpeg"}, 
	"RegisteredImages" -> {"*.tif", "*.tiff", "*.png", "*.jpg", "*.jpeg"},
	"Flows" -> {"*.dat"},
	"MetaFiles" -> {"*.m"}, 
	"LabelImages" -> {"*.m", "*.mat", "*.tif", "*.tiff", "*.png", "*.jpg", "*.jpeg"}
};

Clear[getFileNames]
getFileNames[path_String, objective_String] := Block[
  	{},
  	FileNameTake /@ FileNames[objective /. $searchPattern, {path}, IgnoreCase -> True]
]

Clear[eGetFileNames];
eGetFileNames::nomemb = nomemb;
eGetFileNames[project_TrackingProject,objective_String]:=Module[
	{data,path,fileNames},
	MemberCheck[eGetFileNames,"objective",objective,{"Images","RegisteredImages","Flows","LabelImages","MetaFiles"}];
	data=Level[project,1];
	{path,fileNames}=objective/.data;
	FileNameJoin[{path,#}]&/@fileNames
]

(*utilities for fluid tracking*)
Clear[ePackLabel64];
ePackLabel64[cloneID_Integer /; cloneID < 4294967295, cellLabel_Integer /; cellLabel < 4294967295] := 
	BitShiftLeft[cloneID, 32] + cellLabel;

Clear[eUnpackLabel64];
eUnpackLabel64[label_Integer] := {BitShiftRight[BitAnd[label, 18446744069414584320], 30], BitAnd[label, 4294967295]}

Clear[ePackLabel32]
ePackLabel32[cloneID_Integer /; cloneID < 65536, cellLabel_Integer /; cellLabel < 65536] := 
	BitShiftLeft[cloneID, 16] + cellLabel;

Clear[eUnpackLabel32]
eUnpackLabel32[label_Integer] := {BitShiftRight[BitAnd[label, 4294901760], 16], BitAnd[label, 65535]}

Clear[eDeleteObjects];
eDeleteObjects::nonum = nonum;
eDeleteObjects::nomemb = nomemb;
Options[eDeleteObjects] = {SizeConstraint -> None, ConstraintType->"Standard"};
eDeleteObjects::nonum = nonum;
eDeleteObjects::invsc="Invalid value for option SizeConstraint. SizeConstraint has to be of the form {minimalSize,maximalSize}, with the following pattern None | {None, None} | {None, _Integer} | {_Integer, None} | {_Integer, _Integer}."
eDeleteObjects[labelMatrix:{{__Integer} ..}, min_Integer, opts:OptionsPattern[]]:=eDeleteObjects[labelMatrix, Sequence@@Join[{SizeConstraint->{min,None}},{"ConstraintType"->OptionValue["ConstraintType"]}]]
eDeleteObjects[labelMatrix:{{__Integer} ..}, min_Integer,max_Integer, opts:OptionsPattern[]]:=eDeleteObjects[labelMatrix, Sequence@@Join[{SizeConstraint->{min,max}},{"ConstraintType"->OptionValue["ConstraintType"]}]]
eDeleteObjects[image_Image, min_Integer, opts:OptionsPattern[]]:=eDeleteObjects[MorphologicalComponents[image], Sequence@@Join[{SizeConstraint->{min,None}},{"ConstraintType"->OptionValue["ConstraintType"]}]]
eDeleteObjects[image_Image, min_Integer, max_Integer, opts:OptionsPattern[]]:=eDeleteObjects[MorphologicalComponents[image], Sequence@@Join[{SizeConstraint->{min,max}},{"ConstraintType"->OptionValue["ConstraintType"]}]]
eDeleteObjects[labelMatrix:{{__Integer} ..}, opts:OptionsPattern[]] := Block[
	{size,minSize,maxSize, type, xdim, ydim, minSizef,maxSizef,components,selected}, 
	{size,type} = OptionValue[{SizeConstraint,ConstraintType}];
	MemberCheck[eDeleteObjects,"ConstraintType", type, {"Standard", "Mercator"}];
	If[MatchQ[size, None | {None, None} | {None, _Integer} | {_Integer, None} | {_Integer, _Integer}],
		Switch[size,
			None|{None,None},
			minSize=0;
			maxSize=Infinity,
			{None,_Integer},
			minSize=0;
			maxSize=Last@size,
			{_Integer,None},
			minSize=First@size;
			maxSize=Infinity,
			{_Integer,_Integer},
			minSize=First@size;
			maxSize=Last@size
		],
		Message[eDeleteObjects::invsc];
		Abort[]
	];
				
  	{xdim, ydim} = Dimensions@labelMatrix;
  	If[type === "Standard",
   		minSizef[{x_, y_}] = minSize;
   		maxSizef[{x_, y_}] = maxSize,
   		minSizef[{x_, y_}] := minSize*Sec[Rescale[y, {0, ydim}, {-Pi/2, Pi/2}]]^2;
   		maxSizef[{x_, y_}] := maxSize*Sec[Rescale[y, {0, ydim}, {-Pi/2, Pi/2}]]^2
   	];
   	If[MatchQ[size,None|{None,None}],
   		labelMatrix,
   		components=ComponentMeasurements[labelMatrix, {"Count", "Mask", "Centroid"}];
   		selected=Total[Cases[components, Rule[_,{count_,mask_,centroid_}]/; (minSizef[centroid] < count < maxSizef[centroid])->mask]];
  		labelMatrix*(Normal@selected)
   	]
]

Clear[eRelabel];
eRelabel::nomemb = nomemb;
Options[eRelabel] = Union@Flatten@{LabelSize -> "32Bit",Options[eDeleteObjects]};
eRelabel[labelMatrix : {{__Integer} .. }, opts : OptionsPattern[]] := Block[
  	{bitDepth,size,opts1,pack,select,tmp},
  	{bitDepth,size} = OptionValue[{LabelSize,SizeConstraint}];
  	opts1 = Sequence@@FilterRules[{opts},First/@Options[eDeleteObjects]];
  	MemberCheck[eRelabel,"LabelSize",bitDepth,{"32Bit","64Bit"}];
  	If[bitDepth == "32Bit",
   		pack = ePackLabel32,
   		pack = ePackLabel64
   	];
   	tmp=If[MatchQ[size,None|{None,None}],
   		ComponentMeasurements[labelMatrix, "Mask"],
   		ComponentMeasurements[eDeleteObjects[labelMatrix,opts1],"Mask"]
   	];
   	If[SameQ[tmp,{}],
   		ConstantArray[0,Dimensions@labelMatrix],	
   		Normal@Total@(MapIndexed[Last@#1*pack[First@#2, 1] &, tmp])
   	]
]

Clear[eUnpackLabelMatrix];
eUnpackLabelMatrix::nomemb = nomemb;
Options[eUnpackLabelMatrix] = {LabelSize -> "32Bit",Target->"CloneIDs"};
eUnpackLabelMatrix[labelMatrix : {{__Integer} ..}, opts : OptionsPattern[]] := Block[
	{bitDepth,target,unpack, labels, rules},
	{bitDepth,target} = OptionValue[{LabelSize,Target}];
	MemberCheck[eUnpackLabelMatrix,"LabelSize",bitDepth,{"32Bit","64Bit"}];
	MemberCheck[eUnpackLabelMatrix,"Target",target,{"CloneIDs","CellIDs"}];
  	If[bitDepth == "32Bit",
   		unpack = eUnpackLabel32,
   		unpack = eUnpackLabel64
   	];
  	labels = DeleteCases[Union@Flatten[labelMatrix], 0];
  	If[target=="CloneIDs",
  		rules = Dispatch[#->First[unpack[#]]&/@labels],
  		rules = Dispatch[#->Last[unpack[#]]&/@labels]
  	];
  	Replace[labelMatrix, rules, {2}]
]

Clear[eExtractTracksFromLabels]
eExtractTracksFromLabels[labels:{__String}]:=Block[
	{ids,tracks},
	DistributeDefinitions[labels];
	ids=ParallelTable[First@# -> {Last@#, i} & /@ ComponentMeasurements[IntegerPart@First@Import[labels[[i]]],"Centroid"],{i,1,Length@labels}];
	tracks=First@First@#->(Last/@#)&/@GatherBy[Flatten@ids,First@#&]
]

End[] (* End Private Context *)

EndPackage[]