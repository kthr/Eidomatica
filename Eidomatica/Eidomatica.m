(* Mathematica Package *)

(* Created by the Wolfram Workbench Oct 27, 2011 *)

BeginPackage["Eidomatica`"]
(* Exported symbols added here with SymbolName::usage *)
AbortWhenOverride::usage="AbortWhenOverride if TRUE the function given this option aborts when file would be overriden."
Iterations::usage = "Iterations is an option for various functions specifying the maximal number of iterations done."
StepSize::usage  = "StepSize is an option for various functions specifying the step size done in each iteration step."
ReturnAll::usage = "ReturnAll is an option for various functions specifying if only last or all results of the iteration are returned."
InterpolationMethod::usage = "InterpolationMethod is an option for eVectorFieldPlot[field] and eRYBtoRGB[color] specifying with which method the RYB color is converted to RGB."
ReturnType::usage="ReturnType is an option for eDisplaceByVectorField[] and CreateMask[] which specifies if the return type is an image or a matrix, possible values are \"Matrix\" or \"Image\"."
SizeConstraint::usage="SizeConstraint is an option for eDeleteObjects[], eCreateMetaData[], eRelabel[] and eFluidTracking[] deleting all objects smaller than the given size."
eVersion::usage="eVersion gives the current version of the Eidomatica package."
Radius::usage = "Radius is an option for ToPolar specifying the radius of the computed polar coordinates.
Radius is an option for eSteinbrueckerOpticalFlow[] specifying the search radius (in pixels) of the complete search."

$libraryEidomatica::usage="$libraryEidomatica library containing various functions."
$librariesFound::usage="$librariesFound True if libEidomatica is found else False."
llAlphaShapes::usage="llAlphaShapes do not use this function!"
llBoundingVolumes::usage="llBoundingVolumes do not use this function!"
llVectorFieldPlot::usage="llVectorFieldPlot do not use this function!"
llVersion::usage="llVersion do not use this function!"
llDensity::usage="llDensity do not use this function!"
llFeatureMap::usage="llFeatureMap do not use this function!"
llHDF5Import::usage"llHDF5Import do not use this function!"

eGraphcut::usage = "eGraphcut[image] returns a binary image."
eMultiLabelGraphcut::usage = "eMultiLabelGraphcut[image, labelImage] returns a multi label segmentation."
eAdaptiveMultiLabelGraphcut::usage = "eAdaptiveMultiLabelGraphcut[image, labelImage] returns a multi label segmentation, fore- and background intensity distributions are estimated automatically."
	Mu::usage = "Mu is an option for eGraphcut."

(*common graphcut parameters*)
Lambda::usage = "Lambda is an option for eGraphcut and eMultiLabelGraphcut."
C0::usage="C0 the mean value for the background intensity (between 0 and 1)."
C1::usage="C1 the mean value for the foreground intensity (between 0 and 1)"
Sigma::usage="Sigma is a parameter for eGraphcut."

eComponentMeasurements::usage="eComponentMeasurements returns the connected components of an binary image."

Begin["`Private`"]
(* Implementation of the package *)

(*****************************************************
 Initial Preparations
   a) load the opencv
   b) add the path to the examples to the system
   c) kill the binary when needed 
*)
(*$mlbinpath = {"Eidomatica", "Binaries"};
$mlbinary = "opencv"

$opencvLink=FileNames[$mlbinary, ToFileName[Prepend[$mlbinpath, #]] & /@ $Path];
opencv::nolink="Can't find \"opencv\". Some functions may not work.";

If[!(Or @@ (StringMatchQ[StringReplace[First[#],"\""->""], "*opencv*"] & /@ Links[])),
  If[{}===$opencvLink,
    Message[opencv::nolink],
    $opencvLink=Install[First[$opencvLink]]
  ]
]*)

lib::nolink="Can't find \"libEidomatica\" at `1`. Some functions may work."
If[StringQ[$libEidomatica],
	Print["Unloading library functions, before reloading them!"];
	LibraryUnload[$libEidomatica]
];
library=FileNameJoin[{$UserBaseDirectory, "Applications/Eidomatica/LibraryResources", $SystemID, "libEidomatica"}];
$libEidomatica=FindLibrary[library];
$librariesFound=If[$libEidomatica === $Failed,
	Message[lib::nolink, library];
 	False,
 	(*llVectorFieldPlot = LibraryFunctionLoad[$libEidomatica,"llVectorFieldPlot", {{Real, 3, "Constant"}}, {Real, 3}];*)
 	llVersion = LibraryFunctionLoad[$libEidomatica,"llVersion", {}, "UTF8String"];
  llAlphaShapes = LibraryFunctionLoad[$libEidomatica, "llAlphaShape", {{Real, 1, "Constant"}, Real, "Boolean"}, {Real, 1}];
  llBoundingVolumes = LibraryFunctionLoad[$libEidomatica, "llBoundingVolumes", {{Real, 1, "Constant"}, Integer, "Boolean"}, {Real, 1}];
 	llDensity = LibraryFunctionLoad[$libEidomatica, "llDensity", {{Real, 1, "Constant"}, {Integer, 1, "Constant"}, {Integer, 1, "Constant"}, Real, Real, Real, Integer, Real, Real}, {Real,2}];
  llFeatureMap = LibraryFunctionLoad[$libEidomatica, "llFeatureMap", {{Real, 1, "Constant"}, {Real, 1, "Constant"}, {Integer, 1, "Constant"}, {Integer, 1, "Constant"}, Real, Real, Real, Integer, Real, Real}, {Real,2}];
  llMultiLabelGraphcut=LibraryFunctionLoad[$libEidomatica,"llMultiLabelGraphcut",{{Integer,2,"Constant"},Integer,{Integer,2,"Constant"},Integer,Integer,Real,Real,Real, Real, Real},{Integer,2}];
 	llAdaptiveMultiLabelGraphcut=LibraryFunctionLoad[$libEidomatica,"llAdaptiveMultiLabelGraphcut",{{Integer,2,"Constant"},Integer,{Integer,2,"Constant"},Integer,Integer,Real, Real,Real},{Integer,2}];
 	llHDF5Import=LibraryFunctionLoad[$libEidomatica,"llHDF5Import", LinkObject, LinkObject];
 	(*llComponents=LibraryFunctionLoad[$libEidomatica,"llComponentMeasurements", LinkObject, LinkObject];*)
 	True
];

eVersion[]:=Block[
	{versionFile, eidomaticaVersion = "Unknown", eidomaticaLibVersion = "Unknown"},
	versionFile = FindFile["Eidomatica/eVersion.txt"];
	If[!SameQ[versionFile,$Failed],
		eidomaticaVersion = ToString@Import[versionFile]
	];
	If[$librariesFound,
		eidomaticaLibVersion = ToString[llVersion[]]
	];
	Print["Revisions: {Eidomatica Package: " <> eidomaticaVersion <> ", libEidomatica: " <> eidomaticaLibVersion <> "}"];
]

Clear[eGraphcut]
eGraphcut::nomultich = "Multichannel images are currently not supported converting image to grayscale.";
eGraphcut::fri = "Image of ImageType 'Real', bit depth can't be inferred. Bit depth is set to 8bit";
Options[eGraphcut] = {Lambda->1.,Sigma->.2, C0->0., C1->1.};
eGraphcut[(image_Image3D|image_Image),opts:OptionsPattern[]]:=Block[
	{converted,bitDepth,data,c0,c1,lambda,sigma,binary},
	If[ImageChannels[image]>1,
		Message[eGraphcut::nomultich];
		converted=ColorConvert[image,"Grayscale"];
		bitDepth=ImageType@converted;
		If[SameQ[bitDepth,"Real"],
			Message[eGraphcut::fri];
			bitDepth="Byte"
		];
		data=ImageData[converted,bitDepth],
		bitDepth=ImageType@image;
		If[SameQ[bitDepth,"Real"],
			Message[eGraphcut::fri];
			bitDepth="Byte";
		];
		data=ImageData[image,bitDepth]
	];
	{c0,c1,lambda,sigma} = OptionValue[{C0, C1, Lambda,Sigma}];
	llGraphCut=If[Depth@data==3,
		If[Head@c0 === Head@c1 === List,
			LibraryFunctionLoad[$libEidomatica,"llGraphcutDistribution",{{Integer,2,"Constant"},Integer,{Real,1,"Constant"},{Real,1,"Constant"},Real,Real},{Integer,2}],
			LibraryFunctionLoad[$libEidomatica,"llGraphCut",{{Integer,2,"Constant"},Integer,Real,Real,Real,Real},{Integer,2}]	
		],
		If[Head@c0 === Head@c1 === List,
			LibraryFunctionLoad[$libEidomatica,"llGraphcutDistribution",{{Integer,3,"Constant"},Integer,{Real,1,"Constant"},{Real,1,"Constant"},Real,Real},{Integer,3}],
	    	LibraryFunctionLoad[$libEidomatica,"llGraphCut",{{Integer,3,"Constant"},Integer,Real,Real,Real,Real},{Integer,3}]
	    ]
	];
	binary=llGraphCut[data,Switch[bitDepth,"Byte",8,"Bit16",16],c0,c1,lambda,sigma];
	LibraryFunctionUnload[llGraphCut];
	If[Depth@data==3,
		Image@binary,
		Image3D@binary
	]
]

Clear[eMultiLabelGraphcut]
eMultiLabelGraphcut::nomultich = "Multichannel images are currently not supported converting image to grayscale.";
eMultiLabelGraphcut::invli = "Invalid label image! Label images have to be single channel images."
eMultiLabelGraphcut::fri = "Image of ImageType 'Real', bit depth can't be inferred. Bit depth is set to 8bit";
eMultiLabelGraphcut::ivd = "Dimensions of 'image' and 'labelImage' do not fit!";
Options[eMultiLabelGraphcut] = {C0->0., C1->1., Lambda->.1, Sigma->.1, Mu->1.};
eMultiLabelGraphcut[image_Image, labelImage_Image,opts:OptionsPattern[]]:=Block[
	{parameters, converted, bitDepth, imageData, labelData, numLabels, labeled},
	parameters = OptionValue[{C0, C1, Lambda, Sigma, Mu}];
	If[!SameQ[ImageDimensions@image,ImageDimensions@labelImage],
		Message[eMultiLabelGraphcut::ivd];
		Abort[]
	];
	If[ImageChannels[image]>1,
		Message[eMultiLabelGraphcut::nomultich];
		converted=ColorConvert[image,"Grayscale"];
		bitDepth=ImageType@converted;
		If[SameQ[bitDepth,"Real"],
			Message[eMultiLabelGraphcut::fri];
			bitDepth="Byte"
		];
		imageData=ImageData[converted,bitDepth],
		bitDepth=ImageType@image;
		If[SameQ[bitDepth,"Real"],
			Message[eMultiLabelGraphcut::fri];
			bitDepth="Byte";
		];
		imageData=ImageData[image,bitDepth]
	];
	If[ImageChannels[labelImage]>1,
		Message[eMultiLabelGraphcut::invli];
		Abort[],
		labelData = ImageData[labelImage,"Bit16"]
	];
	numLabels=Length@Union@Flatten[labelData]+1;
	labeled=llMultiLabelGraphcut[imageData,Switch[bitDepth,"Byte",8,"Bit16",16],labelData, 16, numLabels, Sequence@@parameters];
    Image[labeled,"Bit16"]
]

Clear[eAdaptiveMultiLabelGraphcut]
eAdaptiveMultiLabelGraphcut::nomultich = "Multichannel images are currently not supported converting image to grayscale.";
eAdaptiveMultiLabelGraphcut::invli = "Invalid label image! Label images have to be single channel images."
eAdaptiveMultiLabelGraphcut::fri = "Image of ImageType 'Real', bit depth can't be inferred. Bit depth is set to 8bit";
eAdaptiveMultiLabelGraphcut::ivd = "Dimensions of 'image' and 'labelImage' do not fit!";
Options[eAdaptiveMultiLabelGraphcut] = {Lambda->.9, Sigma->.1, Mu->1.};
eAdaptiveMultiLabelGraphcut[image_Image, labelImage_Image,opts:OptionsPattern[]]:=Block[
	{parameters, converted, bitDepth, imageData, labelData, numLabels, labeled},
	parameters = OptionValue[{Lambda, Sigma, Mu}];
	If[!SameQ[ImageDimensions@image,ImageDimensions@labelImage],
		Message[eMultiLabelGraphcut::ivd];
		Abort[]
	];
	If[ImageChannels[image]>1,
		Message[eAdaptiveMultiLabelGraphcut::nomultich];
		converted=ColorConvert[image,"Grayscale"];
		bitDepth=ImageType@converted;
		If[SameQ[bitDepth,"Real"],
			Message[eAdaptiveMultiLabelGraphcut::fri];
			bitDepth="Byte"
		];
		imageData=ImageData[converted,bitDepth],
		bitDepth=ImageType@image;
		If[SameQ[bitDepth,"Real"],
			Message[eAdaptiveMultiLabelGraphcut::fri];
			bitDepth="Byte";
		];
		imageData=ImageData[image,bitDepth]
	];
	If[ImageChannels[labelImage]>1,
		Message[eAdaptiveMultiLabelGraphcut::invli];
		Abort[],
		labelData = ImageData[labelImage,"Bit16"]
	];
	numLabels=Length@Union@Flatten[labelData]+1;
	labeled=llAdaptiveMultiLabelGraphcut[imageData,Switch[bitDepth,"Byte",8,"Bit16",16],labelData, 16, numLabels, Sequence@@parameters];
    Image[labeled,"Bit16"]
]

Clear[eComponentMeasurements]
eComponentMeasurements::invprop = "`1` is not a known image segment property."
eComponentMeasurements[something_, measure_String]:=eComponentMeasurements[something, {measure}]
eComponentMeasurements[labelMatrix:{{__Integer}..}, measures:{__String}]:=cm[Null,labelMatrix,measures];
eComponentMeasurements[labelMatrix:{{{__Integer}..}..}, measures:{__String}]:=cm[Null,labelMatrix,measures];
eComponentMeasurements[image_Image3D|image_Image,measures:{__String}]:=cm[Null,MorphologicalComponents@image,measures]
eComponentMeasurements[{image_Image, labelMatrix:{{__Integer}..}},measures:{__String},opts:OptionsPattern[]]:=cm[image,labelMatrix,measures];
eComponentMeasurements[{image_Image3D|image_Image, labelMatrix_},measures:{__String},opts:OptionsPattern[]]:=cm[image,labelMatrix,measures];
cm[image_, labelMatrix_,measures_]:=Block[
	{ids,masks,dimensions,tmp,convert,intensities,res},
	Map[If[!MemberQ[{"Centroid","Count","Mask","MeanIntensity","MedianIntensity","Size","StandardDeviationIntensity"},#],
		Message[eComponentMeasurements::invprop, #];
		Abort[]
	]&, measures];
	dimensions=Dimensions@labelMatrix;
	tmp=Join[{0},Rest[Reverse@dimensions]];
	{ids,masks}=Transpose[{First@#,Reverse/@(Last@#)}&/@llComponents[16,1,Reverse@dimensions, Flatten@labelMatrix]];
	convert[x:{{__Integer}..}]:=SparseArray[#->1&/@x,dimensions];
	If[Or@@Map[MemberQ[{"MeanIntensity", "MedianIntensity", "StandardDeviationIntensity"}, #]&, measures],
		If[!SameQ[image,Null],
			intensities=Extract[ImageData[image,"Real32"],#]&/@masks,
			intensities=Extract[labelMatrix,#]&/@masks
		]
	];
	res=Map[
		Switch[#,
			"Centroid",
			Abs[tmp-#]&/@(Reverse/@N[Mean/@masks]),
			"Count",
			Length/@masks,
			"Mask",
			convert/@masks,
			"MeanIntensity",
			Mean/@intensities,
			"MedianIntensity",
			Median/@intensities,
			"StandardDeviationIntensity",
			StandardDeviation/@intensities
		]&,
	measures];
	If[Length@measures==1,
		MapThread[#1->#2&,{ids,First@res}],
		MapThread[#1->#2&,{ids,Transpose@res}]
	]
]

End[]

EndPackage[]

