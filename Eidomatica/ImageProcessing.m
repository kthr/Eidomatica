(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: ImageProcessing *)
(* :Context: ImageProcessing` *)
(* :Author: kthierbach *)
(* :Date: 2017-03-23 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 kthierbach *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["Eidomatica`ImageProcessing`"]
(* Exported symbols added here with SymbolName::usage *)
ImageNormalize::usage="ImageNormalize[image] normalizes intensity values, based on the min and max intensities of the image.
  ImageNormalize[image, {min, max}] normalizes intensity values of the image, based on the given min and max values.
  ImageNormalize[{image1, image2, ...}] normalizes intensity values of all images, based on the min and max intensites of all images.
  ImageNormalize[{image1, image2, ...}, {min, max}] normalizes intensity values of all images,based on the given min and max values."

Begin["`Private`"]

Clear[ImageNormalize]
ImageNormalize[images : {__Image},
  minMax : ({_?NumericQ, _?NumericQ} | _Automatic)] := Block[
  {range},
  If[SameQ[minMax, Automatic],
    range = {Min@#, Max@#} &@Flatten@(ImageData /@ images),
    range = minMax
  ];
  ImageNormalize[#, range] & /@ images;
]
ImageNormalize[(image_Image | image_Image3D), minMax_: Automatic] := Block[
  {type = ImageType[image], range, head = Head[image]},
  If[SameQ[minMax, Automatic],
    range = {Min@#, Max@#} &@Flatten@ImageData@image,
    range = minMax
  ];
  head[head[Rescale[ImageData@image, range], "Real"], type]
]
End[] (* `Private` *)

EndPackage[]