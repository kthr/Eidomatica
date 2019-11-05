(* Mathematica Package *)

BeginPackage["Eidomatica`Import`"]
Needs["Eidomatica`"]
(* Exported symbols added here with SymbolName::usage *)  
eImportHDF5::usage="eImportHDF5[fileName, what, where, depth]"

Begin["`Private`"] (* Begin Private Context *) 

Clear[eImportHDF5]
llHDF5Import::error="`1`"
eImportHDF5::what="Property `1` is not valid, it has to be one of the following \"Annotations\", \"Data\", \"Names\"."
eImportHDF5[fileName_String, what_String, depth_Integer:0]:=eImportHDF5[fileName, what, {"/"}, depth]
eImportHDF5[fileName_String, what_String, where_String, depth_Integer:0]:=eImportHDF5[fileName, what, {where}, depth]
eImportHDF5[fileName_String, what_String, where:{__String}, depth_Integer:0]:=Block[
	{},
	Switch[what,
		"Annotations",
		llHDF5Import[fileName, 0, StringReplace[#, RegularExpression["/$"] -> ""]&/@where, depth],
		"Data",
		llHDF5Import[fileName, 1, StringReplace[#, RegularExpression["/$"] -> ""]&/@where, depth],
		"Names",
		llHDF5Import[fileName, 2, StringReplace[#, RegularExpression["/$"] -> ""]&/@where, depth],
		_,
		Message[eImportHDF5::what, First@what]
	]
]

End[] (* End Private Context *)

EndPackage[]