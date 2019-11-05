(* Mathematica Package *)

BeginPackage["Eidomatica`Internal`"]
(* Exported symbols added here with SymbolName::usage *)
IntegerCheck::usage = "IntegerCheck tests if given value is an integer, if not the message \"noint\" corresponding to the given module name is thrown and further evaluation is aborted."
IsBoolean::usage = "IsBoolean tests if given value is a boolean, if not the message \"nobool\" corresponding to the given module name is thrown and further evaluation is aborted."
IsPositive::usage = "IsPositive tests if given value is positive, if not the message \"nopos\" corresponding to the given module name is thrown and further evaluation is aborted."
IsVectorField::usage = "IsVectorField tests if given value is of type VectorField[], if not the message \"novf\" corresponding to the given module name is thrown and further evaluation is aborted."
NumberCheck::usage = "NumberCheck tests if given value has a numeric value, if not the message \"nonum\" corresponding to the given module name is thrown and further evaluation is aborted."
MemberCheck::usage = "IntegerCheck tests if given value is a member of the given elements, if not the message \"nomem\" corresponding to the given module name is thrown and further evaluation is aborted."

Begin["`Private`"] (* Begin Private Context *)
IntegerCheck[moduleName_Symbol,optionName_String,value_]:= If[IntegerQ[value],
	True,
	Message[moduleName::noint,optionName];
	Abort[]
]
IsBoolean[moduleName_Symbol,optionName_String,value_]:= If[MemberQ[{True,False},value],
	True,
	Message[moduleName::nobool,optionName];
	Abort[]
]
IsPositive[moduleName_Symbol,optionName_String,value_]:= If[value>0,
	True,
	Message[moduleName::nopos,optionName];
	Abort[]
]
IsVectorField[moduleName_Symbol,optionName_String,value_]:= If[Head[value]===VectorField,
	True,
	Message[moduleName::novf,optionName];
	Abort[]
]
NumberCheck[moduleName_Symbol,optionName_String,value_]:= If[NumericQ[value],
	True,
	Message[moduleName::nonum,optionName];
	Abort[]
]
MemberCheck[moduleName_Symbol,optionName_String,value_,elements_List]:= If[MemberQ[elements,value],
	True,
	Message[moduleName::nomemb,optionName,
		StringJoin[Join["\"" <> # <> "\"," & /@ Drop[elements, -1], {"\"" <> Last@elements <> "\""}]]
   	];
	Abort[]
]
End[] (* End Private Context *)

noint  = "Illegal value for option '`1`'. Value has to be an integer.";
nobool = "Illegal value for option '`1`'. Value has to be a boolean.";
nopos = "Illegal value for option '`1`'. Value has to be positive.";
novf = "Illegal value for option '`1`'. Value has to be of type VectorField[].";
nonum = "Illegal value for option '`1`'. Value has to be numeric.";
nomemb = "Illegal value for option '`1`'. Value has to be one of the following `2`.";
override = "Process stopped to prevent file override.";

EndPackage[]