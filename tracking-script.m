#!/Applications/Mathematica.app/Contents/MacOS/MathKernel -script

If[Length@$CommandLine != 16,
	Print["Wrong number of arguments!"];
	Abort[]
]

Get["Eidomatica`"];

binPath=$CommandLine[[4]];
flowPath=$CommandLine[[5]];
parentDir=$CommandLine[[6]];
sizeConstraint=ToExpression[$CommandLine[[7;;8]]];
overlap=ToExpression[$CommandLine[[9]]];
interpolated=ToExpression[$CommandLine[[10]]];
constraintType=$CommandLine[[11]];
boundary=$CommandLine[[12]];
appearing=ToExpression[$CommandLine[[13]]];
initialLabelMatrix=If[$CommandLine[[14]] === "None",
 	None,
 	IntegerPart@First@Import[$CommandLine[[14]]]
];
embExport=ToExpression[$CommandLine[[15]]];
time=$CommandLine[[16]];

divisionFile=FileNameJoin[{parentDir,"divisions.m"}];
trackFile=FileNameJoin[{parentDir,"tracks.m"}];
labelPath=FileNameJoin[{parentDir,"label"}];

CheckAbort[
	eFluidTracking[binPath,flowPath,labelPath,BoundaryCondition->boundary,SizeConstraint->sizeConstraint,Overlap->overlap,Interpolated->interpolated,ConstraintType->constraintType,FileNameDivision->divisionFile,AbortWhenOverride->False,IncludeAppearingCells->appearing,InitialLabelMatrix->initialLabelMatrix],
	Quit[1]
];
If[flowPath==="",
	vfInfo="Tracking done without displacement fields.",
	vfInfo=eVectorFieldInfo@eBinaryImport[First@FileNames["*.dat",{flowPath}]];
	vfInfo=Complement[vfInfo,FilterRules[vfInfo,{"CoordinateSystem","ActualEndTime","ActualMismatchError"}]]
];
trackingInfo={Images->binPath,BoundaryCondition->boundary,SizeConstraint->sizeConstraint,Overlap->overlap,Interpolated->interpolated,ConstraintType->constraintType};
Export[FileNameJoin[{parentDir,time<>"-tracking.log"}],{"RegistationParameters"->vfInfo,"TrackingParameters"->trackingInfo},"Text"];
Export[trackFile,eExtractTracksFromLabels[FileNames["*.mat",{labelPath}]]];
If[embExport,
	tracks=Import[trackFile];
	csv=Map[{First@#,Sequence@@Flatten@Last@#}&,tracks];
	Export[FileNameJoin[{parentDir,"./tracks.csv"}],csv];
	divisions=Import[divisionFile];
	Export[FileNameJoin[{parentDir,"./divisions.csv"}],{First@#,Last@#}&/@Flatten@divisions]
]