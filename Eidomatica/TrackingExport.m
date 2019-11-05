(* Mathematica Package *)

BeginPackage["Eidomatica`TrackingExport`"]
Needs["Eidomatica`TrackingUtilities`"]
Needs["Eidomatica`Internal`"]
Needs["Eidomatica`Utilities`"]

(* Exported symbols added here with SymbolName::usage 

eExport2XML::usage = "eExportToXML[]"
	AddImagesToTar::usage = "AddImagesToTar"
	DeleteSuperfluousFiles::usage="DeleteSuperfluousFiles is an option for eExport2XML[]. If true, xml meta files and trackXML.xml
	are deleted IF AND ONLY IF these files are successfully exported and packed into a tar archive."
	ExportToTarFile::usage = "ExportToTarFile"
	ProjectLabel::usage="ProjectLabel is used to give the exported tar archive a project specific name."
	
*)
Begin["`Private`"] (* Begin Private Context *)

Clear[eExport2XML];
Options[eExport2XML] = {AddImagesToTar->True,DeleteSuperfluousFiles->True,ExportToTarFile->True,AbortWhenOverride->False,ProjectLabel->""}
eExport2XML::nobool = nobool;
eExport2XML::nodat = "There is no data to export.";
eExport2XML::override = override;
eExport2XML::nostr = "Option 'ProjectLabel' has to be a string.";
eExport2XML[project_TrackingProject,opts:OptionsPattern[]]:=Module[
	{tar,ask,label,delete,addImages,data,projectPath,trackFile,metaFiles,tracks,ID2FrameMap,tracksXML, trackXMLFile, xmlPath,digits,tarFile,filesIncluded,imagePath,images},
	(*option validation*)
	{tar,ask,label,delete,addImages}=OptionValue[{ExportToTarFile,AbortWhenOverride,ProjectLabel,DeleteSuperfluousFiles,AddImagesToTar}];
	IsBoolean[eExport2XML,"ExportToTarFile",tar];
	IsBoolean[eExport2XML,"ExportToTarFile",ask];
	IsBoolean[eExport2XML,"ExportToTarFile",delete];
	IsBoolean[eExport2XML,"ExportToTarFile",addImages];
	If[!StringQ[label],
		Message[eExport2XML::nostr];
		Abort[]
	];
	
	(*implementation*)
	data=Level[project,1];
	projectPath="ProjectPath"/.data;
	If[!eValidateProject[project,"MetaFiles"],
		Abort[]
	];
	trackFile="TrackFile"/.data;
	metaFiles=eGetFileNames[project,"MetaFiles"];
	If[trackFile=="" || metaFiles == {},
		Message[eExport2XML::nodat];
		Abort[]
	];
	tracks=Import["TrackFile"/.data];
	ID2FrameMap=Dispatch@Flatten@ParallelTable[#->i&/@("ObjectID"/.Import[metaFiles[[i]]]),{i,Length@metaFiles},DistributedContexts->Automatic];
	
	tracksXML = XMLElement["Tracks", {},
  		Table[
  			XMLElement["Track", {}, Join[{XMLElement["TrackID", {}, {ToString[i]}]}, trackElement2XML[#, # /. ID2FrameMap] & /@ tracks[[i]]]]
  			,{i, Length@tracks}
 		]
	];
	trackXMLFile = FileNameJoin[{projectPath,"tracksXML.xml"}];
	xmlPath=FileNameJoin[{projectPath,"xml"}];
	If[!DirectoryQ[xmlPath],
		CreateDirectory[xmlPath]
	];
	If[ask && (FileExistsQ[trackFile] || Length@FileNames["*.xml",{xmlPath}]>0),
		Message[eExport2XML::override];
		Abort[]
	];
	digits = Length[IntegerDigits[Length[metaFiles]]];
	Export[trackXMLFile,tracksXML];
	ParallelTable[Export[eComposeFileName[xmlPath<>"/meta",".xml",i,digits],translateMetaFile2XML[metaFiles[[i]],i]],{i,Length@metaFiles},DistributedContexts->Automatic];
	eLogString["eExport2XML : Export of meta data [successful]."];
	If[tar,
		If[label=="",
			tarFile=FileNameJoin[{projectPath,"trackingXML.tar.bz"}],
			tarFile=FileNameJoin[{projectPath,label<>".tar.bz"}]
		];
		If[addImages,
			imagePath=FileNameJoin[{projectPath,"images"}];
			If[!DirectoryQ[imagePath],
				CreateDirectory[imagePath];
				If[First["RegisteredImages"/.data]=="",
					eLogString["eExport2XML : Images from " <> First["Images"/.data] <> " will be included in tar archive."];
					images=eGetFileNames[project,"Images"],
					eLogString["eExport2XML : Images from " <> First["RegisteredImages"/.data] <> " will be included in tar archive."];
					images=eGetFileNames[project,"RegisteredImages"]
				];
				ParallelTable[Export[eComposeFileName[imagePath<>"/img",".jpg",i,digits],Import[images[[i]]]],{i,Length@images},DistributedContexts->Automatic];
				filesIncluded = "tracksXML.xml xml images";
				eLogString["eExport2XML : Inclusion of images [successful]."],
				eLogString["eExport2XML : Inclusion of images to tar archive [failed]. Directory " <> imagePath <> " exists."];
				filesIncluded = "tracksXML.xml xml"
			],
			filesIncluded = "tracksXML.xml xml"
		];
		If[Run["tar","cfj " <> tarFile <> " -C " <> projectPath <> " " <> filesIncluded] == 0,
			If[delete,
				DeleteDirectory[xmlPath,DeleteContents->True];
				DeleteFile[trackXMLFile]
			];
			If[addImages && filesIncluded=="tracksXML.xml xml images",
				DeleteDirectory[imagePath,DeleteContents->True];
			];
			eLogString["eExport2XML : Tar file creation [successful]."],
			eLogString["eExport2XML : Tar file creation [failed]."]
		]
	];
]

Clear[trackElement2XML];
trackElement2XML[id_Integer,time_Integer]:= XMLElement["object", {}, {XMLElement["ObjectID", {}, {ToString[id]}], XMLElement["Time", {}, {ToString[time]}]}]

Clear[translateMetaRule2XML]
translateMetaRule2XML[Rule[name_,value_]] := 
	Switch[Depth[value],
   		1, XMLElement[name, {}, {XMLElement["value", {}, {ToString[value]}]}],
   		2, 
   		XMLElement[name, {}, {XMLElement["point", {}, {XMLElement["x", {}, {ToString[value[[1]]]}], 
       		XMLElement["y", {}, {ToString[value[[2]]]}]}]}
    	],
   		3, XMLElement[name, {}, 
    	XMLElement["point", {}, {XMLElement["x", {}, {ToString[#[[1]]]}], 
        XMLElement["y", {}, {ToString[#[[2]]]}]}] & /@ value
    	]
   	]
   	
Clear[translateMetaFile2XML];
translateMetaFile2XML[metaData_String,id_Integer]:=XMLElement["Frame_" <> ToString[id], {}, XMLElement["Object", {}, translateMetaRule2XML /@ #] & /@ Import[metaData]]

End[] (* End Private Context *)

EndPackage[]