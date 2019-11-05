(* Mathematica Package *)

BeginPackage["Eidomatica`TrackingImport`"]
Needs["Eidomatica`Import`"]
Needs["Eidomatica`Internal`"]
(* Exported symbols added here with SymbolName::usage *)  

(*
eImportTrackingData::usage="eImportTrackingData[xml,elements] extracts the given elements from the new XML data format xml, valid Elements are \"BoundingBox\", \"Centroid\", \"Links\", \"Mask\". The elements are given frame wise in the form {frameID,objectID}->{elements}.
eImportTrackingData[tracks] where tracks is the respective xml file containing the data for import.
eImportTrackingData[objectsLayer, trackingDataLayer] imports the tracking data from the automatic tracking approach implemented in ilastik, objectLayer is the exported object layer hdf5 file name and trackingDataLayer the project file name of the tracking project."
	UniqueIDs::usage="UniqueIDs is an option for eImportTrackingData, if 'True' objects which belong to the same track get the same unique ID."
eImportObjects::usage="eImportObjects[metaFile] imports objects from meta files into Mathematica."
eExtractTrackingData::usage ="eExtractTrackingData[observations, xmlfiles] extracts centroids and/or outlines from the xml files. observations is a list {{object_id,time}..} and xmlfiles a list {__String} containing the file names."
	What::usage ="What is an option for eExtractTrackingData, available options are \"All\", \"Tracks\" or \"Outline\"."
*)
$ObjectProperties = {"Centroid", "CentroidAndTime", "Mask", "Outline", "Rules", "Time"};
$TrackProperties = {"Length", "Complete"};

eReadDimensions::usage="eReadDimensions[fileName] reads the image dimensions from the given HDF5 file."
eReadTrackNames::usage="eReadTrackNames[fileName] reads the track names in the given HDF5 file."
$eReadTracksUsage=StringJoin["eReadTracks[fileName, properties] reads the given track properties for all tracks from the given HDF5 file with name 'fileName'. Properties can be a single property or a list of properties. Valid property values are ", Sequence@@Map[StringJoin["\"", #, "\", "] &, Most@$ObjectProperties], "and ", StringJoin["\"", Last@$ObjectProperties, "\""], "."]
eReadTracks::usage=$eReadTracksUsage
	ReadTracklets::usage="ReadTracklets is an option for eReadTracks if True /tracklets are read otherwise /autotracklets."
$eReadTrackUsage=StringJoin["eReadTrack[fileName, trackName, properties] reads the given track properties, for the given track properties from the given HDF5 file with name 'fileName'. Properties can be a single property or a list of properties. Valid property values are ", Sequence@@Map[StringJoin["\"", #, "\", "] &, Most@$ObjectProperties], "and ", StringJoin["\"", Last@$ObjectProperties, "\""], "."]
eReadTrack::usage=$eReadTrackUsage
eSortPaths::usage="eSortPaths[pathList, partID] sorts the list of path strings by the parts specified by partID, the given part of the path should be a number. Paths are separated by \"/\" by default.
eSortPaths[pathList, partID, pathSeparator] same as above with a user specified path separator."

	
Begin["`Private`"] (* Begin Private Context *)

(***********************************************************************
	Functions for import of tracking data and evaluation
************************************************************************)

Clear[eReadDimensions]
eReadDimensions[fileName_String]:=eImportHDF5[fileName, "Data", "/image_dimensions"]

Clear[eReadTrackNames]
eReadTrackNames[fileName_String]:=eSortPaths[eImportHDF5[fileName, "Names", "/tracklets", 1], -1, "/"];

dPcore[L_, p : {q___, _}] := 
 Inner[L[[# ;; #2]] &, {0, q} + 1, p, Head@L]
dPcore[L_, p_, All] := dPcore[L, p]~Append~Drop[L, Last@p]
dPcore[L_, p_, n__] := dPcore[L, p]~Join~Partition[L~Drop~Last@p, n]
dynamicPartition[L_, p : {__Integer}, x___] := 
 dPcore[L, Accumulate@p, x] /; ! Negative@Min@p && Length@L >= Tr@p

Clear[eSortPaths]
eSortPaths[list:{__String}, part_Integer, separator_String:"/"]:=Block[
	{},
	Values@KeySort@Association[Map[ToExpression@(StringSplit[#, separator][[part]]) -> # &, list]]
]

Clear[eReadTracks]
Options[eReadTracks]={ReadTracklets->True};
eReadTracks::nomemb = "Illegal value for parameter '`1`'. Value has to be one of the following `2`.";
eReadTracks::nobool = nobool;
eReadTracks[fileName_String, properties:(_String|{__String}), opts:OptionsPattern[]]:=Block[
	{trackNames,data},
    If[Head@properties===List,
        MemberCheck[eReadTracks, "properties", #, trackProperties] & /@ properties,
        MemberCheck[eReadTracks, "properties", properties, trackProperties]
    ];
    IsBoolean[eReadTracks,"ReadTracklets",OptionValue[ReadTracklets]];
    trackNames=eSortPaths[eImportHDF5[fileName, "Names", If[OptionValue[ReadTracklets],"/tracklets", "/autotracklets"], 1], -1, "/"];
    If[Head@properties===List,
    	data=eReadTracks[fileName, Flatten@trackNames, #] & /@ properties;
    	Map[Transpose, Transpose@data],
    	eReadTracks[fileName, Flatten@trackNames, properties]
    ]
]
eReadTracks[fileName_String, trackNames:{__String} ,property_String]:=Block[
	{objectNames, f, data},
	MemberCheck[eReadTracks, "property", property, $ObjectProperties];
	(*implementation*)
	objectNames = eImportHDF5[fileName, "Names", FileNameJoin[{#,"objects"}]& /@ trackNames, 1];
	objectNames = GatherBy[objectNames, ToExpression[StringSplit[#,"/"][[2]]]&];
    f[track_]:=eSortPaths[Flatten@StringCases[track, x: __ ~~ RegularExpression["[0-9]+$"]], -1, "/"];
    objectNames=f/@objectNames;
    data=Switch[property,
		"CentroidAndTime",
	     Flatten /@
			 Transpose[
				 {
					 eImportHDF5[fileName, "Data", FileNameJoin[{#, "centroid"}] & /@ Flatten@objectNames],
					 eImportHDF5[fileName, "Data", FileNameJoin[{#, "frame_id"}] & /@ Flatten@objectNames]
				 }
			 ],
	     "Centroid",
	     Flatten[eImportHDF5[fileName, "Data", FileNameJoin[{#, "centroid"}] & /@ Flatten@objectNames], {1,2}],
	     "Length",
	     eImportHDF5[fileName, "Data", FileNameJoin[{#, "end"}]&/@ trackNames] - eImportHDF5[fileName, "Data", FileNameJoin[{#, "start"}]&/@ trackNames],
	     "Mask",
	     "not implemented",
			 "Outline",
       eImportHDF5[fileName, "Data", FileNameJoin[{#, "outline"}]&/@ Flatten@objectNames],
	     "Rules",
	      Transpose[
	      	{
	      		Flatten[eImportHDF5[fileName, "Data", FileNameJoin[{#, "centroid"}] & /@ Flatten@objectNames],{1,2}],
            eImportHDF5[fileName, "Data", FileNameJoin[{#, "frame_id"}] & /@ Flatten@objectNames]
	      	}
	      ],
	     "Time",
       eImportHDF5[fileName, "Data", FileNameJoin[{#, "frame_id"}] & /@ Flatten@objectNames]
     ];
     If[SameQ[property,"Rules"],
     	MapThread[#1->#2&,{eImportHDF5[fileName, "Data", FileNameJoin[{#,"tracklet_id"}] &/@ trackNames],dynamicPartition[data,Length/@objectNames]}],
     	dynamicPartition[data,Length/@objectNames]
     ]
]

Clear[eReadTrack]
eReadTrack::nomem = "Illegal value for parameter '`1`'. Value has to be one of the following `2`.";
eReadTrack[fileName_String, trackName_String, properties : {__String}]:=Block[
	{},
    MemberCheck[eReadTrack, "property", #, $ObjectProperties] & /@
     properties;
    Transpose[eReadTrack[fileName, trackName, #] & /@ properties]
]
eReadTrack[fileName_String, trackName_String, property_String] :=Block[ 
	{objectNames},
    MemberCheck[eReadTrack, "property", property, $ObjectProperties];
    objectNames=eSortPaths[Flatten@StringCases[eImportHDF5[fileName, "Names", FileNameJoin[{trackName,"objects"}], 1], x: __ ~~ RegularExpression["[0-9]+$"]], -1, "/"];
    Switch[property,
		"CentroidAndTime",
	     Flatten /@ 
	      Transpose[
	      	{
	      		eImportHDF5[fileName, "Data", FileNameJoin[{#, "centroid"}] & /@ objectNames], 
						eImportHDF5[fileName, "Data", FileNameJoin[{#, "frame_id"}] & /@ objectNames]
					}
	      ],
	     "Centroid",
	     Flatten[eImportHDF5[fileName, "Data", FileNameJoin[{#, "centroid"}] & /@ objectNames], {1,2}],
	     "Length",
	     First@eImportHDF5[fileName, "Data", FileNameJoin[{trackName, "end"}]] - First@eImportHDF5[fileName, "Data", FileNameJoin[{trackName, "start"}]],
	     "Mask",
	     "not implemented",
			 "Outline",
       eImportHDF5[fileName, "Data", FileNameJoin[{#, "outline"}]&/@ Flatten@objectNames],
			 "Rules",
       Rule[eImportHDF5[fileName, "Data", FileNameJoin[{trackName,"tracklet_id"}]],
				Transpose[
        {
          Flatten[eImportHDF5[fileName, "Data", FileNameJoin[{#, "centroid"}] & /@ Flatten@objectNames],{1,2}],
          eImportHDF5[fileName, "Data", FileNameJoin[{#, "frame_id"}] & /@ Flatten@objectNames]
        }
       ]],
	     "Time",
			 eImportHDF5[fileName, "Data", FileNameJoin[{#, "frame_id"}] & /@ objectNames]
     ]
]
  
(*
Clear[eImportObjects]
eImportObjects[frameFile_] := Block[
  {xml, objects},
  If[! FileExistsQ[frameFile],
   Abort[]
   ];
  xml = Import[frameFile];
  objects = Cases[xml, XMLElement["Object", _, _], Infinity];
  transform /@ objects
]

validate[tracks_]:=Block[
	{observations},
	observations="Observations" /. # & /@ tracks;
	If[!And@@(#<0&/@(Min/@Transpose@Flatten[observations,{1,2}])),(*if object id's or time < 0*)
		Message[eImportTrackingData::invd]
	]
]

(*extract and rearrange track information*)
Clear[getTrack]
getTrack[rawTrack_] := Module[
	{items},
  	items = DeleteCases[Flatten[transform[rawTrack], 1], {}];
  	{"TrackID" -> ToExpression[("ID" /. items)], 
   	"Status" -> ToExpression[("Status" /. items)], 
   	"Daughters" -> ToExpression /@ ("Daughters" /. items), 
   	"Observations" -> Map[ToExpression, (Select["Observation" /. List /@ items, ListQ]), -1]}
]

(*Transformation rules to parse XML data*)
Clear[transform]
transform[XMLElement["Track", {}, {XMLElement["TrackID", {}, {id_}], children___}]] := {"ID" -> ToExpression@id, Map[transform, {children}]};
transform[XMLElement["Daugters", {}, {daughters___}]] := "Daughters" -> Flatten@Map[transform, {daughters}];
transform[XMLElement["DaugterID", {}, {id_}]] := ToExpression@id;
transform[XMLElement["Status", {}, {id_}]] := "Status" -> ToExpression@id;
transform[XMLElement["MotherID", {}, {id_}]] := {};
transform[XMLElement["object", {}, {XMLElement["ObjectID", {}, {id_}], XMLElement["Time", {}, {t_}]}]] := "Observation" -> ToExpression@{id, t}

(*transformation rules for meta files *)
transform[XMLElement["Object", {}, {XMLElement["ObjectID", {}, {XMLElement["value", {}, {id_}]}], children___}]] := Rule[ToExpression@id, Map[transform, {children}]]
transform[XMLElement["ObjectCenter", {}, {point_}]] := "Centroid" -> transform[point]
transform[XMLElement["ObjectBoundingBox", {}, points__]] := "BoundingBox" -> Map[transform, points]
transform[XMLElement["Outline", {}, points__]] := "Outline" -> Map[transform, points]
transform[XMLElement["TrackID", {}, {XMLElement["value", {}, {id_}]}]] := "TrackID" -> id
transform[XMLElement["point", {}, {XMLElement["x", {}, {x_}], XMLElement["y", {}, {y_}]}]] := ToExpression@{x, y}

Clear[eExtractTrackingData];
eExtractTrackingData::nf = "Timepoints exceed number of xml files.";
eExtractTrackingData::nomemb = nomemb;
Options[eExtractTrackingData] = {"What" -> "All"};
eExtractTrackingData[observations : {{_Integer, _Integer} ..}, xmlfiles : {__String}, opts : OptionsPattern[]]:=eExtractTrackingData[{observations}, xmlfiles, opts]
eExtractTrackingData[observations:{{{_Integer, _Integer}..}..}, xmlfiles : {__String}, opts : OptionsPattern[]]:=Block[
	{list,ids, time, what, centroids, outlines, combine, gather, res},
	what=OptionValue["What"];
	MemberCheck[eExtractTrackingData,"What",what,{"All","Tracks","Outline"}];
	{ids, time} = Transpose[Flatten[observations,{1,2}]];
  	If[Max[time] > Length@xmlfiles,
   		Message[eExtractTrackingData::nf];
   		Abort[]
   	];
   	list=Flatten[MapIndexed[Join[#1, {First@#2}] &, observations, {2}], {1, 2}];
	list = GatherBy[list, #[[2]] &];
   	combine[x_, y_] := MapThread[Join[{#1}, #2[[2;;3]]] &, {x, y}];
   	gather[function_,data_]:=Block[
   		{xml, tmp, ret},
   		ret=Map[
    		(xml = Import[xmlfiles[[(First@#)[[2]]]]];
    		tmp=function[xml,#[[All,1]]];
    		combine[tmp,#])&
    		,data
    		];
    	Map[Last@First@#->#[[All,1;;2]]&,GatherBy[Flatten[ret, {1, 2}], Last]]
	];
  	res = Switch[what,
    	"All",
    	{centroids, outlines}={gather[extractCentroid,list],gather[extractOutline,list]};
    	{"Tracks" -> centroids, "Outlines" -> outlines},
    	"Tracks",
    	centroids=gather[extractCentroid,list];
      	"Tracks"->centroids,
    	"Outline",
    	outlines=gather[extractOutline,list];
      	"Outlines"->outlines,
      	_,
      	{}
    ];
    res
]

Clear[extractCentroid]
extractCentroid[xml_,ids:{__Integer}]:=extractCentroid[xml,#]&/@ids;
extractCentroid[xml_, id_Integer] := 
  ToExpression@
   Flatten@Cases[
      Cases[Cases[
        xml, {XMLElement[
          "ObjectID", {}, {XMLElement[
            "value", {}, {ToString[id]}]}], __}, Infinity], 
       XMLElement["ObjectCenter", _, _], Infinity], 
      XMLElement[x_, _, _] /; MemberQ[{"x", "y"}, x], Infinity][[All, 
     3]];
Clear[extractOutline]
extractOutline[xml_,ids:{__Integer}]:=extractOutline[xml,#]&/@ids;
extractOutline[xml_, id_Integer] := 
  ToExpression@
   Partition[
    Flatten@Cases[
       Cases[Cases[xml, 
         XMLElement[
          "Object", _, {XMLElement[
            "ObjectID", {}, {XMLElement[
              "value", {}, {ToString[id]}]}], __}], Infinity], 
        XMLElement["Outline", _, _], Infinity], 
       XMLElement[x_, _, _] /; MemberQ[{"x", "y"}, x], Infinity][[All,
        3]], 2];
        
Clear[eImportTrackingData]
eImportTrackingData::invd = "Warning, tracking data contains gaps."
eImportTrackingData::inve="The given element `1` is not valid, possible values are \"BoundingBox\",\"Centroid\",\"Links\",\"Mask\"."
Options[eImportTrackingData]={UniqueIDs->False};
eImportTrackingData[file_String]:=Block[
	{xml,rawTracks,tracks},
	xml = Import[file];
	If[!SameQ[xml[[1]], {}],
		(*my proper xml implementarion*)
		Print["not yet implemented"]
		,
		(*the rest*)
		rawTracks = Cases[xml, XMLElement["Track", _, _], Infinity];
		tracks = getTrack /@ rawTracks;
	];
	validate[tracks];
	tracks
]
eImportTrackingData[data:(XMLObject["Document"][{XMLObject["Declaration"]["Version" -> "1.0", "Encoding" -> "ISO-8859-1"]}, XMLElement["celltrackingResults", {}, {__}], {}]), 
 	element_String,opts:OptionsPattern[]]:=eImportTrackingData[data, {element},opts]
eImportTrackingData[data:(XMLObject["Document"][{XMLObject["Declaration"]["Version" -> "1.0", "Encoding" -> "ISO-8859-1"]}, XMLElement["celltrackingResults", {}, {__}], {}]), 
	elements : {__String},opts:OptionsPattern[]]:=Block[
	{elementValues, elem, getFrameID, getPoint, getSuccessors, rleDecode, getMask, getObjects, dimensions, frames,links,unique,divisions,ends,comp,dispatch, rest, res},
  
	(*check parameters*)
	elementValues = {"BoundingBox", "Centroid", "Links", "Mask"};
	If[ ! MemberQ[elementValues, #],
	    Message[eImportTrackingData::inve, #];
	    Abort[]
	] & /@ elements;
	elem=If[Length@elements==1,First@elements,elements];
	
	(*function definitions*)
	getFrameID[XMLElement["frame", {}, {XMLElement["id", _, {id_}], ___}]]:=ToExpression@id;
	getPoint[points : {__XMLElement}]:=getPoint[#] & /@ points;
	getPoint[XMLElement["point", {}, {XMLElement["x", {}, {x_}], XMLElement["y", {}, {y_}]}]]:=N@ToExpression@{x, y};
	getSuccessors[succ:{___XMLElement}]:=getSuccessors /@ succ;
	getSuccessors[XMLElement["successor", {}, {XMLElement["objectID", {}, {objectID_}], XMLElement["frameID", {}, {frameID_}], _}]]:=ToExpression@{frameID, objectID};
	rleDecode[rle_String]:=Flatten[ConstantArray[If[Last@#=="B",0,1], {ToExpression@First@#}] & /@ Partition[StringSplit[rle, {"B" -> "B", "W" -> "W"}], 2]];
	getMask[rle_, imageDimensions_, box_]:=Block[ 
    	{x, y},
        {x, y} = IntegerPart@Transpose@box+1;
        SparseArray[
         MapThread[#1 -> #2 &, {Flatten[
            Table[{i, j}, {i, y[[1]], y[[2]]}, {j, x[[1]], x[[2]]}], {1, 
             2}], rleDecode[rle]}], Reverse@imageDimensions]
    ];
	getObjects[frame_,extraction_]:=Block[
		{xml},
        xml = 
         Cases[frame, 
          XMLElement[
            "object", _, {XMLElement["id", _, {id_}], _, 
             XMLElement["centroid", _, {point_}], 
             XMLElement[
              "mask", {}, {XMLElement["bbox", {}, boundingBox_], 
               XMLElement["runLengthString", {}, {rle_}]}], 
             XMLElement["links", {}, successors_]}] -> 
           Rule[{getFrameID[frame], ToExpression[id]},
            {"Centroid" -> point, "BoundingBox" -> boundingBox, 
             "Mask" -> rle, "Links" -> successors}], Infinity];
        ReleaseHold[
			ToExpression@First@# -> (extraction /. 
         		{"Centroid" -> Hold[getPoint["Centroid"]], 
				 "BoundingBox" -> Hold[getPoint["BoundingBox"]], 
				 "Mask" -> Hold[getMask["Mask", dimensions, getPoint["BoundingBox"]]], 
                 "Links" -> Hold[getSuccessors["Links"]]}
                 ) /. Last@# & /@ xml
		]
	];	
	
	(*import*)
	dimensions = 
	 ToExpression@
	  First@Cases[data, 
	    XMLElement[
	      "imageDimensions", {}, {XMLElement["width", {}, {width_}], 
	       XMLElement["height", {}, {height_}]}] -> {width, height}, 
	    Infinity];
	frames = Cases[data, XMLElement["frame", _, _], Infinity];
	res=Map[getObjects[#,elem] &, frames];
	If[OptionValue[UniqueIDs]==True,
    	links=Flatten[Map[getObjects[#,"Links"] &, frames]];
    	unique = First@# -> Last@Last@# & /@ Cases[links, x_ /; MatchQ[x, Rule[{__Integer}, {{__Integer}}]]];
		divisions = Cases[links, x_ /; Length@Last@x > 1];
		ends = First@# -> First@# & /@ Cases[links, x_ /; MatchQ[x, Rule[y_, {}]]];
		comp = WeakComponents[Join[unique, ends]];
		dispatch=Dispatch@Flatten@Table[# -> i & /@ comp[[i]], {i, 1, Length@comp}];
		res=res/.dispatch;
		rest=First/@Cases[res, x_ /; Head@First@x == List, {2}];
		dispatch=Dispatch@Table[rest[[i]]->i+Length@comp,{i,1,Length@rest}];
		res/.dispatch,
		res
    ]
]
eImportTrackingData[objectsLayer_String, trackingDataLayer_String]:=Block[
	{datasets, movements, divisions, labels, links, tmp, mov, div, comp, dispatch, rest, dispatched, uniqueDivisions, index, uniqueLabels},
  	datasets=ImportHDF5[trackingDataLayer];
  	movements=Sort[Flatten@StringCases[datasets, __ ~~ "/mov"], ToExpression@Last@StringCases[#1, DigitCharacter ..] < ToExpression@Last@StringCases[#2, DigitCharacter ..] &];
  	divisions=Sort[Flatten@StringCases[datasets, __ ~~ "/div"], ToExpression@Last@StringCases[#1, DigitCharacter ..] < ToExpression@Last@StringCases[#2, DigitCharacter ..] &];
	labels=Transpose@Flatten[#, {1, 3, 4}] & /@ ImportHDF5[objectsLayer, {"Datasets", "/exported_data"}];
  	links=Flatten@Table[
			mov={frame, #[[1]]} -> {frame + 1, #[[2]]} & /@ IntegerPart /@
        		If[(tmp=Cases[movements, x_ /; ToExpression@Last@StringCases[x, NumberString]==frame]) === {},
      				{},
         			First@ImportHDF5[trackingDataLayer, {"Datasets", tmp}]
         		];
     		div=Flatten[{frame, First@#} -> {{frame + 1, #[[2]]}, {frame + 1, #[[3]]}} & /@
					If[(tmp=Cases[divisions, x_ /; ToExpression@Last@StringCases[x, NumberString] == frame]) === {},
         				{},
         				Most /@ IntegerPart /@ First@ImportHDF5[trackingDataLayer, {"Datasets", tmp}]
         			]
       			];
     		Join[mov, div]
     		,{frame, 1, Length@labels}
     ];
  	mov = Cases[links, x_ /; MatchQ[x, Rule[{__Integer}, {__Integer}]]];
  	div = Cases[links, x_ /; MatchQ[Last@x, {{__Integer} ..}]];
  	comp = WeakComponents[mov];
  	dispatch = Flatten@Table[# -> i & /@ comp[[i]], {i, 1, Length@comp}];
  	rest = Cases[Cases[links /. Dispatch@dispatch, x_ /; ! SameQ[First@x, Last@x]], x_ /; Head@x == List, {2}] /. {{x_List, _Integer} -> x, {_Integer, x_List} -> x, {x_List, y_List} -> Sequence @@ {x, y}};
  	dispatched=Dispatch@Flatten@Join[dispatch, Table[rest[[i]] -> i + Length@comp, {i, 1, Length@rest}]];
  	uniqueDivisions = SortBy[DeleteCases[links /. dispatched, x_ /; SameQ[First@x, Last@x]], First];
  	index = Length@rest + Length@comp;
  	uniqueLabels=Table[({i, First@#} -> Last@# & /@ ComponentMeasurements[labels[[i]], "Mask"]) /. dispatched /. (Rule[_List, x_] -> Rule[++index, x]), {i, 1, Length@labels}];
  	{uniqueDivisions, uniqueLabels}
]
*)


End[] (* End Private Context *)

EndPackage[]