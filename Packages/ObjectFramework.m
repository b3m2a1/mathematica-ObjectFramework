(* ::Package:: *)

$packageHeader

$OFObjectTable::usage=
	"The master table of objects an types";


OFObject::usage="An object-framework object";
OFType::usage="An object-framework type";
OFMethod::usage=
	"A method wrapper for a function and a property association";


OFNew::usage="Makes a new object instance";
OFSelect::usage="Gets parts of the object table by key pattern";
OFCount::usage="Returns the number of objects matching a pattern";
OFClear::usage="Clears objects matching a pattern";


OFNewType::usage="Makes a new type";
OFTypeSelect::usage="Gets parts of the type table by key pattern";
OFTypeCount::usage="Returns the number of types matching a pattern";
OFTypeClear::usage="Clears types matching a pattern";


OFStrings::usage=
	"Takes a list of strings and OFObjects and gets the object keys";
OFRecursiveLookup::usage=
	"The recursive lookup function that scans up an object heiarchy for a property";
$OFPrepMethods::usage=
	"Specified whether to attempt to convert methods into functions";
OFPrepMethod::usage=
	"Converts an OFMethod-key pairing into a bound function";
OFPostLookup::usage=
	"A function applied to convert methods into functions";
$OFLookupGroup::usage=
	"The default group (\"Objects\" or \"Types\") to assume the object is part of";
OFLookup::usage=
	"Looks up object properties or definitions";


$OFMutateGroup::usage=
	"Mutation equivalent of $OFLookupGroup";


OFMutate::usage=
	"Applies a function to an object either by entire array or by keys";
OFSet::usage=
	"Sets an object key value";
OFSetDelayed::usage=
	"Sets an object key value with RuleDelayed";
OFSetPart::usage=
	"Uses set on a part of an object key";
OFSetDelayed::usage=
	"Uses SetDelayed on a part of an object key";
OFSetIndex::usage=
	"Sets an index part of an object key";
OFUnset::usage=
	"Uses unset on an object key or keys";
OFAdjust::usage=
	"Mutates a single key, passing the key value and an adjustment value to a function";
OFAddTo::usage=
	"Uses OFAdjust to implement AddTo";
OFSubtractFrom::usage=
	"Uses OFAdjust to implement SubtractFrom";
OFJoin::usage=
	"Uses Join with OFAdjust";
OFJoinMerge::usage=
	"Uses Join to merge the current object definitions and a new definition association";
OFMerge::usage=
	"Uses Merge to merge the current object definitions and a new definition association";
OFMap::usage=
	"Uses Map or MapAt to apply a function to the current definitions or a subset of them";
OFMergeApply::usage=
	"Uses a custom mergeApply on the current definitions.
Applies functions provided in a secondary association to their current values";


OFCopy::usage="";
OFRecursiveCopy::usage="";


OFExport::usage=
	"Exports to a file, directory, LocalObject";
OFCloudExport::usage=
	"Exports to a CloudObject (takes the specifications of CloudPut)";
OFExportExpression::usage=
	"Returns the expression corresponding to OFExport";
OFExportString::usage=
	"Returns ths string corresponding to OFExport";


OFHold::usage=
	"A formatted version of Hold"
$OFState::usage=
	"The current object framework state wrapped in OFHold";


$OFConstructorStack::usage=
	"The build stack (current object last)";


OFBegin::usage="";
OFInit::usage="";
OFRepr::usage="";
OFStr::usage="";
OFField::usage="";
OFEnd::usage="";


Begin["`Private`"];


If[!MatchQ[$OFObjectTable,_Association],
	$OFObjectTable=<|
		"Types"-><|
			"Object"-><||>
			|>,
		"Objects"-><||>
		|>
	];


OFNew[a:{(_Association->_)..}]:=
	With[{
		types=Lookup[Map[First,a],"ObjectType","Object"],
		objs=Map[First,a],
		args=Map[Last,a]
		},
		With[{uuids=CreateUUID[#<>"-"]&/@types},
			AssociateTo[$OFObjectTable["Objects"],
				MapThread[
					#3->
						Join[
							#,
							<|
								"ObjectType"->#2
								|>
							]&,
					{
						objs,
						types,
						uuids
						}]
				];
			MapThread[
				With[{f=#2,o=#},
					Function[Null,f[o,##],HoldComplete]@@#3
					]&,
				{
					OFObject/@uuids,
					Lookup[
						Lookup[$OFObjectTable["Types"],types,<||>],
						"ObjectInitialization",
						Identity
						],
					args
					}];
		OFObject/@uuids
		]
	];
OFNew[s_String,args___]:=
	OFNew[<|"ObjectType"->s|>,args];


OFSelect[pattern_:_]:=
	KeySelect[$OFObjectTable["Objects"],MatchQ[pattern]]


OFCount[pattern_:_]:=
	Length@OFSelect[pattern]


OFClear[pattern_:_]:=
	With[{deleted=OFSelect[pattern]},
		$OFObjectTable["Objects"]=
			KeySelect[$OFObjectTable["Objects"],MatchQ[Except[pattern]]];
		deleted
		];


OFObject[OFObject[k_]]:=
	OFObject[k];


OFNewType[name_,a_Association]:=
	$OFObjectTable["Types",name]=a;
OFNewType[name_]:=
	$OFObjectTable["Types",name]=<||>;


OFTypeSelect[pattern_:_]:=
	KeySelect[$OFObjectTable["Types"],MatchQ[pattern]]


OFTypeCount[pattern_:_]:=
	Length@OFTypeSelect[pattern]


OFTypeClear[pattern_:_]:=
	With[{deleted=OFTypeSelect[pattern]},
		$OFObjectTable["Types"]=
			KeySelect[$OFObjectTable["Types"],MatchQ[Except[pattern]]];
		deleted
		];


OFType[t_][a___]:=
	OFNew[t,a];


objectPattern=_String|_OFObject|{__String}|{__OFObject};


OFStrings[k:objectPattern]:=
	Map[If[StringQ@#,#,First@#]&,Flatten[{k},1]];


OFStrings[{}]={};


OFRecursiveLookup[a_Association,
	key:Except[_List|_Association],
	default_:Missing["KeyAbsent"]]:=
	Lookup[a,key,
		With[{type=Lookup[a,"ObjectType"]},
			If[KeyMemberQ[$OFObjectTable["Types"],type],
				OFRecursiveLookup[$OFObjectTable["Types",type],key,default],
				If[default===Missing["KeyAbsent"],
					Missing["KeyAbsent",key],
					default
					]
				]
			]
		];
OFRecursiveLookup[a_Association,
	keys:_List|_Association,
	default_:Missing["KeyAbsent"]]:=
	OFRecursiveLookup[a,#,default]&/@keys;
OFRecursiveLookup[m_Missing,__]:=
	m;


$OFPrepMethods=True;


OFPrepMethod[OFMethod[f_,a_Association],id_String]:=
	With[{
		atts=Lookup[a,"Attributes",{}],
		autoEval=Lookup[a,"Evaluate",False],
		obj=
			Replace[
				Lookup[a,"PassFirst",OFObject@id],{
					OFType|"ObjectType":>
						OFType@Lookup[id,"ObjectType","Object"],
					None:>
						Sequence[]
				}]},
		If[autoEval,
			f[obj],
			Function[Null,f[obj,##],atts]
			]
		];
OFPrepMethod[OFMethod[f_Symbol],id_String]:=
	With[{obj=OFObject@id,attrs=Attributes[f]},
		Function[f[obj,##],attrs]
		];
OFPrepMethod[OFMethod[f:HoldPattern[Function[_]|Function[_,_]]],id_String]:=
	With[{obj=id},
		Function[f[obj,##]]
		];
OFPrepMethod[OFMethod[f:HoldPattern[Function[_,_,a_]]],id_String]:=
	With[{obj=OFObject@id},
		Function[Null,f[obj,##],a]
		];


OFPostLookup[e_OFMethod,id_]:=
	If[$OFPrepMethods//TrueQ,
		OFPrepMethod[e,id],
		e
		];
OFPostLookup[l:_List,id_]:=
	OFPostLookup[#,id]&/@l;
OFPostLookup[e:Except[_OFMethod|_List],id_]:=
	e;


$OFLookupGroup="Objects";


OFLookup[k_String|OFObject[k_String]]:=
	Lookup[$OFObjectTable[$OFLookupGroup],k];
OFLookup[k:{__String}]:=
	Lookup[$OFObjectTable[$OFLookupGroup],k];
OFLookup[k:{__OFObject}]:=
	Lookup[$OFObjectTable[$OFLookupGroup],First/@k];


OFLookup[k_String,s_,default_:Missing["KeyAbsent"]]:=
	OFPostLookup[
		OFRecursiveLookup[Lookup[$OFObjectTable[$OFLookupGroup],k],s,default],
		k];
OFLookup[OFObject[k_String],s_,default_:Missing["KeyAbsent"]]:=
	Block[{$OFLookupGroup="Objects"},
		OFLookup[k,s,default]
		];
OFLookup[OFType[k_String],s_,default_:Missing["KeyAbsent"]]:=
	Block[{$OFLookupGroup="Types"},
		OFLookup[k,s,default]
		];
OFLookup[
	keys:{__String},p_,
	default_:Missing["KeyAbsent"]]:=
	MapThread[
		OFPostLookup[
			OFRecursiveLookup[#2,p,default],
			#]&,
		{
			keys,
			Lookup[$OFObjectTable[$OFLookupGroup],keys]
			}
		];
OFLookup[keys:{__OFObject},p_,default_:Missing["KeyAbsent"]]:=
	Block[{$OFLookupGroup="Objects"},
		OFLookup[First/@keys,p,default]
		];
OFLookup[keys:{__OFType},p_,default_:Missing["KeyAbsent"]]:=
	Block[{$OFLookupGroup="Types"},
		OFLookup[First/@keys,p,default]
		];


$OFMutateGroup=
	"Objects";


OFMutate[k_String,f_]:=
	If[KeyMemberQ[$OFObjectTable[$OFMutateGroup],k],
		$OFObjectTable[$OFMutateGroup,k]=
			f@$OFObjectTable[$OFMutateGroup,k],
		Missing["KeyAbsent",k]
		];
OFMutate[k_String,p_,f_]:=
	If[KeyMemberQ[$OFObjectTable[$OFMutateGroup],k],
		f[OFLookup[k,p],k],
		Missing["KeyAbsent",k]
		];
OFMutate[OFObject[k_String],f_]:=
	Block[{$OFMutateGroup="Objects"},
		OFMutate[k,f]
		];
OFMutation[OFObject[k_String],f_,p_]:=
	Block[{$OFMutateGroup="Objects"},
		OFMutate[k,f,p]
		];
OFMutate[OFType[k_String],f_]:=
	Block[{$OFMutateGroup="Types"},
		OFMutate[k,f]
		];
OFMutation[OFType[k_String],f_,p_]:=
	Block[{$OFMutateGroup="Types"},
		OFMutate[k,f,p]
		];


OFMutate[k:{__String},f_]:=
	If[KeyMemberQ[$OFObjectTable[$OFMutateGroup],#],
		$OFObjectTable[$OFMutateGroup,#]=
			f[$OFObjectTable[$OFMutateGroup,#]];
		OFObject[#],
		Missing["KeyAbsent",#]
		]&/@k;
OFMutate[k:{__String},p_,f_]:=
	With[{l=
		If[KeyMemberQ[$OFObjectTable[$OFMutateGroup],#],
			f[OFLookup[#,p],#];
			OFObject[#],
			Missing["KeyAbsent",#]
			]&/@k
		},
		If[Length@l==1,
			First@l,
			l
			]
		];
OFMutate[k:{__OFObject},f_]:=
	Block[{$OFMutateGroup="Objects"},
		OFMutate[First/@k,f]
		];
OFMutate[k:{__OFObject},p_,f_]:=
	Block[{$OFMutateGroup="Objects"},
		OFMutate[First/@k,p,f]
		];
OFMutate[k:{__OFType},f_]:=
	Block[{$OFMutateGroup="Types"},
		OFMutate[First/@k,f]
		];
OFMutate[k:{__OFType},p_,f_]:=
	Block[{$OFMutateGroup="Types"},
		OFMutate[First/@k,p,f]
		];


OFSet[k:objectPattern,p_,v_]:=
	OFMutate[k,p,
		Function[Null,Set[#,v],HoldFirst]];


OFSetDelayed[k:objectPattern,p_,v_]:=
	OFMutate[k,p,Function[Null,SetDelayed[#,v],HoldFirst]];


OFSetPart[k:objectPattern,key_,parts__,value_]:=
	Block[{ofObCachedPart=OFLookup[k,key]},
		ofObCachedPart[parts]=value;
		OFSet[k,key,ofObCachedPart]
		];


OFSetPartDelayed[k:objectPattern,key_,parts__,value_]:=
	Block[{ofObCachedPart=OFLookup[k,key]},
		ofObCachedPart[parts]:=value;
		OFSet[k,key,ofObCachedPart]
		];


OFSetIndex[k:objectPattern,key_,inds__,value_]:=
	Block[{ofObCachedPart=OFLookup[k,key]},
		ofObCachedPart[[inds]]=value;
		OFSet[k,key,ofObCachedPart]
		];


OFUnset[k:objectPattern,p_]:=
	OFMutate[k,p,Function[Null,Unset@#,HoldFirst]];


OFAdjust[k:objectPattern,p_,v_,function_:Plus]:=
	OFMutate[k,p,($OFObjectTable[$OFMutateGroup,#2,p]=function[v,#])&]


OFAddTo[k:objectPattern,p_,v_]:=
	OFAdjust[k,p,v,Plus]


OFSubtractFrom[k:objectPattern,p_,v_]:=
	OFAdjust[k,p,v,Subtract]


OFJoin[k:objectPattern,p_,v_]:=
	OFAdjust[k,p,v,Join]


OFJoinMerge[k:objectPattern,a_Association,mode:"Override"|"Default":"Override"]:=
	If[mode=="Override",
		OFMutate[k,Join[#,a]&],
		OFMutate[k,Join[a,#]&]
		];


OFMerge[k:objectPattern,a_Association,f_:Last]:=
	OFMutate[k,Merge[{#,a},f]&]


OFMap[k:objectPattern,f_]:=
	OFMutate[k,Map[#,f]&];
OFMap[k:objectPattern,f_,keys_]:=
	With[{keySpec=Key/@Flatten[{keys},1]},
		OFMutate[k,MapAt[#,f,keySpec]&]
		];


mergeApply[a_List]:=
	Merge[a,mergeTag]/.{
		mergeTag[{e_}]:>
			RuleCondition[Unevaluated[e],True],
		mergeTag[{v_,f__}]:>
			Fold[Function[Null,#2[#],HoldFirst],Unevaluated[v],{f}]
		};
mergeTag~SetAttributes~HoldAllComplete;


OFMergeApply[k:objectPattern,a_Association]:=
	OFMutate[k,mergeApply[{#,a}]&]


OFReferencedObjects[k:objectPattern,exclude:_List:{}]:=
	With[{refs=
		Cases[
			OFLookup[#],
			OFObject[id:Except[Alternatives@@exclude]]:>id,
			\[Infinity]
			]},
		If[Length@refs>0,	
			OFReferencedObjects[refs,Append[exclude,#]],
			refs
			]
		]&/@OFStrings[k];


OFReferencedTypes[k:objectPattern,exclude:_List:{}]:=
	With[{type=
		DeleteCases[
			Flatten@OFLookup[#,{"ObjectType"},{"Object"}],
			Alternatives@@exclude
			]},
		If[Length@type>0,	
			Flatten@{
				Block[{$OFLookupGroup="Types"},
					OFReferencedTypes[Flatten@type,Flatten@{exclude,#,type}]
					],
				type
				},
			type
			]
		]&/@OFStrings[k];


OFReferencedMethods[k:objectPattern]:=	
	Cases[
		Flatten@{
			OFLookup[#],
			With[{t=Flatten@OFReferencedTypes@#},
				Block[{$OFLookupGroup="Types"},
					OFLookup@t
					]
				]},
		s_Symbol?(
			Function[Null,
				KeyMemberQ[$OFBuiltMethods,Unevaluated[#]],
				HoldFirst]):>
			HoldPattern[s],
		\[Infinity],
		Heads->True]&/@OFStrings[k];


OFCopy[k:objectPattern]:=
	Replace[OFLookup[OFStrings[k]],{
		a_Association:>
			OFNew[a]
		},
		1]


OFRecursiveCopy[k:objectPattern]:=
	With[{totalRefs=
		DeleteDuplicates@Join[
			Flatten@{k},
			First/@Flatten@OFReferencedObjects@OFStrings[k]
			]},
		With[{a=Association[#->First[#]&/@totalRefs]},
			OFMutate[Values@a,ReplaceAll[a]]
			]
		]


OFRestoreObjects[k:objectPattern]:=
	With[{
		objs=
			KeySelect[$OFObjectTable["Objects"],
				Flatten@{OFReferencedObjects[k],OFStrings[k]}],
		types=
			KeySelect[$OFObjectTable["Types"],
				MatchQ[Alternatives@@
					Flatten@OFReferencedTypes@
						Flatten@{OFReferencedObjects[k],OFStrings[k]}]]},
		Hold[
			If[!MatchQ[$OFObjectTable,_Association],
				$OFObjectTable=<|
					"Types"->types,
					"Objects"->objs
					|>,
				$OFObjectTable["Objects"]=
					Join[$OFObjectTable["Objects"],objs];
				$OFObjectTable["Types"]=
					Join[$OFObjectTable["Types"],types];
				];
			]
		]


OFRestoreMethods[s_Symbol|Verbatim[HoldPattern][s_Symbol]]:=
	With[{
		ats=Attributes[s],
		ops=Options[s],
		msgs=Messages[s],
		ovs=OwnValues[s],
		dvs=DownValues[s],
		uvs=UpValues[s],
		svs=SubValues[s],
		fvs=FormatValues[s],
		nvs=NValues[s]},
		Hold[
			Attributes[s]=ats;
			Options[s]=ops;
			Messages[s]=msgs;
			OwnValues[s]=ovs;
			DownValues[s]=dvs;
			UpValues[s]=uvs;
			SubValues[s]=svs;
			FormatValues[s]=fvs;
			NValues[s]=nvs;
			AssociateTo[$OFBuiltMethods,s->Null];
			]
		];


OFRestoreMethods[k:objectPattern]:=
	ReplacePart[
		Thread[
			Map[Thread[#,CompoundExpression]&,
				OFRestoreMethods/@
				Flatten@OFReferencedMethods@OFStrings[k]
				],
			Hold
			],
		{1,0}->CompoundExpression
		];


OFHold/:
	HoldPattern[ReleaseHold@OFHold[e_,___]]:=
		e;
OFHold~SetAttributes~HoldFirst


OFRestoreExpression[k:objectPattern]:=
	Replace[{OFStrings[k],OFRestoreObjects[k],OFRestoreMethods[k]},{
		{id_,Hold[CompoundExpression[o__]],Hold[CompoundExpression[m__]]}:>
			OFHold[
				CompoundExpression[o,m],
				<|"Items"->id|>
				],
		{id_,Hold[CompoundExpression[o__]],{}}:>
			OFHold[
				CompoundExpression[o],
				<|"Items"->id|>
				]
		}]


OFRestoreExpression[Optional[All,All]]:=
	OFRestoreExpression[Keys@OFSelect[]]


OFExport[OFHold[code_CompoundExpression,__],
	f_String?(
		FileExistsQ@DirectoryName[#]&&
		FileExtension[#]=="m"&)]:=
	Block[{$ContextPath={"System`"}},
		Export[f,Unevaluated[code]]];
OFExport[
	OFHold[code_CompoundExpression,__],
	l_LocalObject]:=
	Put[Unevaluated[code],l];


Options[OFCloudExport]=
	Options[CloudPut];
OFCloudExport[
	OFHold[code_CompoundExpression,__],
	cloudStuf___]:=
	CloudPut[Unevaluated[code],cloudStuf];


OFExport[
	k:objectPattern,
	f_String?(FileExistsQ@*DirectoryName)]:=
	If[DirectoryQ@f,
		OFExport[First@OFRestoreExpression[#],
			FileNameJoin@{f,#<>".m"}
			]&/@OFStrings[k],
		OFExport[
			OFRestoreExpression[k],
			f]
		]


OFExportString[k:objectPattern]:=
	OFExport[k,
		FileNameJoin@{
			$TemporaryDirectory,
			RandomSample[Alphabet[],15]<>".m"
			}
		]//With[{t=Import[#,"Text"]},DeleteFile@#;t]&//
			StringTrim[#,"(* Created with the Wolfram Language : www.wolfram.com *)\n"]&


OFExportExpression[k:objectPattern]:=
	OFRestoreExpression[k];


$OFState:=
	With[{k=Keys@OFSelect[]},
		If[Length@k==0,
			None,
			Insert[
				OFExportExpression@k,
				"Cached State",
				{2,"Name"}
				]
			]
		]


$OFAutoFormat=True;


$objIcon=
	With[{
		height=.75,width=.75,
		braX=2,braS=.5,braY=3,
		barX=2.7,dotSp=1.5,
		genThick=.035,arrowThick=.2,dotThick=Scaled@.035,
		arrX=10},
		Graphics[{
			Arrowheads@arrowThick,
			Thickness@genThick,
			Hue[.6,.5,.75],
			Arrow@BSplineCurve[{{0,0},{.5-width/arrX,0},{.5-width/arrX,.5-height/4}}],
			Line@{
				{.5-width/braX,.5-height/braY},
				{.5-width/(braX-braS),.5},
				{.5-width/braX,.5+height/braY}},
			Line@{{.5-width/barX,.5-height/2.5},{.5-width/barX,.5+height/2.5}},
			Hue[.4,.5,.75],
			Arrow@BSplineCurve[{{1,1},{.5+width/arrX,1},{.5+width/arrX,.5+height/4}}],
			Line@{
				{.5+width/braX,.5-height/braY},
				{.5+width/(braX-braS),.5},
				{.5+width/braX,.5+height/braY}},
			Line@{{.5+width/barX,.5-height/2.5},{.5+width/barX,.5+height/2.5}},
			Hue[0,.75,.75],
			Disk[{.5-width/(barX+dotSp),.5},dotThick],
			Disk[{.5,.5},dotThick],
			Disk[{.5+width/(barX+dotSp),.5},dotThick]
			},
			PlotRange->{{-.1,1.1},{-.1,1.1}},
			Background->GrayLevel[.95],
			Frame->True,
			FrameTicks->None,
			FrameStyle->GrayLevel[.75],
			ImageSize->{28,28}
			]
		];


$functIcon=
	With[{
		height=.75,width=.75,
		braX=2,braS=.5,braY=3,
		barX=2.7,dotSp=1.5,
		genThick=.035,arrowThick=.2,dotThick=Scaled@.035,
		arrX=10},
		Graphics[{
			Arrowheads@arrowThick,
			Thickness@genThick,
			Hue[.6,.5,.75],
			Line@{
				{.5-width/braX,.5-height/braY},
				{.5-width/(braX-braS),.5},
				{.5-width/braX,.5+height/braY}},
			Line@{{.5-width/barX,.5-height/2.5},{.5-width/barX,.5+height/2.5}},
			Hue[.4,.5,.75],
			Line@{
				{.5+width/braX,.5-height/braY},
				{.5+width/(braX-braS),.5},
				{.5+width/braX,.5+height/braY}},
			Line@{{.5+width/barX,.5-height/2.5},{.5+width/barX,.5+height/2.5}},
			Hue[0,.75,.75],
			Inset["@"]
			},
			PlotRange->{{-.1,1.1},{-.1,1.1}},
			Background->GrayLevel[.95],
			Frame->True,
			FrameTicks->None,
			FrameStyle->GrayLevel[.75],
			ImageSize->{28,28}
			]
		];


$deferIcon=
	With[{
		height=.75,width=.75,
		braX=2,braS=.5,braY=3,
		barX=2.7,dotSp=1.5,
		genThick=.035,arrowThick=.2,dotThick=Scaled@.035,
		arrX=10},
		Graphics[{
			Arrowheads@arrowThick,
			Thickness@genThick,
			GrayLevel[.65],
			(*Arrow@BSplineCurve[{{0,0},{.5-width/arrX,0},{.5-width/arrX,.5-height/4}}],*)
			Line@{
				{.5-width/braX,.5-height/braY},
				{.5-width/(braX-braS),.5},
				{.5-width/braX,.5+height/braY}},
			Line@{{.5-width/barX,.5-height/2.5},{.5-width/barX,.5+height/2.5}},
			(*Arrow@BSplineCurve[{{1,1},{.5+width/arrX,1},{.5+width/arrX,.5+height/4}}],*)
			Line@{
				{.5+width/braX,.5-height/braY},
				{.5+width/(braX-braS),.5},
				{.5+width/braX,.5+height/braY}},
			Line@{{.5+width/barX,.5-height/2.5},{.5+width/barX,.5+height/2.5}},
			Disk[{.5-width/(barX+dotSp),.5},dotThick],
			Disk[{.5,.5},dotThick],
			Disk[{.5+width/(barX+dotSp),.5},dotThick]
			},
			PlotRange->{{-.1,1.1},{-.1,1.1}},
			Background->GrayLevel[.95],
			Frame->True,
			FrameTicks->None,
			FrameStyle->GrayLevel[.75],
			ImageSize->{28,28}
			]
		];


Format[OFObject[id_String?(KeyMemberQ[$OFObjectTable["Objects"],#]&)]/;
	TrueQ@$OFAutoFormat]:=
	With[{repr=OFLookup[id,"ObjectRepresentation"]},
		If[repr===Missing["KeyAbsent","ObjectRepresentation"],
			RawBoxes@
				BoxForm`ArrangeSummaryBox[
					"OFObject",
					OFObject[id],
					$objIcon,
					{
						BoxForm`MakeSummaryItem[{"Type: ",OFLookup[id,"ObjectType"]},StandardForm]
						},
					KeyValueMap[
						BoxForm`MakeSummaryItem[{Row@{#,": "},
							#2/.OFObject[s_]:>s},StandardForm]&,
						KeySelect[$OFObjectTable["Objects",id],
							MatchQ@Except[_String?(StringMatchQ["Object*"])]]
						],
					StandardForm
					],
			Interpretation[repr[OFObject[id]],OFObject[id]]
			]
		];


Format[OFMethod[f_,attrs:_Association:<||>]/;
	TrueQ@$OFAutoFormat]:=
	RawBoxes@
		BoxForm`ArrangeSummaryBox[
			"OFMethod",
			OFMethod[f,attrs],
			$functIcon,
			{
				BoxForm`MakeSummaryItem[{"Type: ","Bound Method"},StandardForm]
				},
			Append[
				KeyValueMap[BoxForm`MakeSummaryItem[{Row@{#,": "},#2},StandardForm]&,attrs],
				BoxForm`MakeSummaryItem[{"Function: ",f},StandardForm]
				],
			StandardForm
			];


Format[OFType[f_?(KeyMemberQ[$OFObjectTable["Types"],#]&)]/;
	TrueQ@$OFAutoFormat]:=
	RawBoxes@
		BoxForm`ArrangeSummaryBox[
			"OFType",
			OFType[f],
			$objIcon,
			{
				BoxForm`MakeSummaryItem[{"Type: ",f},StandardForm]
				},
			KeyValueMap[
				BoxForm`MakeSummaryItem[{Row@{#,": "},#2},StandardForm]&,
				$OFObjectTable["Types",f]
				],
			StandardForm
			];


Format[OFHold[e_,info:_Association:<||>]/;
	TrueQ@$OFAutoFormat]:=
	RawBoxes@
		BoxForm`ArrangeSummaryBox[
			"OFHold",
			OFHold[e,info],
			$deferIcon,
			{
				Lookup[info,"Name","Unnamed"]
				},
			KeyValueMap[
				BoxForm`MakeSummaryItem[{Row@{#,": "},#2},StandardForm]&,
				KeyDrop[info,"Name"]
				],
			StandardForm
			];


Clear@OFObjectQ;
OFObjectQ[_OFObject]:=True;
OFObjectQ[s_Symbol]:=
	MatchQ[OwnValues[s],{Verbatim[HoldPattern][HoldPattern[s]]:>_OFObject}];
OFObjectQ[HoldPattern@Part[_MessageName,_]]:=True;
OFObjectQ[(_MessageName)[]]:=True;
OFObjectQ[_]:=False;
OFObjectQ~SetAttributes~HoldFirst;


OFSetMessageNameLookup[]:=
	(
		Unprotect@MessageName;
		MessageName[s_,props:Except["usage"]..]/;
				!TrueQ@$messageNameOverride:=
			If[OFObjectQ@s,
				Fold[OFLookup[#,#2]&,s,{props}],
				Block[{$messageNameOverride=True},MessageName[s,props]]
				];
		Protect@MessageName;
		);


OFMessageNameOverload[function_[pat___],overload_[args___],else_[elargs___]]:=
	(
		Unprotect@MessageName;
		MessageName/:
			HoldPattern[
				function[MessageName[s_,props:Except["usage"]..],pat]/;
					!TrueQ@$messageNameOverride
				]:=
				If[OFObjectQ@s,
					With[{obj=Fold[OFLookup[#,#2]&,s,Most@{props}]},
						overload[obj,Last@{props},args]
						],
					Block[{$messageNameOverride=True},
						else[MessageName[s,props],elargs]
						]
					];
		Protect@MessageName;
		)
OFMessageNameOverload~SetAttributes~HoldAllComplete;


OFSetMessageNameSet[]:=(
	OFMessageNameOverload[Set[v_],OFSet[v],Set[v]];
	OFMessageNameOverload[SetDelayed[v_],OFSetDelayed[v],SetDelayed[v]];
	OFMessageNameOverload[Unset[v_],OFUnset[v],Unset[v]];
	OFMessageNameOverload[AddTo[v_],AddTo[v],AddTo[v]];
	OFMessageNameOverload[SubtractFrom[v_],SubtractFrom[v],SubtractFrom[v]];
	OFMessageNameOverload[TimesBy[v_],TimesBy[v],TimesBy[v]];
	)


OFCallPatternOverload[
	callargs___,function_[pat___],
	overload_[args___],else_[elargs___]]:=
	CompoundExpression[
		Unprotect@MessageName;
		MessageName/:
			HoldPattern[
				function[MessageName[s_,props:Except["usage"]..][callargs],pat]/;
					!TrueQ@$messageNameOverride
				]:=
				If[OFObjectQ@s,
					With[{obj=Fold[OFLookup[#,#2]&,s,Most@{props}]},
						overload[obj,Last@{props},args]
						],
					Block[{$messageNameOverride=True},
						else[MessageName[s,props],elargs]
						]
					];
		Protect@MessageName;
		];
OFCallPatternOverload~SetAttributes~HoldAllComplete;


OFSetCallPatternSet[]:=(
	OFCallPatternOverload[keys__,Set[v_],OFSetPart[keys,v],Hold[]];
	OFCallPatternOverload[keys__,SetDelayed[v_],OFSetPartDelayed[keys,v],Hold[]];
	);


OFIndexPatternOverload[indsPat___,function_[pat___],
	overload_[args___],else_[elargs___]]:=
	CompoundExpression[
		Unprotect@MessageName;
		Unprotect@Part;
		Part/:
			HoldPattern[
				function[Part[MessageName[s_,props:Except["usage"]..],indsPat],pat]/;
					!TrueQ@$messageNameOverride
				]:=
				If[OFObjectQ@s,
					With[{obj=Fold[OFLookup[#,#2]&,s,Most@{props}]},
						overload[obj,Last@{props},args]
						],
					Block[{$messageNameOverride=True},
						else[MessageName[s,props],elargs]
						]
					];
		Protect@MessageName;
		];
OFIndexPatternOverload~SetAttributes~HoldAllComplete;


OFSetIndexPatternSet[]:=
(
	OFIndexPatternOverload[i__,Set[v_],OFSetIndex[i,v],Hold[]];
	OFIndexPatternOverload[i__,SetDelayed[v_],OFSetIndex[i,v],Hold[]];
	)


OFSetMessageNameOverloading[]:=
	(
		OFSetMessageNameLookup[];
		OFSetMessageNameSet[];
		OFSetCallPatternSet[];
		OFSetIndexPatternSet[];
		)


If[!MatchQ[$OFConstructorStack,_List],
	$OFConstructorStack={}
	];


If[!MatchQ[$OFBuiltMethods,_Association],
	$OFBuiltMethods=<||>
	];


Options[OFBegin]={
	"Type"->"Object",
	"Construct"->"Type",
	"Assign"->None
	};
OFBegin[name:Except[_Rule|_RuleDelayed],ops:OptionsPattern[]]:=
	AppendTo[$OFConstructorStack,
		<|
			"Name"->
				Replace[name,None|Automatic:>CreateUUID[]],
			"Construct"->
				Replace[OptionValue["Construct"],Except["Object"]->"Type"],
			"Assign"->
				OptionValue["Assign"],
			"Fields"->
				<|
					"ObjectType"->
						Replace[OptionValue["Type"],
							_?(Not@KeyMemberQ[$OFObjectTable["Types"],#]&)->"Object"
							]
					|>,
			"Methods"->
				<|
					|>
			|>
		];
OFBegin~SetAttributes~HoldRest;


OFAddField[name_,value_]:=
	(
		AssociateTo[$OFConstructorStack[[-1,"Fields"]],
			name->value
			];
		);
OFAddFieldDelayed[name_,value_]:=
	(
		AssociateTo[$OFConstructorStack[[-1,"Fields"]],
			name:>value
			];
		);


OFMethodSymbol[name_,add:True|False:False]:=
	If[KeyMemberQ[$OFConstructorStack[[-1,"Methods"]],name],
		$OFConstructorStack[[-1,"Methods",name,"Symbol"]],
		If[add,
			OFAddMethod[name];
			OFMethodSymbol[name,False],
			Symbol[
				StringReplace[ToString[name],Except[WordCharacter|"$"]->""]<>
					"$"<>StringReplace[CreateUUID[],"-"->""]
				]
			]
		];


OFAddMethod[nameProvided_,data:_Association:<||>]:=
	With[{name=
		Replace[nameProvided,
			s_String:>
				Replace[
					StringReplace[s,{
						"__"~~l:WordCharacter..~~"__":>"Object"<>Capitalize@l,
						"__"~~l:WordCharacter..:>PrivateKey[l]
						}],
					StringExpression[__,p_PrivateKey]:>p
					]
			]},
		If[KeyMemberQ[$OFConstructorStack[[-1,"Methods"]],name],
			$OFConstructorStack[[-1,"Methods",name,"Properties"]]=
				Join[
					$OFConstructorStack[[-1,"Methods",name,"Properties"]],
					data
					],
			AssociateTo[$OFConstructorStack[[-1,"Methods"]],
				name->
					<|
						"Symbol"->OFMethodSymbol[name],
						"Properties"->data
						|>
				]
			]
		];


OFBuildMethod[f_,data_Association]:=
	With[{
		a=Lookup[data,"Attributes",{}],
		o=Lookup[data,"Options",{}]
		},
		If[MatchQ[f,_Symbol],
			Attributes[f]=a;
			Options[f]=o;
			];
		OFMethod[f,KeyDrop[data,{"Attributes","Options"}]]
		]


OFEnd[]:=
	If[Length@$OFConstructorStack>0,
		With[{a=Last@$OFConstructorStack},
			$OFConstructorStack=Delete[$OFConstructorStack,-1];
			With[{
				t=a["Construct"],
				s=a["Assign"],
				n=a["Name"],
				f=a["Fields"],
				m=OFBuildMethod[#Symbol,#Properties]&/@a["Methods"]
				},
				$OFBuiltMethods=
					Join[$OFBuiltMethods,
						AssociationMap[Null&,
							Values@a[["Methods",All,"Symbol"]]
							]
						];
				With[{obj=
					If[t=="Type",
						OFNewType[n,Join[f,m]];
						OFType[n],
						OFNew[Join[f,m,<|"ObjectType"->n|>]]
						]
					},
					If[s=!=None,s=obj,s]
					]
				]
			],
		None
		];


OFBegin/:
	HoldPattern[
		(h:Set|SetDelayed)[s_,
			OFBegin[name:Except[_Rule|_RuleDelayed]:None,ops:OptionsPattern[]]
			]
		]:=
		With[{
			n=Replace[name,
					None:>
						Replace[Unevaluated[s],{
							OFField[f_]:>ToString@Unevaluated@f,
							_:>ToString@Unevaluated@s
							}]
					]
			},
			If[MatchQ[Unevaluated[s],_OFField],h[s,OFType[n]]];
			OFBegin[
				n,
				ops,
				"Assign"->HoldPattern[s]]
			];


OFField/:
	HoldPattern[Set[OFField[f_],v:Except[_OFBegin]]]:=
		OFAddField[f,v];
OFField/:
	HoldPattern[SetDelayed[OFField[f_],v:Except[_OFBegin]]]:=
		OFAddFieldDelayed[f,v];


OFInit/:
	HoldPattern[(s:Set|SetDelayed)[OFInit[p___],code_]]:=
		s[OFMethod["ObjectInitialization"][p],code];
OFRepr/:
	HoldPattern[(s:Set|SetDelayed)[OFRepr[p___],code_]]:=
		s[OFMethod["ObjectRepresentation"][p],code];
OFStr/:
	HoldPattern[(s:Set|SetDelayed)[OFStr[p___],code_]]:=
		s[OFMethod["ObjectString"][p],code];


OFMethodOptions[m:_Symbol|_Sting]:=
	If[Length@$OFConstructorStack==0,
		Options[m],
		Lookup[$OFConstructorStack[[-1,"Methods",m,"Properties"]],"Options",{}]
		];


OFMethodSetOptions[m:_Symbol|_String,ops_]:=
	If[Length@$OFConstructorStack==0,
		If[MatchQ[ops,_List],
			SetOptions[m,Sequence@@ops],
			SetOptions[m,ops]
			],
		OFAddMethod[m,<|"Options"->ops|>]
		];


OFMethodAttributes[m:_Symbol|_String]:=
	If[Length@$OFConstructorStack==0,
		Attributes[m],
		Lookup[$OFConstructorStack[[-1,"Methods",m,"Properties"]],"Attributes",{}]
		];


OFMethodSetAttributes[m:_Symbol|_String,ops_]:=
	If[Length@$OFConstructorStack==0,
		SetAttributes[m,ops],
		OFAddMethod[m,<|"Attributes"->ops|>]
		];


OFMethodSetProperties[m:_Symbol|_String,ops_Association]:=
	If[Length@$OFConstructorStack>0,
		OFAddMethod[m,ops]
		];
OFMethodSetProperties[m:_Symbol|_String,ops_List]:=
	OFMethodSetProperties[m,Association@ops];
OFMethodSetProperties[m:_Symbol|_String,ops:(_Rule|_RuleDelayed)..]:=
	OFMethodSetProperties[m,<|ops|>];


OFMethodSetProperties[OFObject[uuid_],m_,props_Association]:=
	OFAdjust[uuid,m,
		Replace[{OFMethod[f_,a_Association]:>OFMethod[f,Join[a,props]]}]]
OFMethodSetProperties[OFObject[uuid_],m_,ops:(_Rule|_RuleDelayed)..]:=
		OFMethodSetProperties[OFObject[uuid],m,<|ops|>]


OFMethodSet[m:_Symbol|_String,p___,code_]:=
	If[Length@$OFConstructorStack==0,
		m[p]=code,
		With[{s=OFMethodSymbol[m,True]},
			s[p]=code
			];
		];


OFMethodSetDelayed[m:_Symbol|_String,p___,code_]:=
	If[Length@$OFConstructorStack==0,
		m[p]:=code,
		With[{s=OFMethodSymbol[m,True]},
			s[p]:=code
			];
		];
OFMethodSetDelayed~SetAttributes~HoldRest;


OFMethod/:
	HoldPattern[Options[OFMethod[m:_Symbol|_String]]]:=
		OFMethodOptions[m];
OFMethod/:
	HoldPattern[SetOptions[OFMethod[m:_Symbol|_String],ops__]]:=
		OFMethodSetOptions[m,Flatten@{ops}];
OFMethod/:
	HoldPattern[(Set|SetDelayed)[OFMethod[m:_Symbol|_String,"Options"],ops_]]:=
		OFMethodSetOptions[m,ops];
OFMethod/:
	HoldPattern[Attributes[OFMethod[m:_Symbol|_String]]]:=
		OFMethodAttributes[m];
OFMethod/:
	HoldPattern[SetAttributes[OFMethod[m:_Symbol|_String],attrs_]]:=
		OFMethodSetAttributes[m,attrs];
OFMethod/:
	HoldPattern[(Set|SetDelayed)[OFMethod[m:_Symbol|_String,"Attributes"],attrs_]]:=
		OFMethodSetAttributes[m,attrs];
OFMethod/:
	HoldPattern[(Set|SetDelayed)[OFMethod[m:_Symbol|_String,"Properties"],props_]]:=
		OFMethodSetProperties[m,props];
OFMethod/:
	HoldPattern[Set[OFMethod[m:_Symbol|_String][p___],code_]]:=
		OFMethodSet[m,p,code];
OFMethod/:
	HoldPattern[SetDelayed[OFMethod[m:_Symbol|_String][p___],code_]]:=
		OFMethodSetDelayed[m,p,code];


End[];



