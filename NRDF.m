
BeginPackage["NRDF`", {"RunFiles`", "DataTable`", "Memo`", "Piraha`", "ReadHDF5`", "Waveforms`"}];

ParseMetadataFile;
StartingFrequency;
UsefulWaveformTime;

Begin["`Private`"];

RunDirectory = Global`RunDirectory;
BackingDirectory = Global`BackingDirectory;

ErrorDefinition[x_] := 
  x[args___] := 
   Throw["Invalid arguments in " <> ToString[x] <> "[" <> 
     StringJoin[Riffle[ToString[#, InputForm] & /@ {args}, ", "]] <> "]", 
    InvalidArguments];

SetAttributes[DefFn, HoldAll];
DefFn[def : (fn_[args___] := body_)] := Module[{}, ErrorDefinition[fn];
   fn[args] :=(*Profile[fn,*)body(*]*)];

FPrint[x_] := (Print[x//InputForm]; x);

NRDF`RunFiles`HaveData[runDir_] :=
  haveRunDir[runDir];
  (* FileExistsQ[FileNameJoin[{RunDirectory,runDir,FileNameTake[runDir,-1]<>".bbh"}]]; *)

NRDF`Waveforms`HaveData[runDir_,___] :=
  NRDF`RunFiles`HaveData[runDir];

NRDF`InitialData`HaveData[runDir_,___] :=
  NRDF`RunFiles`HaveData[runDir];

DefFn[addDataSubDir[output_String] :=
  Module[
    {parFiles,runName},

    runName = FileNameSplit[output][[-2]];
    dir = FileNameJoin[{output,runName}];
    If[FileType[dir] === Directory,
       Return[dir]];

    parFiles = FileNames["*/*.par", {output}, 2];
    If[Length[parFiles] === 0, Return[None]];
    If[Length[parFiles] =!= 1, Throw["Found more than one */*.par in " <> output]];
    FileNameJoin[Drop[FileNameSplit[parFiles[[1]]],-1]]]];

NRDF`RunFiles`FindRunDirSegments[dir_] :=
  {findRunDir[dir]};

DefineMemoFunction[ParseMetadataFile[run_String],
  CleanParseTree[Parse["nrdfmd.peg","file",findMetadataFile[run]]]];

DefFn[processMetadata[md_] :=
  md /. {"keyword"[k_String] :> "keyword"[ToLowerCase[k]],
         "key"[k_String] :> "key"[ToLowerCase[k]]}];

NRDF`Waveforms`ReadPsi4RadiiStrings[runName_] :=
  Module[
    {md, radii, radStrs},
    md = ParseMetadataFile[runName];
    Put[md, "~/metadata.m"];

    md = processMetadata[md];

    radii = Cases[md, 
                  "section"[___, "section_name"["keyword"["psi4t-data"]], ___, 
                            "elements"[___, 
                                       "element"["key"["extraction-radius"], val_], ___], ___] :> 
                  val, Infinity];

    radii = Select[radii, (!MatchQ[#, "value"["keyword"["extrapolated"|"infinite"]]]) &];

    If[Length[radii] === 0,
       Print["No Psi4 radii found in metadata file of "<>runName];
       {}];

    Map[If[!MatchQ[#,"value"["number"[n_]]],
           Throw["Unrecognised radius "<>ToString[#,InputForm]]] &, radii];
    radStrs = radii/."value"["number"[n_]] -> n;
    radStrs
  ];

DefFn[readPsi4HDF5Data[file_String, dataset_String] :=
  MakeDataTable[
    Map[{#[[1]], #[[2]] + I #[[3]]} &,
        ReadHDF5[file, {"Datasets", dataset}]]]];

DefFn[findRunDir[run_String] :=
  If[FileExistsQ[run], run,
     If[FileExistsQ[FileNameJoin[{RunDirectory,run}]],
        FileNameJoin[{RunDirectory,run}],
        None]]];

DefFn[haveMetadataFile[dir_String] :=
  FileNames["*.bbh", dir] =!= {}];

DefFn[findMetadataFile[run_String] :=
      Module[{files},
             files = FileNames["*.bbh", findRunDir[run]];
             If[Length[files] =!= 1, Throw["Failed to find exactly one metadata file in run "<>
                                           run<>" in directory "<>findRunDir[run]]];
             files[[1]]]];

DefFn[haveRunDir[run_String] :=
  If[haveMetadataFile[run],
     True,
     If[haveMetadataFile[FileNameJoin[{RunDirectory, run}]],
        True,
        False]]];

DefFn[
  ensureLocalFile[run_String, file_String] :=
  Module[
    {src,dst},
    (* Print["ensureLocalFile: run = ", run]; *)
    (* Print["ensureLocalFile: file = ", file]; *)
    src = FileNameJoin[{BackingDirectory,run,file}];
    dst = FileNameJoin[{findRunDir[run],file}];
    (* Print["ensureLocalFile: src = ", src]; *)
    (* Print["ensureLocalFile: dst = ", dst]; *)
    syncFile[src,dst]]];

DefFn[
  syncFile[src_String, dst_String] :=
  If[FileType[dst] === None,
     Print[StringForm["Syncing file `1` to `2`", src, dst]];
     output = ReadList["! rsync -avz "<>src<>" "<>dst<>" >>rsync.log 2>&1 </dev/null", String];
     log[StringJoin[Riffle[output,"\n"]]];
     If[FileType[dst] === None,
        Throw["File " <> src <> " could not be downloaded"]]]];

NRDF`Waveforms`ReadPsi4Data[runName_, l_?NumberQ, m_?NumberQ, rad_] :=
  Module[
    {md, filenames, filename, tmp,data},
    md = ParseMetadataFile[runName];
    md = processMetadata[md];
    filenames = Cases[md,
                      "section"[___, "section_name"["keyword"["psi4t-data"]], ___,
                                "elements"[___,
                                           "element"["key"["extraction-radius"], "value"["number"[rad]]], ___,
                                           "element"["key"["2,2"|"2,+2"],"string"[f_]],___],
                                ___] :> f,
                      Infinity];

    If[Length[filenames] === 0, Throw["Cannot find filename in metadata file for 2,2 mode in "<>runName]];
    If[Length[filenames] > 1, Throw["Found multiple files in metadata file for 2,2 mode in "<>runName]];

    (* Print["run dir = ", findRunDir[runName]]; *)

    filename = filenames[[1]];

    (* Print["filename = ", filename]; *)

    (* Print["NRDF`Waveforms`ReadPsi4Data: runName = ", runName]; *)

    tmp = StringCases[filename, base__~~".h5:"~~ds__ :> {base<>".h5",ds}];
    data =
    If[Length[tmp] === 0,
       ensureLocalFile[runName, filenames[[1]]];
       ReadWaveformFile[FileNameJoin[{findRunDir[runName], filename}]],
       (* else *)
       ensureLocalFile[runName, tmp[[1,1]]];
       readPsi4HDF5Data[FileNameJoin[{findRunDir[runName], tmp[[1,1]]}], tmp[[1,2]]]];

    (* Print["data = ", data]; *)

    data];

DefFn[
  ReadMetadataKey[run_String, keyPattern_] :=
  Module[
    {md, filenames, filename, tmp, results},
    md = ParseMetadataFile[run];
    results = Cases[md,
                 "section"[___, 
                           "section_name"["keyword"["metadata"]],
                           ___,
                           "elements"[___,
                                      "element"["key"[keyPattern], v_], 
                                      ___]] :> v,
                 Infinity];

    If[Length[results] =!= 1, Throw["Did not find exactly one metadata key for "<>ToString[keyPattern,InputForm]<>" in "<>run]];

    Which[
      MatchQ[results[[1]], "value"["number"[_]]],
      ImportString[results[[1,1,1]], "Table"][[1,1]],

      MatchQ[results[[1]], "value"["keyword"[_]]],
      results[[1,1,1]],

      True,
      Throw["Unsupported metadata type for "<>ToString[keyPattern,InputForm]<>": "<>ToString[results[[1]],InputForm]]]]];

DefFn[NRDF`InitialData`ReadADMMass[run_String] :=
  ReadMetadataKey[run, "initial-ADM-energy"]];

DefFn[
  StartingFrequency[run_] :=
  ReadMetadataKey[run, "initial-freq-22"|"freq-start-22"]];

DefFn[
  UsefulWaveformTime[run_] :=
  ReadMetadataKey[run, "after-junkradiation-time"]];

End[];

EndPackage[];
