
BeginPackage["NRDF`", {"RunFiles`", "DataTable`", "Memo`", "Piraha`", "ReadHDF5`", "Waveforms`"}];

ParseMetadataFile;

Begin["`Private`"];

nrdfDir = "/Users/ian/Projects/nrar/nrardata";

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
  (* FileExistsQ[FileNameJoin[{nrdfDir,runDir,FileNameTake[runDir,-1]<>".bbh"}]]; *)

NRDF`Waveforms`HaveData[runDir_,___] :=
  NRDF`RunFiles`HaveData[runDir];

NRDF`InitialData`HaveData[runDir_,___] :=
  NRDF`RunFiles`HaveData[runDir];

addDataSubDir[output_String] :=
  Module[
    {parFiles,runName},

    runName = FileNameSplit[output][[-2]];
    dir = FileNameJoin[{output,runName}];
    If[FileType[dir] === Directory,
       Return[dir]];

    parFiles = FileNames["*/*.par", {output}, 2];
    If[Length[parFiles] === 0, Return[None]];
    If[Length[parFiles] =!= 1, Throw["Found more than one */*.par in " <> output]];
    FileNameJoin[Drop[FileNameSplit[parFiles[[1]]],-1]]];

NRDF`RunFiles`FindRunDirSegments[dir_] :=
  {findRunDir[dir]};

DefineMemoFunction[ParseMetadataFile[run_String],
  CleanParseTree[Parse["nrdfmd.peg","file",FileNameJoin[{findRunDir[run], FileNameTake[run,-1]<>".bbh"}]]]];

processMetadata[md_] :=
  md /. {"keyword"[k_String] :> "keyword"[ToLowerCase[k]],
         "key"[k_String] :> "key"[ToLowerCase[k]]};

NRDF`Waveforms`ReadPsi4RadiiStrings[runName_] :=
  Module[
    {md, radii, radStrs},
    md = ParseMetadataFile[runName];

    md = processMetadata[md];

    radii = Cases[md, 
                  "section"[___, "section_name"["keyword"["psi4t-data"]], ___, 
                            "elements"[___, 
                                       "element"["key"["extraction-radius"], val_], ___], ___] :> 
                  val, Infinity];

    If[Length[radii] === 0,
       Throw["No Psi4 radii found in metadata file of "<>runName]];

    radii = Select[radii, (!MatchQ[#, "value"["keyword"["extrapolated"|"infinite"]]]) &];

    Map[If[!MatchQ[#,"value"["number"[n_]]],
           Throw["Unrecognised radius "<>ToString[#,InputForm]]] &, radii];
    radStrs = radii/."value"["number"[n_]] -> n;
    radStrs
  ];

readPsi4HDF5Data[file_String, dataset_String] :=
  MakeDataTable[
    Map[{#[[1]], #[[2]] + I #[[3]]} &,
        ReadHDF5[file, {"Datasets", dataset}]]];

findRunDir[run_String] :=
  If[FileExistsQ[run], run,
     If[FileExistsQ[FileNameJoin[{nrdfDir,run}]],
        FileNameJoin[{nrdfDir,run}],
        None]];

haveRunDir[run_String] :=
  If[FileExistsQ[FileNameJoin[{run,FileNameTake[run,-1]<>".bbh"}]],
     True,
     If[FileExistsQ[FileNameJoin[{nrdfDir, run,FileNameTake[run,-1]<>".bbh"}]],
        True,
        False]];

DefFn[
  ensureLocalFile[run_String, file_String] :=
  Module[
    {src,dst},
    src = FileNameJoin[{"io1:/scratch/ianhin/nrar/data",run,file}];
    dst = FileNameJoin[{findRunDir[run],file}];
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
    {md, filenames, filename, tmp},
    md = ParseMetadataFile[runName];
    md = processMetadata[md];
    filenames = Cases[md,
                      "section"[___, "section_name"["keyword"["psi4t-data"]], ___,
                                "elements"[___,
                                           "element"["key"["extraction-radius"], "value"["number"[rad]]], ___,
                                           "element"["key"["2,2"],"string"[f_]],___],
                                ___] :> f,
                      Infinity];

    If[Length[filenames] === 0, Throw["Cannot find file for 2,2 mode in "<>runName]];
    If[Length[filenames] > 1, Throw["Found multiple files for 2,2 mode in "<>runName]];

    (* Print["run dir = ", findRunDir[runName]]; *)

    filename = FileNameJoin[{findRunDir[runName],filenames[[1]]}];

    (* Print["filename = ", filename]; *)

    tmp = StringCases[filename, base__~~".h5:"~~ds__ :> {base<>".h5",ds}];
    data =
    If[Length[tmp] === 0,
       ensureLocalFile[runName, filenames[[1]]];
       ReadWaveformFile[filename],
       readPsi4HDF5Data[tmp[[1,1]], tmp[[1,2]]]]];

NRDF`InitialData`ReadADMMass[run_] :=
  Module[
    {md, filenames, filename, tmp},
    md = ParseMetadataFile[run];
    Put[md, "~/metadata.m"];
    adms = Cases[md,
                 "section"[___, 
                           "section_name"["keyword"["metadata"]],
                           ___,
                           "elements"[___,
                                      "element"["key"["initial-ADM-energy"], "value"["number"[e_]]], 
                                      ___]] :> e,
                 Infinity];

    If[Length[adms] === 0, Throw["NRDF`InitialData`ReadADMMass: Cannot find ADM mass in "<>run]];
    If[Length[adms] > 1, Throw["Found multiple ADM masses in "<>run]];
    ImportString[adms[[1]], "Table"][[1,1]]];

End[];

EndPackage[];
