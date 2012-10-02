(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["SimulationTools`NRDF`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "Piraha`",
  "SimulationTools`ReadHDF5`",
  "SimulationTools`RunFiles`",
  "SimulationTools`Waveforms`",
  "SimulationTools`Utils`"
 }];

ParseMetadataFile;
StartingFrequency;
UsefulWaveformTime;
HaveInfiniteRadiusWaveforms;
ReadMetadataKey;
ReadRuns;
ReadResolution;
ExpectedConvergenceOrder;

Begin["`Private`"];

FPrint[x_] := (Print[x//InputForm]; x);

SimulationTools`NRDF`RunFiles`HaveData[runDir_String,___] :=
  haveRunDir[runDir];

SimulationTools`NRDF`Waveforms`HaveData[runDir_,___] :=
  SimulationTools`NRDF`RunFiles`HaveData[runDir];

SimulationTools`NRDF`InitialData`HaveData[runDir_,___] :=
  SimulationTools`NRDF`RunFiles`HaveData[runDir];

SimulationTools`NRDF`RunFiles`FindRunDirSegments[dir_] :=
  {FindRunDir[dir]};

DefineMemoFunction[ParseMetadataFile[run_String],
  CleanParseTree[Parse["nrdfmd.peg","file",findMetadataFile[run]]]];

processMetadata[md_] :=
  md /. {"keyword"[k_String] :> "keyword"[ToLowerCase[k]],
         "key"[k_String] :> "key"[ToLowerCase[k]]};

SimulationTools`NRDF`Waveforms`ReadPsi4RadiiStrings[runName_] :=
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

    radii = Select[radii, (!MatchQ[#, "value"["keyword"["extrapolated"]]]) &];

    If[Length[radii] === 0,
       Print["No Psi4 radii found in metadata file of "<>runName];
       {}];

    Map[If[!MatchQ[#,"value"["number"[n_]|"keyword"["infinite"]]],
           Error["Unrecognised radius "<>ToString[#,InputForm]]] &, radii];
    radStrs = radii/.{"value"["number"[n_]] -> n, "value"["keyword"["infinite"]] -> "inf"};
    radStrs
  ];

readPsi4HDF5Data[file_String, dataset_String] :=
  MakeDataTable[
    Map[{#[[1]], #[[2]] + I #[[3]]} &,
        ReadHDF5[file, {"Datasets", dataset}]]];

haveMetadataFile[dir_String] :=
  FileNames["*.bbh", dir] =!= {};

findMetadataFile[run_String] :=
      Module[{files},
             files = FileNames["*.bbh", FindRunDir[run]];
             If[Length[files] =!= 1, Error["Failed to find exactly one metadata file in run "<>
                                           run<>" in directory "<>FindRunDir[run]]];
             files[[1]]];

haveRunDir[run_String] :=
  If[haveMetadataFile[FindRunDir[run]],
     True,
     False];


ensureLocalFile[run_String, file_String] :=
  Module[
    {src,dst},
    (* Print["ensureLocalFile: run = ", run]; *)
    (* Print["ensureLocalFile: file = ", file]; *)
    dst = FileNameJoin[{FindRunDir[run],file}];
    If[FileExistsQ[dst], Return[Null]];
    If[!ValueQ[Global`BackingDirectory],
       Error["Cannot find file "<>file<>" in run "<>run<>" and BackingDirectory has not been set"]];
    src = FileNameJoin[{Global`BackingDirectory,run,file}];
    (* Print["ensureLocalFile: src = ", src]; *)
    (* Print["ensureLocalFile: dst = ", dst]; *)
    syncFile[src,dst]];


syncFile[src_String, dst_String] :=
  Module[{output},
  If[FileType[dst] === None,
     Print[StringForm["Syncing file `1` to `2`", src, dst]];
     output = ReadList["! rsync -avz "<>src<>" "<>dst<>" >>rsync.log 2>&1 </dev/null", String];
     log[StringJoin[Riffle[output,"\n"]]];
     If[FileType[dst] === None,
        Error["File " <> src <> " could not be downloaded"]]]];

SimulationTools`NRDF`Waveforms`ReadPsi4Data[runName_, l_?NumberQ, m_?NumberQ, rad_String] :=
  Module[
    {md, filenames, filename, tmp,data, radPattern},
    md = ParseMetadataFile[runName];
    md = processMetadata[md];
    radPattern = If[rad==="inf", "keyword"["infinite"], "number"[rad]];
    filenames = Cases[md,
                      "section"[___, "section_name"["keyword"["psi4t-data"]], ___,
                                "elements"[___,
                                           "element"["key"["extraction-radius"], "value"[radPattern]], ___,
                                           "element"["key"["2,2"|"2,+2"|"2, 2"],"string"[f_]],___],
                                ___] :> f,
                      Infinity];

    If[Length[filenames] === 0, Error["Cannot find filename in metadata file for 2,2 mode in "<>runName]];
    If[Length[filenames] > 1, Error["Found multiple files in metadata file for 2,2 mode in "<>runName]];

    (* Print["run dir = ", FindRunDir[runName]]; *)

    filename = filenames[[1]];

    (* Print["filename = ", filename]; *)

    (* Print["SimulationTools`NRDF`Waveforms`ReadPsi4Data: runName = ", runName]; *)

    tmp = StringCases[filename, base__~~".h5:"~~ds__ :> {base<>".h5",ds}];
    data =
    If[Length[tmp] === 0,
       ensureLocalFile[runName, filenames[[1]]];
       ReadWaveformFile[FileNameJoin[{FindRunDir[runName], filename}]],
       (* else *)
       ensureLocalFile[runName, tmp[[1,1]]];
       readPsi4HDF5Data[FileNameJoin[{FindRunDir[runName], tmp[[1,1]]}], tmp[[1,2]]]];

    (* Print["data = ", data]; *)

    Check[Interpolation[data], Print["Warning: Duplicate points in "<>runName<>"/"<>tmp], Interpolation::inddp];

    Module[
      {tol = 10.^-5},
      If[!MonotonicQ[data, tol],
         Module[
           {posns = Position[Prepend[Drop[Drop[RotateLeft[IndVar[data]] - IndVar[data],1],-1],1.0], 
                             _?(Abs[#]<tol &)],
            pos},
           pos = posns[[1,1]];
           data = MakeDataTable[Delete[ToList@data, posns]];
           Print["Warning: Data in "<>runName<>"/"<>ToString[tmp,InputForm]<>" is not monotonic (tolerance "<>
                 ToString[tol,InputForm]<>", index "<>ToString[pos+1]<>", t = "<>ToString[IndVar[data][[pos+1]]]<>")"]]]];

    If[rad=!="inf",
       Module[
         {absMax,radNum},
         radNum = ImportString[rad,"List"][[1]];
         absMax = radNum Max[Abs[data]];
         If[absMax > 1.0,
            data = data / radNum]]];

    data];


ReadMetadataKey[run_String, keyPattern_] :=
  Module[
    {md, results},
    md = ParseMetadataFile[run];
    results = Cases[md,
                 "section"[___, 
                           "section_name"["keyword"["metadata"]],
                           ___,
                           "elements"[___,
                                      "element"["key"[keyPattern], v_], 
                                      ___]] :> v,
                 Infinity];

    If[Length[results] =!= 1, Error["Did not find exactly one metadata key for "<>ToString[keyPattern,InputForm]<>" in "<>run]];

    Which[
      MatchQ[results[[1]], "value"["number"[_]]],
      ImportString[results[[1,1,1]], "Table"][[1,1]],

      MatchQ[results[[1]], "value"["keyword"[_]]],
      results[[1,1,1]],

      MatchQ[results[[1]], "string"[_]],
      results[[1,1]],

      True,
      Error["Unsupported metadata type for "<>ToString[keyPattern,InputForm]<>": "<>ToString[results[[1]],InputForm]]]];

SimulationTools`NRDF`InitialData`ReadADMMass[run_String] :=
  ReadMetadataKey[run, "initial-ADM-energy"];

StartingFrequency[run_] :=
  ReadMetadataKey[run, "initial-freq-22"|"freq-start-22"];

UsefulWaveformTime[run_] :=
  ReadMetadataKey[run, "after-junkradiation-time"];

HaveInfiniteRadiusWaveforms[run_] :=
  StringMatchQ[ReadMetadataKey[run, "extraction-radius"], "*infinite*"];

ReadRuns[dirp_:Automatic] :=
  Module[{dir},
    dir = If[dirp === Automatic,
             If[Length[SimulationPath[]]===1, First[SimulationPath[]], "."],
             dirp];
    Map[FileNameDrop[FileNameDrop[#,-1],Length[FileNameSplit[dir]]] &,FileNames["*/*/*/*.bbh", dir]]];

ReadResolution[run_String] :=
  ReadMetadataKey[run, "resolution"];

ExpectedConvergenceOrder[run_String] :=
  ReadMetadataKey[run, "resolution-expected-order"];

End[];

EndPackage[];
