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
  "SimulationTools`"
 }];

ParseMetadataFile;
StartingFrequency;
UsefulWaveformTime;
HaveInfiniteRadiusWaveforms;
ReadMetadataKey;
HaveMetadataKey;
ReadRuns;
ReadResolution;
ExpectedConvergenceOrder;
ReadMasses;
ReadMassRatio;
ReadTotalMass;
NameEmailList;
EmailList;
ReadInitialBHPosition;
ReadInitialBHMomentum;
ReadInitialBHVelocity;
HaveInitialBHMomentum;
HaveInitialBHVelocity;
ReadInitialBHSpin;
ReadInitialBHMassParameter;
ReadBHSpin;
ReadBHMass;
ReadBHCoordinatesNRDF;
ReadBHUsefulSpin;
ParseMetadataFileFromName;

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
  Module[{mdFile},
    mdFile = If[FileType[run] === File, run, findMetadataFile[run]];
    CleanParseTree[ParsePEG["nrdfmd.peg","file",mdFile]]]];

ParseMetadataFileFromName[fileName_String] :=
  CleanParseTree[ParsePEG["nrdfmd.peg","file",fileName]];


processMetadata[md_] :=
  md /. {"keyword"[k_String] :> "keyword"[ToLowerCase[k]],
         "key"[k_String] :> "key"[ToLowerCase[k]]};

SimulationTools`NRDF`Waveforms`ReadPsi4RadiiStrings[runName_] :=
  Module[
    {md, radii, radStrs},
    md = ParseMetadataFile[runName];

    md = processMetadata[md];

    radii = Cases[md, 
                  "section"[___, "section_name"["keyword"["psi4t-data"]], ___, 
                            "elements"[___, 
                                       "element"["key"["extraction-radius"], val_], ___], ___] :> 
                  val, Infinity];

    radii = Select[radii, (!MatchQ[#, "value"["keyword"["extrapolated"]]]) &];

    If[Length[radii] === 0,
       (* Print["No Psi4 radii found in metadata file of "<>runName]; *)
       {}];

    Map[If[!MatchQ[#,"value"["number"[n_]|"keyword"["infinite"]]],
           Error["Unrecognised radius "<>ToString[#,InputForm]]] &, radii];
    radStrs = radii/.{"value"["number"[n_]] -> n, "value"["keyword"["infinite"]] -> "inf"};
    radStrs
  ];

DefineMemoFunction[NRDF`Waveforms`ReadPsi4Modes[runName_],
  Module[
    {md, sections, keys, modes},
    md = ParseMetadataFile[runName];
    md = processMetadata[md];
    sections = Cases[md,
                  "section"[___, "section_name"["keyword"["psi4t-data"]], contents___] :> contents
                  , Infinity];
    keys = Cases[sections, "element"["key"[k_], v_] :> k, Infinity];
    modes = Select[keys, StringMatchQ[#,NumberString ~~ "," ~~ Whitespace... ~~ NumberString] &];
    modes = Map[Map[ToExpression, StringSplit[#, ","]] &, modes];
    Union[modes]]];

readPsi4HDF5Data[file_String, dataset_String] :=
 Module[{data},
  data = ReadHDF5[file, {"Datasets", dataset}];
  ToDataTable[data[[All, 1]], Complex @@@ data[[All, 2;;3]] /.
      {(Complex[Indeterminate,_]|Complex[_,Indeterminate]) -> Indeterminate}]
];

haveMetadataFile[dir_String] :=
  FileNames["metadata.txt" | (__ ~~ ".bbh"), dir] =!= {};

findMetadataFile[run_String] :=
      Module[{files},
             files = FileNames["metadata.txt" | (__ ~~ ".bbh"), FindRunDir[run],1];
             If[Length[files] =!= 1, Error["Failed to find exactly one metadata file in run "<>
                                           run<>" in directory "<>FindRunDir[run]]];
             files[[1]]];

haveRunDir[run_String] :=
  If[haveMetadataFile[FindRunDir[run]],
     True,
     False];

Options[SimulationTools`NRDF`Waveforms`ReadPsi4Data] = {"Section" -> "psi4t-data"};

SimulationTools`NRDF`Waveforms`ReadPsi4Data[runName_, l_?NumberQ, m_?NumberQ, rad_String,
                            opts:OptionsPattern[]] :=
  Module[
    {md, filenames, filename, tmp,data, radPattern},
    md = ParseMetadataFile[runName];
    md = processMetadata[md];
    radPattern = If[rad==="inf", "keyword"["infinite"], "number"[rad]];
    filenames = Cases[md,
                      "section"[___, "section_name"["keyword"[OptionValue[Section]]], ___,
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
       ReadWaveformFile[FileNameJoin[{FindRunDir[runName], filename}]],
       (* else *)
       readPsi4HDF5Data[FileNameJoin[{FindRunDir[runName], tmp[[1,1]]}], tmp[[1,2]]]];

    (* Print["data = ", data]; *)

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

    Check[Interpolation[data], Print["Warning: Duplicate points in "<>runName<>"/"<>tmp], Interpolation::inddp];

    If[rad=!="inf",
       Module[
         {absMax,radNum},
         radNum = ImportString[rad,"List"][[1]];
         absMax = radNum Max[Abs[data]];
         If[absMax > 1.0,
            (* Print["WARNING: dividing stored data by quoted radius"]; *)
            data = data / radNum]]];

    data];

ReadMetadataKey[run_String, keyPattern_] :=
  ReadMetadataKey[run, "metadata", keyPattern];

ReadMetadataKey[run_String, section_String, keyPattern_] :=
  Module[
    {md, results},
    md = ParseMetadataFile[run];

    results = Cases[md,
                 "section"[___, 
                           "section_name"["keyword"[section]],
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

      MatchQ[results[[1]], "value"["name_email_list"[___]]],
      NameEmailList@@results[[1,1]] /. "name_email"["pname"[pname_],"email"[email_]] :> {pname,email},

      MatchQ[results[[1]], "value"["email_list"[___]]],
      EmailList@@results[[1,1]] /. "email"[email_] :> email,

      True,
      Error["Unsupported metadata type for "<>ToString[keyPattern,InputForm]<>": "<>ToString[results[[1]],InputForm]]]];

HaveMetadataKey[run_String, keyPattern_] :=
  HaveMetadataKey[run, "metadata", keyPattern];

HaveMetadataKey[run_String, section_String, keyPattern_] :=
  Module[
    {md, results},
    md = ParseMetadataFile[run];
    results = Cases[md,
                 "section"[___, 
                           "section_name"["keyword"[section]],
                           ___,
                           "elements"[___,
                                      "element"["key"[keyPattern], v_], 
                                      ___]] :> v,
                 Infinity];

    Length[results] === 1];

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

ReadMasses[run_String] :=
  {ReadMetadataKey[run, "mass1"],ReadMetadataKey[run, "mass2"]};

ReadMassRatio[run_String] :=
  Divide@@ReadMasses[run];

ReadTotalMass[run_String] :=
  Plus@@ReadMasses[run];

ReadInitialBHPosition[run_String, i:(1|2)] :=
  Table[ReadMetadataKey[run,"initial-bh-position"<>ToString[i]<>d], {d,{"x","y","z"}}];

ReadInitialBHMomentum[run_String, i:(1|2)] :=
  Table[ReadMetadataKey[run,"initial-bh-momentum"<>ToString[i]<>d], {d,{"x","y","z"}}];

ReadInitialBHVelocity[run_String, i:(1|2)] :=
  Table[ReadMetadataKey[run,"initial-bh-velocity"<>ToString[i]<>d], {d,{"x","y","z"}}];

ReadInitialBHSpin[run_String, i:(1|2)] :=
  Table[ReadMetadataKey[run,"initial-bh-spin"<>ToString[i]<>d], {d,{"x","y","z"}}];

ReadBHUsefulSpin[run_String, i:(1|2)] :=
  Table[ReadMetadataKey[run,"after-junkradiation-spin"<>ToString[i]<>d], {d,{"x","y","z"}}];

HaveInitialBHMomentum[run_String] :=
  HaveMetadataKey[run, "initial-bh-momentum1x"];

HaveInitialBHVelocity[run_String] :=
  HaveMetadataKey[run, "initial-bh-velocity1x"];

ReadInitialBHMassParameter[run_String,i:(1|2)] :=
  ReadMetadataKey[run, "initial-bh-mass-parameter"<>ToString[i]];

ReadBHSpin[run_String, i:(1|2)] :=
  Module[{base,file,data},
    base = ReadMetadataKey[run, "body-data", "spin"<>ToString[i]];
    file = FileNameJoin[{FindRunDir[run], base}];
    data = readColumnData[file];
    Table[MakeDataTable[data[[All,{1,d+1}]]], {d,1,3}]];

readColumnData[file_String] :=
  Module[
    {tmp},
    tmp = StringCases[file, base__~~".h5:"~~ds__ :> {base<>".h5",ds}];
    If[Length[tmp] === 0,
       ReadColumnFile[file],
       ReadHDF5[tmp[[1,1]], {"Datasets", tmp[[1,2]]}]]];

SimulationTools`MinTracker`Trackers`ReadCoordinates[runName_String, trackers_List] :=
  Table[SimulationTools`MinTracker`Trackers`ReadCoordinates[runName, t], {t, trackers}];

SimulationTools`NRDF`Trackers`ReadCoordinates[run_String, i_Integer] :=
  Module[{base,file,data},
    base = ReadMetadataKey[run, "body-data", "trajectory"<>ToString[i]];
    file = FileNameJoin[{FindRunDir[run], base}];
    data = readColumnData[file];
    Table[MakeDataTable[data[[All,{1,d+1}]]], {d,1,3}]];

ReadBHCoordinatesNRDF = SimulationTools`NRDF`Trackers`ReadCoordinates;

SimulationTools`NRDF`BHCoordinates`HaveData[run_String, tracker_Integer] :=
  haveMetadataFile[FindRunDir[run]] && HaveMetadataKey[run, "body-data", "trajectory"<>ToString[tracker]];

ReadBHMass[run_String, i_Integer] :=
  Module[{base,file,data},
    base = ReadMetadataKey[run, "body-data", "horizon-mass"<>ToString[i]];
    file = FileNameJoin[{FindRunDir[run], base}];
    data = readColumnData[file];
    MakeDataTable[data[[All,{1,2}]]]];

HaveBHCoordinatesNRDF[run_String, tracker_Integer] :=
  HaveMetadataKey[run, "body-data", "trajectory"<>ToString[tracker]];

End[];

EndPackage[];
