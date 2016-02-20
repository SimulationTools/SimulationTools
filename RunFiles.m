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

BeginPackage["SimulationTools`RunFiles`",
 {
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`ProfileCall`",
  "SimulationTools`Providers`",
  "SimulationTools`"
 }];

FindSimulationFiles::usage = "FindSimulationFiles[simname, filename] gives all the files with the given name across all the segments of a simulation.  filename can be a string, a string expression or a regular expression.  This function always returns full pathnames.";
SimulationNames::usage = "SimulationNames[] lists all simulations." <>
  "SimulationNames[form] lists all simulations whose names match the string pattern form.";

HaveRunDir;

(* Deprecated *)
FindRunSegments;
FindRunDir;
FindRunFile;
FindRunFilesFromPattern;
FindFirstRunFile;
FileIsInRun;

(* TODO: Convert these to strings and make them options to FindRunFiles *)
FullFilenames;
LeafNamesOnly;

(* Old names *)
FindRunFiles = FindSimulationFiles;

Begin["`Private`"];

If[Head[FileNameJoin[{"a","b"}]] === FileNameJoin,

(* FileNameJoin (available since v7) is not defined.
   $PathnameSeparator has been available since v2, according to the
   documentation. *)

FileNameJoin[elems_] :=
  StringJoin[Riffle[elems, $PathnameSeparator]];

FileNameDepth[s_] :=
  Length[StringCases[s,$PathnameSeparator]] + 1;

FileNameTake[s_] :=
  Last[StringSplit[s, $PathnameSeparator]];

]; 

(*--------------------------------------------------------------------
  Finding run directories
  --------------------------------------------------------------------*)

HaveRunDir[runName_String] :=
  findRunDir[runName] =!= None;

(* Given the name of a run directory, return a path to it *)
findRunDir[runNamep_String] :=
 Module[{dir, dirs},
  If[DirectoryQ[runNamep], Return[runNamep]];

  If[StringTake[runNamep,1] === "/", Return[None]];

  dirs = SimulationNames[runNamep, "FullPath" -> True];
  Which[
   Length[dirs]>1,
    Error["Multiple runs called "<>runNamep<>" found: "<>ToString[dirs]];,
   Length[dirs] === 1,
    dir = First[dirs];,
   Length[dirs] === 0,
    dir = None;
  ];
  dir
];

FindRunDir[runName_String] :=
  Module[
    {d},

    d = findRunDir[runName];
    If[d =!= None, Return[d]];

    (* Handle the case of the PSU merged run directory *)
    d = findRunDir[runName<>"-all"];
    If[d =!= None, Return[d]];

    Error["Cannot find run "<>runName]
];

(*--------------------------------------------------------------------
  Finding segments in run directories
  --------------------------------------------------------------------*)

FindRunSegments[runName_] :=
  FindRunDirSegments[FindRunDir[runName]];

FindRunDirSegments[runDir_] :=
  If[HaveData["RunFiles", runDir],
     CallProvidedFunction["RunFiles","FindRunDirSegments",{runDir}],
     (* else *)
     If[FileType[runDir] === Directory,
     {runDir},
        Error[runDir<>" is not a directory or simulation"]]];

(*--------------------------------------------------------------------
  Finding files from runs
  --------------------------------------------------------------------*)

DocumentationBuilder`SymbolDescription["FindSimulationFiles"] =
  "find instances of a specific file across all segments of a simulation";

FindRunFiles[run_String, filename_String] := FindRunFile[run, filename];
FindRunFiles[run_String, filename:(_StringExpression|_RegularExpression)] :=
  FindRunFilesFromPattern[run, filename, FullFilenames -> True];

FindRunFile[runName_String, fileName_String] :=
  Module[{segments, files1, files2},
    segments = FindRunSegments[runName];
    files1 = Map[FileNameJoin[{#, fileName}] &, segments];
    files2 = Select[files1, FileType[#] =!= None &];

    If[files2 === {},
      Return[FindRunFilesFromPattern[runName, fileName, FullFilenames->True]]];

    Return[files2];
  ];

DefineMemoFunction[FindFirstRunFile[runName_String, fileName_String],
  ProfileCall["FindFirstRunFile",
  Module[{files},
    files = FindRunFile[runName, fileName];
    If[files === {}, Error["File " <> fileName <> " not found in run " <> runName]];
    files[[1]]]]];

Options[FindRunFilesFromPattern] = {"FullFilenames" -> False, "LeafNamesOnly" -> False};
DefineMemoFunction[FindRunFilesFromPattern[runName_String, filePattern:(_String|_StringExpression|_RegularExpression), opts:OptionsPattern[]],
  Module[{segments, nToDrop, names},
    segments = FindRunSegments[runName];
    If[segments === {}, Return[{}]];
    nToDrop = If[OptionValue[FullFilenames], 0, Length[FileNameSplit[segments[[1]]]]];
    names = Union[Map[FileNameDrop[#, nToDrop] &, Flatten[Map[FileNames[filePattern, #, Infinity, IgnoreCase -> False] &, segments], 1]]];
    If[OptionValue[LeafNamesOnly],
      names = Map[FileNameTake[#,-1]&, names]];
    names
]];

stringToReal[s_String] :=
  ProfileCall["stringToReal",
 Module[{p, n, mantissa, exponent},
  p = StringPosition[s, "e", 1];
  If[Length[p] == 0,
   ToExpression[s],
   n = p[[1, 1]];
   mantissa = ToExpression[StringTake[s, n - 1]];
   exponent = ToExpression[StringDrop[s, n]];
   1.0*mantissa*10^exponent]
  ]];

FileIsInRun[run_, file_] :=
  FindRunFiles[run, file] =!= {};

(**********************************************************)
(* SimulationNames                                        *)
(**********************************************************)

SyntaxInformation[SimulationNames] =
 {"ArgumentsPattern" -> {_., OptionsPattern[]}};

Options[SimulationNames] = {"FullPath" -> False};

SimulationNames[form_, OptionsPattern[]] :=
 Module[{dirs},
  If[StringQ[form] && StringTake[form,1] === "/",
    Return[FileNames[form]]]; (* TODO: detect absolute paths when form is a pattern *)
  If[SameQ[SimulationPath[], {}], Return[{}] ];
  dirs = Select[FileNames[form, SimulationPath[]], DirectoryQ];
  Switch[OptionValue["FullPath"],
    False,
    dirs = Map[FileNameTake[#, -1]&, dirs];,
    True,
    dirs = dirs,
    _,
    Error["Invalid value \""<>ToString[OptionValue["FullPath"]]<>"\" for FullPath option"]
  ];
  dirs
];

SimulationNames[OptionsPattern[]] := SimulationNames[Except["."] ~~ "*"]

End[];

EndPackage[];
