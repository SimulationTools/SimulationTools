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

BeginPackage["SimulationTools`",
 {
  "SimulationTools`Error`",
  "CCompilerDriver`"
 }
];

$SimulationToolsCompatibilityVersion::usage = "$SimulationToolsCompatibilityVersion sets the behaviour of functions to be compatible with that of a specific version of SimulationTools.";

$SimulationToolsInformation::usage = "$SimulationToolsInformation is a list of rules that gives information about the version of SimulationTools you are running.";
$SimulationToolsInstallationDirectory::usage = "$SimulationToolsInstallationDirectory gives the top-level directory in which SimulationTools is installed.";

$SimulationToolsVersionNumber::usage = "$SimulationToolsVersionNumber is a real number which gives the current SimulationTools version number.";
$SimulationToolsReleaseNumber::usage = "$SimulationToolsReleaseNumber is an integer which gives the current SimulationTools release number.";
$SimulationToolsVersion::usage = "$SimulationToolsVersionNumber is a string that gives the version of SimulationTools you are running.";

SimulationPath::usage = "SimulationPath[] gives the list of directories which SimulationTools will search for simulations.  It contains those directories listed in $SimulationPath as well as (for backward compatibility) the value of RunDirectory if it exists.";

$SimulationPath::usage = "$SimulationPath is the default list of directories to search in attempting to find a simulation.";
SimulationToolsCCompiler;

(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

$SimulationToolsTestSimulationDirectory;
$SimulationToolsTestSimulation;

(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

nrmmaVersion;

Begin["`Private`"];

packages = {
  (*"SimulationTools`Kernel`SimulationTools`",*)
    "SimulationTools`Ascii`",
    "SimulationTools`Ascii1D`",
  (*"SimulationTools`AsyncCommand`,"*)
    "SimulationTools`BHCoordinates`",
    "SimulationTools`Binary`",
    "SimulationTools`BlackHole`",
  (*"SimulationTools`CactusTimers`",*)
    "SimulationTools`CarpetGrids`",
    "SimulationTools`CarpetHDF5`",
  (*"SimulationTools`CarpetHDF5Plot`",*)
    "SimulationTools`CarpetIOHDF5`",
    "SimulationTools`CarpetIOScalar`",
  (*"SimulationTools`CarpetVTK`",*)
  (*"SimulationTools`CircularPN`",*)
    "SimulationTools`ColumnFile`",
    "SimulationTools`Convergence`",
    "SimulationTools`CoordinateTransformations`",
    "SimulationTools`DataAnalysis`",
    "SimulationTools`DataRegion`",
    "SimulationTools`DataRepresentations`",
    "SimulationTools`DataTable`",
  (*"SimulationTools`Empty`",*)
  (*"SimulationTools`Error`",*)
    "SimulationTools`FieldLines`", 
    "SimulationTools`EccentricityReduction`", 
    "SimulationTools`FileDependencies`",
    "SimulationTools`FileMemo`",
    "SimulationTools`Formaline`",
    "SimulationTools`Geodesics`",
    "SimulationTools`GridFunctions`",
    "SimulationTools`GridFunctionConvergence`",
    "SimulationTools`Grids`",
    "SimulationTools`Horizons`",
    "SimulationTools`IniFile`",
    "SimulationTools`InitialData`",
    "SimulationTools`Kicks`",
    "SimulationTools`Memo`",
    "SimulationTools`MessageCatcher`",
    "SimulationTools`MinTracker`",
    "SimulationTools`Movie`",
    "SimulationTools`MultipoleASCII`",
    "SimulationTools`MultipoleHDF5`",
    "SimulationTools`NR`",
    "SimulationTools`NRDF`",
    "SimulationTools`NRExport`",
    "SimulationTools`Parameters`",
    "SimulationTools`Performance`",
    "SimulationTools`Plotting`",
    "SimulationTools`Providers`",
    "SimulationTools`PunctureTracker`",
  (*"SimulationTools`Quasinormal`",*)
    "SimulationTools`ReadHDF5`",
    "SimulationTools`RunFiles`",
    "SimulationTools`Segments`",
    "SimulationTools`ShiftTracker`",
    "SimulationTools`SimFactory`",
    "SimulationTools`SimulationCompare`",
    "SimulationTools`SimulationProperties`",
    "SimulationTools`SimulationWeb`",
    "SimulationTools`SimView`",
    "SimulationTools`SimViewRRMHD`",
    "SimulationTools`SpEC`",
  (*"SimulationTools`Stack`",*)
    "SimulationTools`Statistics`",
    "SimulationTools`SystemStatistics`",
  (*"SimulationTools`Timers`",*)
    "SimulationTools`Trackers`",
    "SimulationTools`Tracks`",
    "SimulationTools`TwoPunctures`",
    "SimulationTools`Utils`",
    "SimulationTools`WaveformExtrapolation`",
    "SimulationTools`Waveforms`",
    If[$VersionNumber >= 8, "SimulationTools`Wavelets`", Sequence[]],
    "SimulationTools`YlmDecomp`"
   };

$SimulationToolsInstallationDirectory = FileNameDrop[FindFile["SimulationTools`"], -2];
$SimulationToolsTestSimulationDirectory = FileNameJoin[{FileNameDrop[FindFile["SimulationTools`"], -2], "Data","Simulations"}];
$SimulationToolsTestSimulation = FileNameJoin[{$SimulationToolsTestSimulationDirectory, "bbh"}];

$SimulationToolsVersionNumber        = 1.1;
$SimulationToolsReleaseNumber        = 0;
If[!ValueQ[$SimulationToolsCompatibilityVersion],
  $SimulationToolsCompatibilityVersion = $SimulationToolsVersionNumber;
];

$SimulationToolsVersion :=
 Module[{path, version, release, buildid, gitrev, gitdir},
  path = $SimulationToolsInstallationDirectory;
  version = ToString[NumberForm[$SimulationToolsVersionNumber, {Infinity, 1}]];
  release = ToString[$SimulationToolsReleaseNumber];

  buildid = Quiet@ReadList[FileNameJoin[{path, "BUILD_ID"}], "String"];
  If[SameQ[buildid, $Failed],
    buildid = "";
  ,
    buildid = " (" <> First[buildid] <> ")";
  ];

  (* First, check for a GIT_REVISION file. If it exists, use its contents as the revision. *)
  gitrev = Quiet@ReadList[FileNameJoin[{path, "GIT_REVISION"}],"String"];

  (* Otherwise, try to determine the git revision directly *)
  If[SameQ[gitrev, $Failed],
    gitdir = FileNameJoin[{path, ".git"}];
    If[FileType[gitdir] === Directory,
      gitrev = Quiet@ReadList["!git --git-dir "<>gitdir<>" rev-parse HEAD", String];
      If[gitrev === {}, gitrev = $Failed];
    ];
  ];

  (* If it worked, ReadList returns a list but we just want the first element (line) *)
  If[Head[gitrev] === List, gitrev = First[gitrev]];

  (* Check we have a git revision and otherwise give up trying *)
  If[Head[gitrev] === String && StringMatchQ[gitrev, RegularExpression["[0-9a-f]{5,40}"]], gitrev = " (" <> gitrev <> ")", gitrev = ""];

  version <> "." <> release <> buildid <> gitrev
]

$SimulationToolsInformation :=
  {"InstallationDirectory" -> $SimulationToolsInstallationDirectory,
   "Version" -> $SimulationToolsVersion,
   "VersionNumber" -> $SimulationToolsVersionNumber,
   "ReleaseNumber" -> $SimulationToolsReleaseNumber}

SimulationPath[] :=
  Join[If[ValueQ[$SimulationPath],
          If[MatchQ[$SimulationPath, {_String...}],
             $SimulationPath,
             Error["Invalid $SimulationPath; it should be a list of strings, but it is currently "<>
                   ToString[$SimulationPath,InputForm]]],
          {}],
       If[MemberQ[Names["Global`RunDirectory"], "RunDirectory"],
          If[MatchQ[ToExpression["Global`RunDirectory"], _String],
             {ToExpression["Global`RunDirectory"]},
             Error["Invalid RunDirectory; it should be a atring, but it is currently "<>
                   ToString[ToExpression["Global`RunDirectory"],InputForm]]],
          {}],
    If[Environment["SIMULATIONTOOLS_SIMULATIONPATH"] =!= $Failed,
      StringSplit[Environment["SIMULATIONTOOLS_SIMULATIONPATH"],":"],
      {}]];

(*********************************************************************
  SimulationToolsCCompiler
 *********************************************************************)

(* If the Intel compiler is available in the environment, it adds its
   include path to CPATH, and gcc cannot be used due to Intel's math.h
   being incompatible with gcc.  So if the Intel compiler is
   available, use it instead. *)

SimulationToolsCCompiler[] :=
  If[$CCompiler === Automatic && DefaultCCompiler[] === CCompilerDriver`GCCCompiler`GCCCompiler &&
    MemberQ["Compiler" /. CCompilers[], CCompilerDriver`IntelCompiler`IntelCompiler],
    (* then *)
    CCompilerDriver`IntelCompiler`IntelCompiler,
    (* else *)
    Automatic];

(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

nrmmaVersion[] = $SimulationToolsInformation;

End[];
EndPackage[];
