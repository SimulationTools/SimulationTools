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
  "SimulationTools`Error`"
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
    "SimulationTools`Profile`",
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
    "SimulationTools`SimView`",
    "SimulationTools`SimViewRRMHD`",
  (*"SimulationTools`Stack`",*)
    "SimulationTools`Statistics`",
    "SimulationTools`SystemStatistics`",
  (*"SimulationTools`Timers`",*)
    "SimulationTools`Trackers`",
    "SimulationTools`Tracks`",
    "SimulationTools`TwoPunctures`",
    "SimulationTools`Utils`",
    "SimulationTools`Waveforms`",
    If[$VersionNumber >= 8, "SimulationTools`Wavelets`", Sequence[]],
    "SimulationTools`YlmDecomp`"
   };

$SimulationToolsInstallationDirectory = FileNameDrop[FindFile["SimulationTools`"], -2];
$SimulationToolsTestSimulationDirectory = FileNameJoin[{FileNameDrop[FindFile["SimulationTools`"], -2], "Data","Simulations"}];
$SimulationToolsTestSimulation = FileNameJoin[{$SimulationToolsTestSimulationDirectory, "bbh"}];

$SimulationToolsVersionNumber        = 1.0;
$SimulationToolsReleaseNumber        = 0;
If[!ValueQ[$SimulationToolsCompatibilityVersion],
  $SimulationToolsCompatibilityVersion = $SimulationToolsVersionNumber;
];

$SimulationToolsVersion :=
 Module[{path, version, release, buildid, gitrev},
  path = $SimulationToolsInstallationDirectory;
  version = ToString[NumberForm[$SimulationToolsVersionNumber, {Infinity, 1}]];
  release = ToString[$SimulationToolsReleaseNumber];

  buildid = Quiet@ReadList[FileNameJoin[{path, "BUILD_ID"}], "String"];
  If[SameQ[buildid, $Failed],
    buildid = "";
  ,
    buildid = " (" <> First[buildid] <> ")";
  ];

  gitrev = Quiet@ReadList[FileNameJoin[{path, "GIT_REVISION"}],"String"];
  If[SameQ[gitrev, $Failed],
    gitrev = Quiet@First@ReadList["!git --git-dir "<>FileNameJoin[{path, ".git"}]<>" rev-parse HEAD", String];
  ,
    gitrev = First[gitrev];
  ];

  If[!StringQ[gitrev], gitrev = "", gitrev = " (" <> gitrev <> ")"];

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
       If[MemberQ[Names["Global`*"], "RunDirectory"],
          If[MatchQ[ToExpression["Global`RunDirectory"], _String],
             {ToExpression["Global`RunDirectory"]},
             Error["Invalid RunDirectory; it should be a atring, but it is currently "<>
                   ToString[ToExpression["Global`RunDirectory"],InputForm]]],
          {}]];

(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

nrmmaVersion[] = $SimulationToolsInformation;

End[];
EndPackage[];
