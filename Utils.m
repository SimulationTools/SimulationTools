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

BeginPackage["Utils`"];

$NRMMACompatibilityVersion::usage = "$NRMMACompatibilityVersion sets the behaviour of functions to be compatible with that of a specific version of NRMMA.";

$NRMMAInformation::usage = "$NRMMAInformation is a list of rules that gives information about the version of NRMMA you are running.";
$NRMMAInstallationDirectory::usage = "$NRMMAInstallationDirectory gives the top-level directory in which NRMMA is installed.";

$NRMMAVersionNumber::usage = "$NRMMAVersionNumber is a real number which gives the current NRMMA version number.";
$NRMMAReleaseNumber::usage = "$NRMMAReleaseNumber is an integer which gives the current NRMMA release number.";
$NRMMAVersion::usage = "$NRMMAVersionNumber is a string that gives the version of NRMMA you are running.";

(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

$NRMMATestSimulationDirectory;
$NRMMATestSimulation;

(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

nrmmaVersion;

Begin["`Private`"];

$NRMMAInstallationDirectory = FileNameDrop[FindFile["nrmma`"], -2];
$NRMMATestSimulationDirectory = FileNameJoin[{FileNameDrop[FindFile["nrmma`"], -2], "Data","Simulations"}];
$NRMMATestSimulation = "bbh";

$NRMMAVersionNumber        = 1.0;
$NRMMAReleaseNumber        = 0;
$NRMMACompatibilityVersion = $NRMMAVersionNumber;

$NRMMAVersion :=
 Module[{path, version, release, buildid, gitrev},
  path = $NRMMAInstallationDirectory;
  version = ToString[NumberForm[$NRMMAVersionNumber, {Infinity, 1}]];
  release = ToString[$NRMMAReleaseNumber];

  buildid = Quiet@ReadList[FileNameJoin[{path, "BUILD_ID"}], "String"];
  If[SameQ[buildid, $Failed],
    buildid = "";
  ,
    buildid = " (" <> First[buildid] <> ")";
  ];

  gitrev = Quiet@ReadList[FileNameJoin[{path, "GIT_REVISION"}],"String"];
  If[SameQ[gitrev, $Failed],
    gitrev = First@ReadList["!git --git-dir "<>FileNameJoin[{path, ".git"}]<>" rev-parse HEAD", String];
  ,
    gitrev = First[gitrev];
  ];

  version <> "." <> release <> buildid <> " (git revision: " <> gitrev <>")"
]

$NRMMAInformation :=
  {"InstallationDirectory" -> $NRMMAInstallationDirectory,
   "Version" -> $NRMMAVersion,
   "VersionNumber" -> $NRMMAVersionNumber,
   "ReleaseNumber" -> $NRMMAReleaseNumber}


(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

nrmmaVersion[] = $NRMMAInformation;

End[];
EndPackage[];
