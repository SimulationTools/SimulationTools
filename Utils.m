(* Mathematica package *)

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

$NRMMATestRunDirectory;
$NRMMATestRun;

(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

nrmmaVersion;

Begin["`Private`"];

$NRMMAInstallationDirectory = FileNameDrop[FindFile["nrmma`"], -2];
$NRMMATestRunDirectory = FileNameJoin[{FileNameDrop[FindFile["nrmma`"], -2], "Data","Simulations"}];
$NRMMATestRun = "bbh";

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
