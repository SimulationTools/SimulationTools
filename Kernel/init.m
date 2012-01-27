(* ::Package:: *)

(* Mathematica Init File *)

(* Load NRMMA packages when << nrmma` is run *)

(* Work around a bug in ImportString in Mathematica 8. Without this,
   DataRegion's definition of GetData will cause problems. *)
If[$VersionNumber == 8. && $ReleaseNumber == 0, ImportString["", "Table"]];

(* Work around a bug in Mathematica 8 where Unprotect doesn't work with
   ListLogPlot unless we call ListLogPlot once first. *)
If[$VersionNumber == 8., ListLogPlot[{1}]];

(* We need to modify the $Path because the packages in nrmma don't
   specify their dependencies as nrmma`DataTable`, but as just
   DataTable`.  Changing this would break existing users.  When users
   are all using nrmma as a Mathematica application, we can change
   this. *)

$Path = Prepend[$Path, FileNameDrop[FindFile["nrmma`"], -2]];

Module[{packages =
 {"Ascii1D",
  "Ascii",
  "BHCoordinates",
  "CarpetHDF5",
  "CoordinateTransformations",
  "Convergence",
  "DataAnalysis",
  "DataRegion",
  "DataTable",
  "FieldLines",
  "FourierAnalysis",
  "Grids",
  "Horizons",
  "Kicks",
  "Memo",
  "MinTracker",
  "MultipoleASCII",
  "MultipoleHDF5",
  "NR",
  "NRExport",
  "nrmmaVersion",
  "Parameters",
  "Plotting",
  "Profile",
  "Providers",
  "PunctureTracker",
  "ReadHDF5",
  "RunFiles",
  "ShiftTracker",
  "SimFactory",
  "SimView",
  "SimViewRRMHD",
  "SystemStatistics",
  "Tracks",
  "TwoPunctures",
  "Waveforms",
  "YlmDecomp"}},

  packages = Map[#<>"`"&, packages];
  Unprotect[$Packages];
  $Packages = Complement[$Packages, packages];
  Protect[$Packages];
  Scan[Needs, packages];

  NRMMADoc[] :=
    Scan[Information[(# ~~ (Except["`"] ..)),
       LongForm -> False] &, packages];
]

(* Load tools which require Mathematica 8 or newer *)
If[$VersionNumber >= 8,
  Unprotect[$Packages];
  $Packages = Complement[$Packages, {"nrmma8`"}];
  Protect[$Packages];
  Needs["nrmma8`"];
];
