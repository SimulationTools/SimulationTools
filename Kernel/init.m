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
$Path = Prepend[$Path, FileNameDrop[FindFile["nrmma`"], -2]<>"/PirahaPeg"];

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
  "Grids",
  "Horizons",
  "InitialData",
  "Kicks",
  "Memo",
  "MinTracker",
  "MultipoleASCII",
  "MultipoleHDF5",
  "NR",
  "NRDF",
  "NRExport",
  "nrmmaVersion",
  "Parameters",
  "Piraha",
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
  "YlmDecomp"},
  ErrorDefinition, DefFn, DefFnQ, withCustomSetDelayed},

  packages = Map[#<>"`"&, packages];
  Unprotect[$Packages];
  $Packages = Complement[$Packages, packages];
  Protect[$Packages];

  (* Report an error when a function is called with unrecognised arguments *)
  ErrorDefinition[x_] :=
    x[args___] :=
     Throw["Invalid arguments in " <> ToString[x] <> "[" <>
       StringJoin[Riffle[ToString[#, InputForm] & /@ {args}, ", "]] <>
       "]", InvalidArguments];

  SetAttributes[DefFn, HoldAll];
  DefFn[fn_[args___], body_] :=
   Module[{},
    ErrorDefinition[fn];
    fn[args] := body;
   ];

  DefFnQ[_] = False;

  SetAttributes[withCustomSetDelayed, HoldAll];
  withCustomSetDelayed[code_] :=
   Internal`InheritedBlock[{SetDelayed},
     Unprotect[SetDelayed];
     SetDelayed[fn_[args___], rhs_] /; ((!DefFnQ[fn]) && MemberQ[packages, Context[fn]]) :=
       (DefFnQ[fn] = True; DefFn[fn[args], rhs];);
     Protect[SetDelayed];
     code
   ];
  withCustomSetDelayed[Scan[Needs, packages]];

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
