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
  ErrorDefinition, DefFn, DefFnQ, withCustomSetDelayed, extraPackages},

  extraPackages = {"Stack`", "Error`"};

  packages = Map[#<>"`"&, packages];
  Unprotect[$Packages];
  $Packages = Complement[$Packages, Join[packages, extraPackages]];
  Protect[$Packages];

  Scan[Needs, extraPackages];

  (* Report an error when a function is called with unrecognised arguments *)
  ErrorDefinition[x_] :=
    x[args___] :=
     If[NRMMADebug===True, Error`CatchError, Identity][Error`Error["Invalid arguments in `1`", ToString[x] <> "[" <>
       StringJoin[Riffle[ToString[#] & /@ {args}, ", "]] <>
       "]"]];

SetAttributes[CheckAssignments, HoldAll];
CheckAssignments[fn_, validSymbolsp_, (Module|DynamicModule|With)[defs_, body_]] :=
 Module[{validSymbols, a, b, assignedSymbols, warnSymbols, noValueQ, 
   moduleSymbols, expr, assignedExpresions, noValueAssignedSymbols},
  moduleSymbols = 
   ReleaseHold@
    Replace[Hold[defs], {HoldPattern[a_ = b_] :> Hold[a], 
      a_ :> Hold[a]}, {2}];
  (* Print["moduleSymbols = ", moduleSymbols]; *)
  (* Print["validSymbolsp = ", validSymbolsp // HoldForm]; *)
  validSymbols = Join[validSymbolsp, moduleSymbols];
  (* Print["validSymbols = ", validSymbols]; *)
  (* Check and remove all Modules in body *)
  expr = Hold[body];
  (* Print["expr = ", expr]; *)
  
  (*expr=Replace[expr,m_Module:>(checkModule[validSymbols,m];Null),{1,
  Infinity}];
  *)
  
  (* See http://library.wolfram.com/conferences/devconf99/villegas/
  UnevaluatedExpressions/Links/index_lnk_ 30.html *)
  
  expr = expr /. 
    (m_Module|m_DynamicModule|m_With) :> 
     With[{eval = (CheckAssignments[fn, Evaluate[validSymbols], m]; Null)}, 
      eval /; True];
  
  (* Don't look inside held expressions *)
  expr=Replace[expr, Hold[_]:>Null,{1,Infinity}];
  (* Print["expr = ", expr]; *)
  
  noValueQ[x_] := ! ValueQ[x];
  assignedSymbols = 
   Cases[expr, HoldPattern[a_Symbol = b_] :> Hold[a], Infinity];

  assignedSymbols = Join[assignedSymbols,
   Cases[expr, HoldPattern[a_List = b_] :> Hold[a], Infinity]];

  assignedSymbols = Flatten[Cases[assignedSymbols, HoldPattern[a_Symbol]:>Hold[a], Infinity]];

  (* If[MemberQ[assignedSymbols, HoldPattern[Hold[ExportStatus]]], *)
  (*    Print["yes"]]; *)

  (* PutAppend[fn,"/Users/ian/test.m"]; *)
  (* PutAppend[assignedSymbols,"/Users/ian/test.m"]; *)

  noValueAssignedSymbols = Cases[assignedSymbols, Hold[x_] /; !ValueQ[x]];
  (* noValueAssignedSymbols = assignedSymbols; *)
  (* Print["assignedSymbols = ", assignedSymbols]; *)
  warnSymbols = Complement[noValueAssignedSymbols, validSymbols];
  If[Length[warnSymbols] > 0,
     (* Using a Message here leads to weird Java errors *)
     Print[StringForm["Warning: `1`: assignment to undeclared symbols(s): `2`", ToString[fn],
            StringJoin[Riffle[ToString /@ (warnSymbols /. Hold -> HoldForm), ", "]]]]];
  Null];

  SetAttributes[DefFn, HoldAll];
  DefFn[fn_[args___], body_] :=
   Module[{x,argSyms},
    ErrorDefinition[fn];
    argSyms = Cases[{args}, x_Pattern :> x[[1]], Infinity, Heads->True]; (* What if the pattern name symbol has meaning in this scope? *)
    CheckAssignments[Evaluate[ToString[fn]],Map[Hold,argSyms],Module[{},body]];

         (* [ *)
    If[NRMMADebug === True,
       fn[args] := 
         If[Length[Stack`CurrentStack[]] === 0, Error`CatchError, Identity][Stack`WithStackFrame[Hold[fn], body]],
       (*else*)
       fn[args] := body];
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

  (* This does not seem to work *)

  (* SetAttributes[withCustomThrow, HoldAll]; *)
  (* withCustomThrow[code_] := *)
  (*  Internal`InheritedBlock[{Throw}, *)
  (*    Unprotect[Throw]; *)
  (*    Throw[s_String] := Error`Error[s]; *)
  (*    Protect[Throw]; *)

  (*    code *)
  (*  ]; *)

  If[NRMMADebug === True,
    (* We do this temporarily until all occurrences of Throw[_String]
      have been replaced with calls to Error *)
    Unprotect[Throw];
    Throw[s_String] := Error`Error[StringReplace[s,"`"->"`.`"]];
    Protect[Throw]];

  If[NRMMADebug === True, withCustomSetDelayed, Identity][Scan[Needs, packages]];

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
