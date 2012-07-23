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

BeginPackage["Quasinormal`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`"
 }];

QuasinormalMode;
QuasinormalModeSum;
FitQuasinormalMode;
FitQuasinormalModes;

Begin["`Private`"];

(* TODO: this won't work because of the absolute path and missing QNM data *)
ReadMOm[l_, m_, n_] :=
 Import["~/Projects/nrmma/QNM-DATA/n" <> ToString[n + 1] <> "l" <> 
   ToString[l] <> 
   "m" <> (If[m >= 0, ToString[m], "m" <> ToString[-m]]) <> ".dat"]

ReadMOmFn[l_, m_, n_] := 
  Interpolation[Map[{#[[1]], #[[2]] - I #[[3]]} &, ReadMOm[l, m, n]], 
   InterpolationOrder -> 4];

If[loadedModes =!= True,
  Table[MOmFn[l,m,n] = ReadMOmFn[l,m,n],{l,2,4},{m,-l,l},{n,0,2}];
  loadedModes = True];

(* Maybe implement this by direct interpolating functions? *)

kerrToFreqs[j0_?NumberQ, M0_?NumberQ, l_?NumberQ, m_?NumberQ, 
  n_?NumberQ] :=
  Module[{MOm, MOm0, Om0,om0,tau0},
  MOm = MOmFn[l,m,n];
  MOm0 = MOm[Re[j0]];
  Om0 = MOm0/Re[M0];
  om0 = Re[Om0];
  tau0 = 1/Im[Om0];
  Return[{om -> om0, tau -> tau0}]]

omFromKerr[j0_?NumberQ, M0_?NumberQ, l_?NumberQ, m_?NumberQ, 
  n_?NumberQ] := om /. kerrToFreqs[j0, M0, l, m, n]

tauFromKerr[j0_?NumberQ, M0_?NumberQ, l_?NumberQ, m_?NumberQ, 
  n_?NumberQ] := tau /. kerrToFreqs[j0, M0, l, m, n]

QuasinormalMode[j_, M_, alpha_, phi_, l_, m_, n_, t_] :=
 Module[{tau, om},
  Exp[-t/Re[tau] + Re[alpha] - I (Re[om] t + Re[phi])] /.
   {tau -> 
     tauFromKerr[j, M, l, m, n],
    om -> omFromKerr[j, M, l, m, n]}]

QuasinormalModeSum[j_, M_, alpha_, phi_, modes_, t_] :=
  Module[{model, modelSum},
  model[mode:{l_,m_,n_}] :=
   QuasinormalMode[j, M, alpha[mode], phi[mode], l, m, n, t];
  modelSum = Apply[Plus, Map[model, modes]]];

FitQuasinormalMode[psi4_DataTable, {alpha_, phi_, M_, j_}] :=
 Module[{model0, fit0, t},
  model0 = 
   QuasinormalMode[j, M, alpha, phi, 2, 2, 0, t];
  fit0 = 
   FindFit[ToList[Re[psi4]], 
    model0, {{alpha, -5}, {phi, 0}, {M, 0.95}, {j, 0.7}},
    t, MaxIterations -> 1000];
  Return[fit0]]

FindFit2[data_DataTable, expr_, pars_, {x_}] :=
  FindFit2[data, {expr, {}}, pars, {x}];

FindFit2[data_DataTable, {expr_,constraints_List}, pars_, {x_}] :=
 Module[{ts, fs, i},
(*  ts = Map[Last,data];
  fs = Map[First,data]; *)
  ts = IndVar[data];
  fs = DepVar[data];

(*  residual[ps:{_Real..}] :=
   Module[{parRules, modelFn, modelData, diff, res},
    parRules = MapThread[Rule, {pars, ps}];
    modelFn = expr /. parRules;
    modelData = Map[modelFn /. x -> # &, ts];
    If[!Apply[And, Map[NumberQ, modelData]],
      Error[{"modelData not numerical: ", modelData, modelFn, parRules}]]; 
    diff = fs - modelData;
    res = Sqrt[diff.Conjugate[diff]];
    If[!NumberQ[res], Error["Not a number: ", res]];
    res];*)

  residual2Vec = Map[expr /. x -> # &, ts] - fs;
  residual2Expr = Re[ComplexExpand[residual2Vec.Conjugate[residual2Vec]]];
  Print[Hello];
  i = 0;
  xx=NMinimize[{residual2Expr,constraints}, pars, EvaluationMonitor :> (Global`path = Append[Global`path, pars])][[2]];
  Print["After"];
  Print[i];
  xx
  ]

 (* ( Print["Hello"]; ) *)

FitQuasinormalModes[psi4_DataTable, modes_, {alpha_, phi_, M_, j_}] :=
 Module[{nModes, data, model, modelSum, paramsOfMode, params, fit0, i},
  nModes = Length[modes];
  data = psi4;
  model[mode:{l_,m_,n_}] :=
   ComplexExpand[QuasinormalMode[j, M, alpha[mode], phi[mode], l, m, n, t]];
  modelSum = Apply[Plus, Map[model, modes]];
  i = -1;
  paramsOfMode[mode:{l_,m_,n_}] :=
    {{alpha[mode], i=i+1;-3-i}, {phi[mode], 0}};
  params = Flatten[Map[paramsOfMode, modes], 1] ~Join~ {{j, 0.6875}, {M, 0.95}};
(*  fit0 = FindFit2[ToList[data], {modelSum,{0.1 < j < 0.9}},
                 params, t (*, MaxIterations -> 1000, Method -> "NMinimize" *)]; *)
  fit0 = FindFit2[data, {modelSum, {0.5 < j < 0.9 (*, 0.1 < M < 1.4 *) }}, 
           Map[First,params], {t}];
  Return[fit0];

(*  res0 = Map[{#[[1]], #[[2]] - (model0 /. fit0 /. t -> #[[1]])} &, 
    data];
  resNorm0 = 
   Sqrt[Apply[Plus, Map[#[[2]]^2 &, res0]]/(tEnd - tStart)];
  modelTable0 = Map[{#[[1]], model0 /. fit0 /. t -> #[[1]]} &, data];
  makeModeTable[m_] :=
   
   Map[{#[[1]], m /. fit0 /. t -> #[[1]]} &, data];
  modeTbs0 = Map[makeModeTable, modeList];
  params2 = 
   Join[{M, j}, Flatten[Table[{alpha[i], phi[i]}, {i, 1, nModes}]]];
  params3 = Map[# -> (# /. fit0) &, params2];
  Return[{fit -> fit0, model -> model0, modelTb -> modelTable0, 
    resNorm -> resNorm0, res -> res0, dataTb -> data, 
    modeTbs -> modeTbs0, params -> params3}] *)
  ];

TableRange[t_List, tStart_?NumberQ, tEnd_?NumberQ] :=
  Select[t, 
   (#[[1]] >= tStart && #[[1]] <= tEnd) &];

TableShift[t_List, tShift_?NumberQ] :=
   Map[{#[[1]] + tShift, #[[2]]} &, t];

ReadPsi4File[name_String] :=
  Module[{},
    If[FileType[name] === None, Error["File " <> name <> " not found"]];
    Map[{#[[1]],#[[2]]+I#[[3]]} &, 
    ReadList[name, Real, RecordLists->True]]];

MassSpinFromPsi4[name_, tMaxEst_] :=
  Module[{psi4Tb, psi4AbsFn, tMax, t, fit},
    psi4Tb = ReadPsi4File[name];
    psi4AbsFn = Interpolation[Abs[psi4Tb]];
    tMax = t /. FindMaximum[psi4AbsFn[t], {t, tMaxEst}][[2]];
    fit = FitRingdownMode[psi4Tb, tMax, 30, 80];
    Print[name, j/.fit, M/.fit]];

End[];

EndPackage[];
