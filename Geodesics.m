(* Copyright 2013 Ian Hinder

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

BeginPackage["SimulationTools`Geodesics`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataTable`",
  "SimulationTools`RunFiles`"
 }];

ReadGeodesicParameter;
ReadGeodesicCovariantTangent;
ReadGeodesicLapse;
ReadGeodesicShift;
ReadGeodesicContravariantTangent;
ReadGeodesicMetric;
ReadGeodesicKretschmann;
ReadGeodesicCoordinatesRHS;
ReadGeodesicFourMetric;
ReadGeodesicParameterRHS;

(* ReadGeodesicRedshift; *)

Begin["`Private`"];

readGeodesic[sim_, i_] :=
 ReadColumnFile[sim, 
  "geodesic::gd_position..asc", {"time", 
   Sequence @@ 
    Table["gd_pos_" <> d <> "[" <> ToString[i] <> 
      "]", {d, {"x", "y", "z"}}]}];

readGeodesicCoordinates[sim_, i_] :=
 Module[{g = readGeodesic[sim, i], t, x, y, z},
  {t, x, y, z} = Transpose[g];
  Map[ToDataTable[t, #] &, {x, y, z}]];

ReadGeodesicParameter[sim_, i_] :=
 ToDataTable[ReadColumnFile[sim, 
  "geodesic::gd_param..asc", {"time", "gd_tau[" <> ToString[i] <> "]"}]];

ReadGeodesicParameterRHS[sim_, i_] :=
 ToDataTable[ReadColumnFile[sim, 
  "geodesic::gd_param_rhs..asc", {"time", "gd_tau_rhs[" <> ToString[i] <> "]"}]];

SimulationTools`Geodesics`Trackers`ReadCoordinates[sim_, i_] :=
  readGeodesicCoordinates[sim, i];

ReadGeodesicCovariantTangent[sim_, i_] :=
  Module[
    {pTable, t, pt, px, py, pz},
    pTable = ReadColumnFile[
      sim, "geodesic::gd_momentum..asc",
      {"time", 
       Sequence @@ 
       Table["gd_mom_" <> d <> "[" <> ToString[i] <> 
             "]", {d, {"t", "x", "y", "z"}}]}];

    {t,pt,px,py,pz} = Transpose[pTable];
    Map[ToDataTable[t, #] &, {pt, px, py, pz}]];  

ReadGeodesicLapse[sim_, i_] :=
  Module[
    {alpTable, t, alp},
    alpTable = ReadColumnFile[
      sim, "geodesic::gd_metric..asc",
      {"time", 
       "gd_alpha[" <> ToString[i] <> "]"}];

    {t,alp} = Transpose[alpTable];
    Map[ToDataTable[t, #] &, {alp}][[1]]];

ReadGeodesicKretschmann[sim_, i_] :=
  Module[
    {fTable, t, alp},
    fTable = ReadColumnFile[
      sim, "geodesic::gd_invariants..asc",
      {"time", 
       "gd_CurvIr[" <> ToString[i] <> "]"}];

    {t,f} = Transpose[fTable];
    Map[16 ToDataTable[t, #] &, {f}][[1]]];

ReadGeodesicContravariantTangent[sim_, i_] :=
  Module[
    {alpTable, t, alp},
    alpTable = ReadColumnFile[
      sim, "geodesic::gd_momentum_con..asc",
      {"time", 
       "gd_mom_con_t[" <> ToString[i] <> "]"}];

    {t,alp} = Transpose[alpTable];
    Join[Map[ToDataTable[t, #] &, {alp}], {None, None, None}]];

ReadGeodesicShift[sim_, i_] :=
  Module[
    {betaTable, betax, betay, betaz},
    betaTable = ReadColumnFile[
      sim, "geodesic::gd_metric..asc",
      {"time", 
       Sequence @@ 
       Table["gd_beta" <> d <> "[" <> ToString[i] <> 
             "]", {d, {"x", "y", "z"}}]}];

    {t,betax,betay,betaz} = Transpose[betaTable];
    Map[ToDataTable[t, #] &, {betax, betay, betaz}]];

ReadGeodesicMetric[sim_, i_] :=
  Module[
    {table, gxx, gxy, gxz, gyy, gyz, gzz},
    table = ReadColumnFile[
      sim, "geodesic::gd_metric..asc",
      {"time", 
       Sequence @@ 
       Table["gd_g" <> d <> "[" <> ToString[i] <> 
             "]", {d, {"xx", "xy", "xz", "yy", "yz", "zz"}}]}];

    {t, gxx, gxy, gxz, gyy, gyz, gzz} = Transpose[table];
    {gxx, gxy, gxz, gyy, gyz, gzz} = Map[ToDataTable[t, #] &, {gxx, gxy, gxz, gyy, gyz, gzz}];

    {{gxx, gxy, gxz},
     {gxy, gyy, gyz},
     {gxz, gyz, gzz}}];


ReadGeodesicFourMetric[sim_String, g_Integer] :=
 Module[{\[Gamma], \[Alpha], \[Beta], \[Beta]d},
  \[Gamma] = ReadGeodesicMetric[sim, g];
  \[Alpha] = ReadGeodesicLapse[sim, g];
  \[Beta] = ReadGeodesicShift[sim, g];
  \[Beta]d = \[Gamma].\[Beta];
  {{-\[Alpha]^2 + \[Beta].\[Beta]d, Sequence @@ \[Beta]d},
   {\[Beta]d[[1]], Sequence @@ \[Gamma][[1]]},
   {\[Beta]d[[2]], Sequence @@ \[Gamma][[2]]},
   {\[Beta]d[[3]], Sequence @@ \[Gamma][[3]]}}];

ReadGeodesicCoordinatesRHS[sim_, i_] := 
 Table[ToDataTable@
   ReadColumnFile[sim, 
    "geodesic::gd_position_rhs..asc", {"time", 
     "gd_pos_rhs_" <> d <> "[" <> ToString[i] <> "]"}], {d, {"x", "y",
     "z"}}];

ReadGeodesicCovariantTangentRHS[sim_, i_] := 
 Table[ToDataTable@
   ReadColumnFile[sim, 
    "geodesic::gd_momentum_rhs..asc", {"time", 
     "gd_mom_rhs_" <> d <> "[" <> ToString[i] <> "]"}], {d, {"x", "y",
     "z"}}];


(* TODO: maybe this should be handled by the Trackers package? *)

quantities = {"Coordinates", "Velocity", "Radius", "Azimuth", "Inclination"};

readGeodesicFns = Table[Symbol["SimulationTools`Geodesics`ReadGeodesic"<>quantity], {quantity, quantities}];
readTrackerFns = Table[Symbol["SimulationTools`Trackers`ReadTracker"<>quantity], {quantity, quantities}];

MapThread[
  (#1[sim_, i_] := #2[sim, {"Geodesics", i}]) &, {readGeodesicFns, readTrackerFns}];

(* ReadGeodesicRedshift; *)

End[];
EndPackage[];
