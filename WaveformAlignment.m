(* Copyright 2010-2017 Ian Hinder and Barry Wardell

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

BeginPackage["SimulationTools`WaveformAlignment`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  If[$VersionNumber >= 10, "GeneralUtilities`", Unevaluated[Sequence[]]]
 }];

TimePhaseAlignmentShifts;
PhaseAlignmentShift;
TimeAlignmentShift;
TimePhaseShifted;
AlignedWaveforms;

Begin["`Private`"];

TimePhaseAlignmentShifts[phis_, window_] :=
  Module[{oms, dt, dphi},
    If[Head[First[phis[[1]]]] === Complex, 
      Error["TimePhaseAlignmentShifts requires a real-valued phase"]];
    oms = NDerivative[1] /@ phis;
    dt = TimeAlignmentShift[oms, window];
    dphi = PhaseAlignmentShift[MapThread[Shifted, {phis, {0, dt}}], 
      window];
    {dt, dphi}];

PhaseAlignmentShift[phis : {phi1_, phi2_}, window : {tMin_, tMax_}] :=
  Module[{phisSlabbed, diff, tGuess, dphiGuess, dphiMinFind, dphi},
    phisSlabbed = 
    ResampleDataTables[Map[Slab[#, Span @@ window] &, phis]];
    diff[dphi_?NumberQ] :=
    GridNorm[phisSlabbed[[1]] - (phisSlabbed[[2]] + dphi)];
    tGuess = Mean[window];
    dphiGuess = 
    Interpolation[phisSlabbed[[1]], tGuess] - 
    Interpolation[phisSlabbed[[2]], tGuess];
    Quiet[dphiMinFind = 
      FindMinimum[diff[dphi], {dphi, dphiGuess, dphiGuess + Pi/4}], 
      FindMinimum::lstol];
    dphi /. dphiMinFind[[2]]];

TimeAlignmentShift[fs : {f1_, f2_}, window : {tMin_, tMax_}] :=
  Module[{dtMin, dtMax, diff, dtMinFind, dtp, dt, f1Slabbed},
    dtMin = tMax - MaxCoordinate[f2];
    dtMax = tMin - MinCoordinate[f2];
    (* Print[{dtMin,dtMax}]; *)
    f1Slabbed = Slab[f1, Span @@ window];
    diff[dt_?NumberQ] :=
    If[dt<dtMin || dt > dtMax(* && False*), 1000000.,
    GridNorm[(*Print[dt];*)
      Subtract @@ 
      ResampleDataTables[{f1Slabbed, 
        Slab[Shifted[f2, dt], Span @@ window]}]]];
    (* dtMinFind = FindMinimum[{diff[dtp],dtp >= dtMin && dtp <= dtMax}, {dtp, Mean[{dtMin, dtMax}]}]; *)
    dtMinFind = FindMinimum[diff[dtp], {dtp, (*0*) dtMin, dtMax}];
    dtp /. dtMinFind[[2]]];

TimePhaseShifted[d_DataTable, {dt_, dphi_}] :=
  Exp[I dphi] Shifted[d, dt];

Clear[AlignedWaveforms];

(* AlignedWaveforms[hs:{h1_,h2_}, window_] := *)
(*   Module[{phis, dtphis}, *)
(*     If[Length[hs] =!= 2,  *)
(*       Error["AlignedWaveforms only accepts two waveforms"]]; *)
(*     phis = Phase /@ hs; *)
(*     dtphis = {{0., 0.}, TimePhaseAlignmentShifts[phis, window]}; *)
(*     MapThread[TimePhaseShifted, {hs, dtphis}]]; *)

AlignedWaveforms[hs_List, window_] :=
  Module[{phis, shifts},
    If[Length[hs] < 2, 
      Error["AlignedWaveforms requires at least two waveforms"]];
    phis = Map[Phase[Slab[#,All;;LocateMaximum[Abs[#]]]] &, hs];
    (* Print["window = ", window]; *)
    (* Print["phis = ", phis]; *)
    shifts = TimePhaseAlignmentShifts[{First[phis], #}, window] & /@ Rest[phis];
    Join[{First[hs]}, MapThread[TimePhaseShifted, {Rest[hs], shifts}]]];

End[];
EndPackage[];
