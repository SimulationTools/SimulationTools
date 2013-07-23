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

BeginPackage["SimulationTools`GridFunctionConvergence`",
 {"SimulationTools`Convergence`",
  "SimulationTools`GridFunctions`",
  "SimulationTools`Grids`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`"
 }];


GridFunctionConvergenceSet::usage = "GridFunctionConvergenceSet[runs, var, dims] returns a convergence set object for the given data.";

Begin["`Private`"];

convRateConst[{d12_, d23_}, {h1_, h2_, h3_}] := 
 (* TODO: give an error or make it work if h1/h2 != h2/h3 *)
 Module[{rho = h2/h1}, 
     Quiet[
   Log[d23/d12]/Log[rho] /. {_Complex -> None, 
     Indeterminate -> None, Infinity -> None}, {Power::infy, 
    Infinity::indet}]];

readTime[sim_, it_, rl_] :=
 ReadTimeStep[sim, RefinementLevel -> rl]*it - First[ReadTimeRange[sim]];

(* Options[GridFunctionConvergenceSet] = {"Interpolation" -> True}; *)

$bndPat = {{_Integer,_Integer}..};

Options[GridFunctionConvergenceSet] = {Iteration -> 0, RefinementLevel -> 0};
GridFunctionConvergenceSet[runs_List, var_String, dims_, bnd:$bndPat, opts:OptionsPattern[]] :=
 Module[{s, hs, itss, ks, rhos, commonIts, slider, checkIt, dtdis, dtdi},

  hs = ReadGridSpacings[#, RefinementLevel -> OptionValue[RefinementLevel]][[1]] & /@ runs;

  (* k is the time step on this refinement level -- UNUSED *)
  ks = ReadTimeStep[#, RefinementLevel -> OptionValue[RefinementLevel]] & /@ runs;
  itss = ReadIterations[#, var, dims, RefinementLevel -> OptionValue[RefinementLevel]] & /@ runs;
  rhos = Rationalize[hs[[1]]/hs];
  commonIts = Intersection @@ (itss/rhos);
  dtdis = Map[ReadTimeStep[#, RefinementLevel -> ReadMaxRefinementLevels[#] - 1] &, runs];
  dtdi = dtdis[[1]];

  slider[Dynamic[i_]] :=
   Row[
    {"Iteration: ",
     Slider[Dynamic[i],
      {s["first-iteration"], s["last-iteration"], 
       s["delta-iteration"]}],
     " ", Dynamic[i], "/", s["last-iteration"]}];
  checkIt[it_] :=
   
   If[! MemberQ[s["iterations"], it], 
    Error["Iteration " <> ToString[it] <> 
      " is not present in this convergence set"]];
  
  s["rhos"] = rhos;

  s["first-iteration"] = First[commonIts];

  s["last-iteration"] = Last[commonIts];

  s["last-times"] = Last/@itss * dtdis;

  s["delta-iteration"] = 
   If[Length[commonIts] > 1, commonIts[[2]] - commonIts[[1]], 0];

  s["iterations"] = commonIts;

  s["iterator"] = Sequence[s["first-iteration"], s["last-iteration"], s["delta-iteration"]];

  s["slider"] = slider[#] &;

  s["data", it_] :=
   (checkIt[it];
    MapThread[
     ReadGridFunction[#1, var, dims, Iteration -> #2, opts] &, {runs, 
      rhos it}]);
  
  s["data", it_, i_] :=
   (checkIt[it];
    ReadGridFunction[runs[[i]], var, dims, Iteration -> rhos[[i]] it, 
     opts]);
  
  s["resampled-data", it_] :=
   Module[{d = s["data", it]},
    Resampled[d, Last[d]]];

  s["common-data", it_] :=
   MapThread[
     Downsample[Take[#1, Sequence @@ Transpose[Transpose[bnd + 1] {1, -1}]], #2] &, {s["data", it], 
                                                   s["rhos"]}];

  s["rescaled-errors", it_, p_, int_] :=
   Module[{d = If[int, s["resampled-data", it], s["common-data",it]],
     cm = s["convergence-multiplier", p]},
    {d[[1]] - d[[2]], (d[[2]] - d[[3]]) cm}];

  s["richardson-extrapolant", it_, p_, int_] :=
   Module[
     {d = If[int, s["resampled-data", it], s["common-data",it]],
      rho = hs[[2]]/hs[[3]]},
     (rho^p d[[3]] - d[[2]])/(rho^p-1)];

  s["richardson-error", it_, p_, int_] :=
   Module[
     {d = If[int, s["resampled-data", it], s["common-data",it]],
      rho = hs[[2]]/hs[[3]]},
     d[[3]] - (rho^p d[[3]] - d[[2]])/(rho^p-1)];

  s["richardson-error-norm", it_, p_, int_, coordRange_:All] :=
   Module[
     {nDims, slab, d = s["richardson-error", it, p, int]},
     nDims = Length[Dimensions[d]];
     slab = If[coordRange === All, Identity, Slab[#, Sequence@@coordRange] &];
     GridNorm[slab[d]]];

  s["richardson-error-norm-of-t", p_, int_, coordRange_:All, timeRange_:All] :=
    ToDataTable[s["function-of-t"][s["richardson-error-norm", #, p, int, coordRange] &, timeRange]];
  
  s["difference", it_, {i_, j_}] :=
   Module[{d = s["resampled-data", it]},
    d[[1]] - d[[2]]];
  
  s["time", it_] :=
   (checkIt[it];
    it dtdi);
  
  s["grid-spacings"] = hs;

  (* s["common-coordinates", it_:0] := *)
  (*   ToListOfCoordinates/@s["data", it]; *)

  s["convergence-multiplier", p_Integer] :=
   ConvergenceMultiplier[hs, p];

  s["convergence-rate", xs_List] :=
   ConvergenceRate[xs, hs];

  s["convergence-rate", it_Integer, int_] :=
   Module[{d = If[int, s["resampled-data", it], s["common-data",it]]},
   ConvergenceRate[d, hs]];

  s["convergence-rate-const", it_Integer, int_] :=
   Module[{d = If[int, s["resampled-data", it], s["common-data",it]]},
   convRateConst[{d[[1]] - d[[2]], d[[2]] - d[[3]]}, 
     s["grid-spacings"]]];

  s["convergence-norm-const", it_Integer, coordRange_:All] :=
   Module[
     {d = s["common-data", it], slab, nDims},
     nDims = Length[Dimensions[d[[1]]]];
     slab = If[coordRange === All, Identity, Slab[#, Sequence@@coordRange] &];
     convRateConst[{GridNorm[slab[d[[1]] - d[[2]]]], GridNorm[slab[d[[2]] - d[[3]]]]}, 
                   s["grid-spacings"]]];
        
  s["convergence-norm-const-of-t", coordRange_:All, timeRange_:All] :=
    ToDataTable[s["function-of-t"][s["convergence-norm-const", #, coordRange] &, timeRange]];
  
  s["function-of-t"][f_, timeRange_:All] :=
    Module[
      {allIts, allTimes, its},
      allIts = s["iterations"];
      allTimes = Map[s["time", #] &, allIts];
      its = If[timeRange === All, allIts,
               Pick[allIts, Map[timeRange[[1]] <= # <= timeRange[[2]] &, allTimes]]];
      Monitor[Table[{N@s["time", it], f[it]},
                    {it, its}],ProgressIndicator[(it-First[its])/(Last[its] - First[its])]]];

  Format[s] = "<convergence-set>";

  s];




End[];
EndPackage[];
