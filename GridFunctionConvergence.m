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
  "SimulationTools`DataRepresentations`"
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

readTime[sim_, it_] :=
 ReadTimeStep[sim]*it - First[ReadTimeRange[sim]];

GridFunctionConvergenceSet[runs_List, var_String, dims_, opts___] :=
 Module[{s, hs, itss, ks, rhos, commonIts, slider, checkIt, dt},
  hs = ReadCoarseGridSpacing /@ runs;
  ks = ReadCoarseTimeStep /@ runs;
  itss = ReadIterations[#, var, dims] & /@ runs;
  rhos = Rationalize[hs[[1]]/hs];
  commonIts = Intersection @@ (itss/rhos);
  dt = readTime[runs[[1]], itss[[1, 2]]]/itss[[1, 2]];
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
  
  s["first-iteration"] = First[commonIts];

  s["last-iteration"] = Last[commonIts];

  s["delta-iteration"] = 
   If[Length[commonIts] > 1, commonIts[[2]] - commonIts[[1]], 0];

  s["iterations"] = commonIts;

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

  s["rescaled-errors", it_, p_] :=
   Module[{d = s["resampled-data", it],
     cm = s["convergence-multiplier", p]},
    {d[[1]] - d[[2]], (d[[2]] - d[[3]]) cm}];
  
  s["difference", it_, {i_, j_}] :=
   Module[{d = s["resampled-data", it]},
    d[[1]] - d[[2]]];
  
  s["time", it_] :=
   (checkIt[it];
    it dt);
  
  s["grid-spacings"] = hs;

  s["convergence-multiplier", p_Integer] :=
   ConvergenceMultiplier[hs, p];

  s["convergence-rate", xs_List] :=
   ConvergenceRate[xs, hs];

  s["convergence-rate", it_Integer] :=
   ConvergenceRate[s["resampled-data", it], hs];

  s["convergence-rate-const", it_Integer] :=
   Module[{d = s["resampled-data", it]},
    convRateConst[{d[[1]] - d[[2]], d[[2]] - d[[3]]}, 
     s["grid-spacings"]]];
  
  Format[s] = "<convergence-set>";

  s];

End[];
EndPackage[];
