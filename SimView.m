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

BeginPackage["SimView`",
 {
  "BHCoordinates`",
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "Horizons`",
  "Memo`",
  "NR`",
  "Parameters`",
  "Plotting`",
  "Providers`",
  "RunFiles`",
  "SimViewRRMHD`",
  "SystemStatistics`",
  "Waveforms`"
 }];

SimulationOverview::usage = "SimulationOverview[sim] gives a quick overview of the simulation sim.
SimView[{sim1, sim2, ...}] gives a quick overview of the simulations sim1, sim2, ....";

(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

SimView;
FinishTime(*::usage = "FinishTime[sim] returns a date list corresponding to the estimated time at which the simulation will finish if it runs continuously."*);

Begin["`Private`"];

(**********************************************************)
(* SimulationOverview                                     *)
(**********************************************************)

Options[SimulationOverview] = {
  "SimulationType" -> Automatic
};

SyntaxInformation[SimulationOverview] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};

SimulationOverview[sim_String, opts___] :=
  SimulationOverview[{sim}, opts];

SimulationOverview[sims_List, OptionsPattern[]] :=
 Module[{type, res},
  type = OptionValue[SimulationType];

  (* TODO: Add detection of the simulation type *)
  type = type /. Automatic -> "BBH";

  Which[
   type === "BBH",
    res = SimView[sims];,
   MatchQ[type, {"BBH", "ExtractionRadius" -> _}],
   	res = SimView[sims, type[[2,2]]];,
   type === "RRMHD",
    res = SimViewRRMHD[sims];,
   True,
    Error["Unrecognised simulation type " <> ToString[type]];
  ];

  res
];


(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

SimView[runName_String, opts___] := SimView[{runName}, opts];

SimView[runNames_List] :=
  Module[{r, rads},
    If[And@@(HaveData["Waveforms",#] & /@ runNames),         
      rads = ReadPsi4Radii[First[runNames]],
      rads = {}];
      r = If[rads === {}, 0, First[rads]];
      SimView[runNames, r]];

SimView[runNames_List, r_] :=
  Module[
    {grid, plots, providers},

  (* spinNorms = Catch[ *)
  (*  PresentationListLinePlot[ *)
  (*    Flatten@Table[ *)
  (*      Norm@ReadIsolatedHorizonDimensionlessSpin[run, hn],{run,runNames},{hn,0,1}], *)
  (*   PlotRange -> {0,Automatic}, PlotLabel -> "S_i/m^2\n", ImageSize -> size],_]; *)

  (* spinPhases = Catch[ *)
  (*  PresentationListLinePlot[ *)
  (*    Flatten@Table[ *)
  (*      ReadIsolatedHorizonSpinPhase[run, hn]/Degree,{run,runNames},{hn,0,1}], *)
  (*   PlotRange -> Automatic, PlotLabel -> "arg[S_i]/deg\n", ImageSize -> size],_]; *)

    (* TODO: Add the RRMHD data as new SimulationOverview providers *)
    providers = {"SystemStatistics", "Binary", "Waveforms", "Statistics"};

    (* This does not give the ordering that we like *)
    (* providers = Map[First@StringSplit[#,"`"] &, Names["*`SimulationOverview`Plots"]]; *)

    plots = Join@@DeleteCases[
      Table[Symbol[p<>"`SimulationOverview`Plots"][runNames], {p, providers}], None|{None}];

    grid = Grid[{{Text[Style[StringJoin[Riffle[runNames,", "]], Bold, 24]],
                  SpanFromLeft}} ~Join~ plots,
                Spacings -> {0, 1}];

    Return[grid]];

End[];

EndPackage[];
