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

BeginPackage["SimulationTools`BlackHole`",
 {
  "SimulationTools`CarpetGrids`",
  "SimulationTools`Horizons`",
  "SimulationTools`SimulationProperties`",
  "SimulationTools`Statistics`",
  "SimulationTools`Binary`",
  "SimulationTools`TwoPunctures`",
  "SimulationTools`DataTable`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`EccentricityReduction`",
  "SimulationTools`Parameters`",
  "SimulationTools`Utils`"
 }];

ReadBlackHoleMass::usage = "ReadBlackHoleMass[sim, i] gives the mass of the ith black hole in the simulation as a function of time as a DataTable.";
ReadBlackHoleIrreducibleMass::usage = "ReadBlackHoleIrreducibleMass[sim, i] gives the irreducible mass of the ith black hole in the simulation as a function of time as a DataTable.";
ReadBlackHoleSpin::usage = "ReadBlackHoleSpin[sim, i] gives a list of DataTables of the Cartesian coordinate components of the spin angular momentum of the ith black hole in the simulation as a function of time.";
ReadBlackHoleDimensionlessSpin;
EstimateBinaryMergerTime;
SimulationCompletionEstimate;
SimulationCompletionStatsTable;
BinaryBlackHolePlot2D;

Begin["`Private`"];

ReadBlackHoleMass[run_String, i_Integer /; i > 0] :=
  ChristodoulouMass[run, i, i-1];

ReadBlackHoleIrreducibleMass[run_String, i_Integer /; i > 0] :=
  ReadAHMass[run, i];

DocumentationBuilder`SymbolDescription["ReadBlackHoleSpin"] =
  "read spin angular momentum of a black hole";

ReadBlackHoleSpin[run_String, i_Integer /; i > 0] :=
  Table[ReadIsolatedHorizonSpin[run, i-1, dir], {dir, 1, 3}];

ReadBlackHoleDimensionlessSpin[sim_, i_] :=
 Module[{s, m},
  s = ReadBlackHoleSpin[sim, i];
  m = ReadBlackHoleMass[sim, i];
  Quiet[WithResampling[s/m^2],{InterpolatingFunction::dmval,Interpolation::inhr}]];

EstimateBinaryMergerTime[sim_String] :=
 Module[{omData, q, chi1, chi2, pnEv, tCurrent, sep, currentOm},
  sep = ReadBinarySeparation[sim];
  omData = NDerivative[1][ReadBinaryAzimuth[sim]];
  currentOm = Last[omData];
  If[Last[sep] < 0.1 || currentOm > 0.08,
   LocateMaximum[-(sep - 0.1)^2],
   (* else *)
   chi1 = Quiet[ReadBlackHoleDimensionlessSpin[sim, 1][[3]],InterpolatingFunction::dmval];
   chi2 = Quiet[ReadBlackHoleDimensionlessSpin[sim, 2][[3]],InterpolatingFunction::dmval];
   tCurrent = MaxCoordinate[omData];
   pnEv = 
    PostNewtonianEvolution[{1, 1/MassRatio[sim], Last[chi1], 
      Last[chi2], currentOm}];
   tCurrent + pnEv["TimeToMerger"]]];

SimulationCompletionEstimate[sim_String] := (*SimulationCompletionEstimate[sim] = *)
 Module[{tMerger, speed, currentSpeed, currentTime, timeAfterMerger, finalTimePar,
   endTime, remainingWalltimeHours, remainingWalltimeDays, 
   elapsedWalltimeHours, completionFraction},
  tMerger = EstimateBinaryMergerTime[sim];
  speed = ReadSimulationSpeed[sim];
  currentSpeed = Last[speed];
  currentTime = MaxCoordinate[speed];

  If[currentSpeed === 0., Print["WARNING: current speed is 0"]];

  (* TODO: work out what to do if this parameter is not found *)
  timeAfterMerger = ToExpression@ReadSimulationParameter[sim, "TrackTriggers::trigger_termination_after_delay", 600];

  finalTimePar = FinalCoordinateTime[sim];
  endTime = Min[tMerger + timeAfterMerger, finalTimePar];
  remainingWalltimeHours =
    If[currentTime > tMerger,
      (endTime - currentTime) / currentSpeed,
      (* else *)
      If[endTime < tMerger,
        (endTime - currentTime) / currentSpeed,
        (* else *)
        (tMerger - currentTime) / currentSpeed +
        (endTime - tMerger) / (1.3 currentSpeed)]];

  elapsedWalltimeHours = ReadWalltimeHours[sim];
  remainingWalltimeDays = remainingWalltimeHours/24;
  completionFraction = 
   elapsedWalltimeHours/(elapsedWalltimeHours + 
      remainingWalltimeHours);
  <|"Simulation" -> sim,
   "CurrentTime" -> currentTime,
   "TimeOfMerger" -> tMerger,
   "FinalTimeParameter" -> finalTimePar,
   "CurrentSpeed" -> currentSpeed,
   "CurrentTime" -> currentTime,
   "TimeAfterMerger" -> timeAfterMerger,
   "EndTime" -> endTime,
   "ElapsedWalltimeHours" -> elapsedWalltimeHours,
   "RemainingWalltimeHours" -> remainingWalltimeHours,
   "CompletionFraction" -> completionFraction,
   "LastOutputTime" -> LastOutputTime[sim],
   "CompletionDate" -> 
    DatePlus[LastOutputTime[sim], remainingWalltimeDays]|>
  ];

SimulationCompletionStatsTable[d_Dataset] := 
  <|"Table" ->
  Map[<|"Simulation" -> #Simulation,
    "% complete" -> Floor[100 #CompletionFraction],
    "Elapsed days" -> 
    NumberForm[
      N@Floor[10 (If[# < 0, 0, #] &)@#ElapsedWalltimeHours/24]/
      10, {Infinity, 1}], 
    "Remaining days" -> 
    NumberForm[
      N@Floor[10 (If[# < 0, 0, #] &)@#RemainingWalltimeHours/24]/
      10, {Infinity, 1}],
    "Finish time" -> DateString[#CompletionDate]|> &, d],
  "Filename" -> "completion",
  "Title" -> "Completion estimate"|>;

SimulationCompletionStatsTable[sims:{_String..}] :=
  SimulationCompletionStatsTable[Dataset[SimulationCompletionEstimate /@ sims]];

BinaryBlackHolePlot2D[sim_String, t_, opts___] :=
  Module[{gridPlot, hnPlots},
    gridPlot = GridPlot2D[sim, t, opts];
    hnPlots = Table[SimpleHorizonPlot2D[sim, i, t], {i, 1, 2}];
    Show[gridPlot, hnPlots]];

End[];
EndPackage[];
