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

BeginPackage["SimulationTools`SimulationProperties`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Plotting`",
  "SimulationTools`Providers`",
  "SimulationTools`RunFiles`",
  "SimulationTools`"
 }];

(* Supported *)
ReadSimulationRunTime::usage = "ReadSimulationRuntime[sim] gives the real time in seconds elapsed during the execution of a simulation.";
ReadSimulationCoreCount::usage    = "ReadSimulationCoreCount[simname] gives the number of cores used by a simulation.";
ReadSimulationSpeed::usage = "ReadSimulationSpeed[sim] gives the execution speed of a simulation (simulation coordinate time per real time elapsed) as a DataTable as a function of simulation coordinate time.";
ReadSimulationCost::usage = "ReadSimulationCost[sim] gives the total number of core-hours used by all processes and segments in a simulation.";

CPUHours;
WallTimeDays;
CostAnalysis;
PresentationCostAnalysis;
ReadCarpetSpeed;

(* Deprecated *)

ReadWalltimeHours;
ReadWalltime = ReadSimulationRunTime;
ReadCores = ReadSimulationCoreCount;
ReadRunSpeed = ReadSimulationSpeed;
ReadCPUHours = ReadSimulationCost;

(* Experimental *)
StandardOutputOfRun;
SimulationSpeedPlot;

(* Exceptions *)
NoSimulationCoreCountAvailable;
NoSimulationRunTimeAvailable;

Begin["`Private`"];

ReadSimulationRunTime[runName_] :=
  Module[{segmentTime, files},
    segmentTime[file_] :=
      (ReadColumnFile[file, {"time", "time_total"}][[-1,2]]);
    files = FindRunFile[runName, "carpet::timing..asc"];
    Plus@@(segmentTime /@ files)];

ReadWalltimeHours[runName_] := 
  If[FindRunFile[runName, "carpet::timing..asc"] =!= {},
    ReadWalltime[runName]/3600,
    (* else *)
    Error[NoSimulationRunTimeAvailable,
          "Simulation run time not available in \""<>runName<>"\""]];

ReadSimulationCoreCount[run_] :=
  If[HaveData["RunFiles", FindRunDir[run]],
    CallProvidedFunction["RunFiles","ReadCores",{FindRunDir[run],run}],
    Error[NoSimulationCoreCountAvailable, "Simulation core count not available in \""<>run<>"\""]];

StandardOutputOfRun[runName_String] :=
  Module[{segments, files1, files2},
    segments = FindRunSegments[runName];
    files1 = Map[FileNameJoin[{#, "../"<>Last@FileNameSplit@runName<>".out"}] &, segments];
    files2 = Select[files1, FileType[#] =!= None &]];

ReadSimulationSpeed[runName_] := 
  If[FindRunFile[runName, "runstats.asc"] =!= {},
     MakeDataTable[ReadColumnFile[runName, "runstats.asc", {2, 4}]],
     ReadCarpetSpeed[runName]];

haveRunSpeed[sim_String] :=
  FindSimulationFiles[sim, "carpet::timing..asc"] =!= {} || FindSimulationFiles[sim, "runstats.asc"] =!= {};

ReadCarpetSpeed[runName_] :=
  MakeDataTable@ReadColumnFile[runName, "carpet::timing..asc", {"time","physical_time_per_hour"}];

ReadSimulationCost[runName_] := 
  ReadSimulationCoreCount[runName] * ReadWalltimeHours[runName];

CPUHoursPerDay[runName_] :=
  ReadSimulationCoreCount[runName] * 24;

PresentationCostAnalysis[prefix_String, T_, tMerger_:None, mergerFactor_:2] :=
  Module[{table},
    table = CostAnalysis[prefix,T,tMerger,mergerFactor];
    Grid@Prepend[Drop[table, 1], Style[#, Bold] & /@ table[[1]]]]

CostAnalysis[prefix_String, T_, tMergerp_:None, mergerFactor_:2] :=
 Module[{runs, costElems, header, tMerger, data},
  tMerger = If[tMergerp === None, T, tMergerp];
  runs = Last /@ 
    FileNameSplit /@ FileNames[prefix <> "_*", SimulationPath[]];
  costElems[run_] :=
   Module[{speed, cores, days, cpuHours},
    speed = Catch[Last@DepVar@ReadSimulationSpeed[run]];
    If[! NumberQ[speed], Return[None]];
    cores = ReadSimulationCoreCount[run];
    days = (tMerger/speed + (T-tMerger)/(mergerFactor*speed)) /24.0;
    cpuHours = (tMerger/speed + (T-tMerger)/(mergerFactor*speed)) * cores;
    {cores, speed, days, cpuHours}];
  data = Sort[
    Select[Map[costElems, runs], # =!= None &], #1[[1]] < #2[[1]] &];
  header = {"Cores", "Speed", "Days", "CPU Hours"};
  Prepend[data, header]];

SimulationSpeedPlot[runNames1_] :=
  Module[{runNames},
    runNames = Select[runNames1, haveRunSpeed];
    If[runNames === {},
       None,

       SimulationTools`Plotting`PresentationListLinePlot[Map[ReadSimulationSpeed, runNames],
                                PlotRange -> {0, All}, PlotLabel -> "Speed",
                                SimulationTools`Plotting`PlotLegend -> runNames, SimulationTools`Plotting`LegendPosition -> {Left, Bottom}]]];

End[];
EndPackage[];
