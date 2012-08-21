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

BeginPackage["SystemStatistics`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "IniFile`",
  "Memo`",
  "Plotting`",
  "RunFiles`"
 }];

ReadSimulationSpeed::usage = "ReadSimulationSpeed[sim] gives the execution speed of a simulation (simulation coordinate time per real time elapsed) as a DataTable as a function of simulation coordinate time.";
ReadSimulationMemoryUsage::usage   = "ReadSimulationMemoryUsage[sim] gives the memory usage of a simulation in MB as a DataTable as a function of coordinate time.";
ReadSimulationSwapUsage::usage   = "ReadSimulationSwapUsage[sim] gives the swap memory usage of a simulation in MB as a DataTable as a function of coordinate time.";
ReadSimulationCost::usage = "ReadSimulationCost[sim] gives the total number of CPU hours used by all processes in a simulation .";
CPUHours;
WallTimeDays;
CostAnalysis;
PresentationCostAnalysis;
ReadCarpetSpeed;

(* Old names *)
ReadRunSpeed = ReadSimulationSpeed;
ReadMemory = ReadSimulationMemoryUsage;
ReadSwap = ReadSimulationSwapUsage;
ReadCPUHours = ReadSimulationCost;

Begin["`Private`"];

ReadRunSpeed[runName_] := 
  If[FindRunFile[runName, "runstats.asc"] =!= {},
     MakeDataTable[ReadColumnFile[runName, "runstats.asc", {2, 4}]],
     ReadCarpetSpeed[runName]];

SystemStatistics`SimulationOverview`Plots[runNames1_] :=
  {Replace[DeleteCases[{simSpeedPlot[runNames1], simMemPlot[runNames1]},None],{}->None]};

simSpeedPlot[runNames1_] :=
  Module[{runNames},
    runNames = Select[runNames1, haveRunSpeed];
    If[runNames === {},
       None,

       Plotting`PresentationListLinePlot[Map[ReadSimulationSpeed, runNames],
                                PlotRange -> {0, All}, PlotLabel -> "Speed\n",
                                Plotting`PlotLegend -> runNames, Plotting`LegendPosition -> {Left, Bottom}]]];

haveRunSpeed[sim_String] :=
  FindSimulationFiles[sim, "carpet::timing..asc"] =!= {} || FindSimulationFiles[sim, "runstats.asc"] =!= {};

ReadCarpetSpeed[runName_] :=
  MakeDataTable@ReadColumnFile[runName, "carpet::timing..asc", {"time","physical_time_per_hour"}];

ReadCPUHours[runName_] := 
  ReadWalltimeHours[runName] * ReadCores[runName];

ReadMemory[runName_] :=
  If[FindRunFile[runName, "systemstatistics::process_memory_mb.maximum.asc"] =!= {},
     MakeDataTable[ReadColumnFile[runName, "systemstatistics::process_memory_mb.maximum.asc", {"time", "maxrss_mb"}]],
     MakeDataTable[ReadColumnFile[runName, "MemStats0000.asc", {1, 2}]]];

simMemPlot[runNames1_] :=
  Module[
    {runNames},
    runNames = Select[runNames1, haveMem];
    If[runNames === {},
       None,
       
       Module[
         {swaps, mems},
         swaps = Catch[Catch[Map[ReadSwap, runNames],RunFiles`Private`UnknownColumns],_];
         If[StringQ[swaps], swaps = {{0,0}}];
         mems = Catch[Catch[Map[ReadMemory, runNames],RunFiles`Private`UnknownColumns],_];
         If[StringQ[mems], mems = {{0,0}}];
         
         Show[PresentationListLinePlot[mems, PlotLegend -> runNames, LegendPosition -> {Left, Bottom}],
              PresentationListLinePlot[swaps, PlotStyle->{Dashed}],
              PlotRange -> {0, All}, AxesOrigin->{0,0}, PlotLabel -> "Memory\n"]]]];

haveMem[sim_String] :=
  FindSimulationFiles[sim, "systemstatistics::process_memory_mb.maximum.asc"] =!= None;

ReadSwap[runName_] :=
  MakeDataTable[ReadColumnFile[
    runName, "systemstatistics::process_memory_mb.maximum.asc",
    {"time", "swap_used_mb"}]];

CPUHoursPerDay[runName_] :=
  ReadCores[runName] * 24;

PresentationCostAnalysis[prefix_String, T_, tMerger_:None, mergerFactor_:2] :=
  Module[{table},
    table = CostAnalysis[prefix,T,tMerger,mergerFactor];
    Grid@Prepend[Drop[table, 1], Style[#, Bold] & /@ table[[1]]]]

CostAnalysis[prefix_String, T_, tMergerp_:None, mergerFactor_:2] :=
 Module[{runs, costElems, header, tMerger, data},
  tMerger = If[tMergerp === None, T, tMergerp];
  runs = Last /@ 
    FileNameSplit /@ FileNames[prefix <> "_*", $SimulationPath];
  costElems[run_] :=
   Module[{speed, cores, days, cpuHours},
    speed = Catch[Last@DepVar@ReadRunSpeed[run]];
    If[! NumberQ[speed], Return[None]];
    cores = ReadCores[run];
    days = (tMerger/speed + (T-tMerger)/(mergerFactor*speed)) /24.0;
    cpuHours = (tMerger/speed + (T-tMerger)/(mergerFactor*speed)) * cores;
    {cores, speed, days, cpuHours}];
  data = Sort[
    Select[Map[costElems, runs], # =!= None &], #1[[1]] < #2[[1]] &];
  header = {"Cores", "Speed", "Days", "CPU Hours"};
  Prepend[data, header]];

End[];

EndPackage[];
