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

BeginPackage["SimulationTools`SystemStatistics`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`IniFile`",
  "SimulationTools`Memo`",
  "SimulationTools`Plotting`",
  "SimulationTools`RunFiles`",
  "SimulationTools`Statistics`",
  "SimulationTools`SimulationProperties`",
  "SimulationTools`"
 }];

(* Supported *)
ReadSimulationMemoryUsage::usage   = "ReadSimulationMemoryUsage[sim] gives the memory usage of a simulation in MB as a DataTable as a function of coordinate time.";
ReadSimulationSwapUsage::usage   = "ReadSimulationSwapUsage[sim] gives the swap memory usage of a simulation in MB as a DataTable as a function of coordinate time.";

(* Old names *)
ReadMemory = ReadSimulationMemoryUsage;
ReadSwap = ReadSimulationSwapUsage;

(* Internal *)
SimulationMemoryPlot;

Begin["`Private`"];

ReadSimulationMemoryUsage[runName_] :=
  If[FindRunFile[runName, "systemstatistics::process_memory_mb.maximum.asc"] =!= {},
     ToDataTable[ReadColumnFile[runName, "systemstatistics::process_memory_mb.maximum.asc", {"time", "maxrss_mb"}]],
     ToDataTable[ReadColumnFile[runName, "MemStats0000.asc", {1, 2}]]];

ReadSimulationSwapUsage[runName_] :=
  ToDataTable[ReadColumnFile[
    runName, "systemstatistics::process_memory_mb.maximum.asc",
    {"time", "swap_used_mb"}]];

SimulationMemoryPlot[runNames1_] :=
  Module[
    {runNames, haveMem},

    haveMem[sim_String] :=
    FindSimulationFiles[sim, "systemstatistics::process_memory_mb.maximum.asc"] =!= {};

    runNames = Select[runNames1, haveMem];
    If[runNames === {},
       None,
       
       Module[
         {swaps, mems},
         swaps = Catch[Catch[Map[ReadSimulationSwapUsage, runNames],UnknownColumns],_];
         If[StringQ[swaps], swaps = {{0,0}}];
         mems = Catch[Catch[Map[ReadSimulationMemoryUsage, runNames],UnknownColumns],_];
         If[StringQ[mems], mems = {{0,0}}];
         
         Show[PresentationListLinePlot[mems, PlotLegend -> runNames, LegendPosition -> {Left, Bottom}],
              PresentationListLinePlot[swaps, PlotStyle->{Dashed}],
              PlotRange -> {0, All}, AxesOrigin->{0,0}, PlotLabel -> "Memory"]]]];

End[];

EndPackage[];
