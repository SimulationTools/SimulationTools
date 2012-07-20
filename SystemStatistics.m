(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["SystemStatistics`",
 {
  "DataRepresentations`",
  "DataTable`",
  "Error`",
  "IniFile`",
  "Memo`",
  "RunFiles`"
 }];

ReadSimulationSpeed::usage = "ReadSimulationSpeed[simname] gives the execution speed of a simulation (simulation coordinate time per real time elapsed) as a DataTable as a function of simulation coordinate time.";
ReadSimulationMemoryUsage::usage   = "ReadSimulationMemoryUsage[simname] gives the memory usage of a simulation in MB as a DataTable as a function of coordinate time.";
ReadSimulationSwapUsage::usage   = "ReadSimulationSwapUsage[simname] gives the swap memory usage of a simulation in MB as a DataTable as a function of coordinate time.";
ReadSimulationRunTime::usage = "ReadSimulationRuntime[simname] gives the real time elapsed during the execution of a simulation.";
ReadSimulationCost::usage = "ReadSimulationCost[simname] gives the total number of CPU hours used by all processes in a simulation .";
CPUHours;
WallTimeDays;
ReadWalltimeHours;
CostAnalysis;
PresentationCostAnalysis;
ReadCarpetSpeed;

(* Old names *)
ReadRunSpeed = ReadSimulationSpeed;
ReadMemory = ReadSimulationMemoryUsage;
ReadSwap = ReadSimulationSwapUsage;
ReadWalltime = ReadSimulationRunTime;
ReadCPUHours = ReadSimulationCost;

Begin["`Private`"];

RunDirectory := Global`RunDirectory;

ReadRunSpeed[runName_] := 
  If[FindRunFile[runName, "runstats.asc"] =!= {},
     MakeDataTable[ReadColumnFile[runName, "runstats.asc", {2, 4}]],
     ReadCarpetSpeed[runName]];

ReadCarpetSpeed[runName_] :=
  MakeDataTable@ReadColumnFile[runName, "carpet::timing..asc", {"time","physical_time_per_hour"}];

ReadCPUHours[runName_] := 
  ReadWalltimeHours[runName] * ReadCores[runName];

ReadWalltime[runName_] :=
  Module[{segmentTime, files},
    segmentTime[file_] :=
      ReadColumnFile[file, {9, 14}][[-1,2]];
    files = FindRunFile[runName, "carpet::timing..asc"];
    Plus@@(segmentTime /@ files)];

ReadWalltimeHours[runName_] := 
  If[FindRunFile[runName, "carpet::timing..asc"] =!= {},
    ReadWalltime[runName]/3600,
  Error["ReadWalltimeHours: No walltime information in run " <> runName]];

ReadMemory[runName_] :=
  If[FindRunFile[runName, "systemstatistics::process_memory_mb.maximum.asc"] =!= {},
     MakeDataTable[ReadColumnFile[runName, "systemstatistics::process_memory_mb.maximum.asc", {"time", "maxrss_mb"}]],
     MakeDataTable[ReadColumnFile[runName, "MemStats0000.asc", {1, 2}]]];

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
    FileNameSplit /@ FileNames[prefix <> "_*", RunDirectory];
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
