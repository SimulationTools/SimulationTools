(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["SystemStatistics`", {"RunFiles`", "DataTable`", "Memo`", "Error`", "IniFile`"}];

ReadRunSpeed::usage = "ReadRunSpeed[run] returns a DataTable with the speed of a run in M/hr as a function of time.";
RunCost::usage      = "RunCost[length, speed, nprocs] returns information about the cost of a run.";
ReadMemory::usage   = "ReadMemory[run] returns a DataTable with the swap usage of run as a function of time.";
ReadSwap::usage     = "ReadSwap[run] returns a DataTable with the swap usage of run as a function of time.";
ReadWalltime::usage = "ReadWalltime[run] returns the total walltime used by sim.";
ReadCPUHours::usage = "ReadCPUHours[run] returns the total number of CPU hours used by run.";
CPUHours;
WallTimeDays;
ReadWalltimeHours;
CostAnalysis;
PresentationCostAnalysis;
ReadCarpetSpeed;

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
