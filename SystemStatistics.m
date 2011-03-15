(* ::Package:: *)

(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["SystemStatistics`", {"RunFiles`", "DataTable`", "Memo`"}];

ReadRunSpeed;
RunCost::usage = "RunCost[length, speed, nprocs] returns information about the cost of a run.";
CPUHours;
WallTimeDays;
ReadMemory;
ReadSwap;
ReadCPUHours;
ReadWalltime;
ReadWalltimeHours;
ReadCores;
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
  Throw["ReadWalltimeHours: No walltime information in run " <> runName]];

ReadMemory[runName_] :=
  If[FindRunFile[runName, "systemstatistics::process_memory_mb.maximum.asc"] =!= {},
     MakeDataTable[ReadColumnFile[runName, "systemstatistics::process_memory_mb.maximum.asc", {"time", "maxrss_mb"}]],
     MakeDataTable[ReadColumnFile[runName, "MemStats0000.asc", {1, 2}]]];

ReadSwap[runName_] :=
  MakeDataTable[ReadColumnFile[
    runName, "systemstatistics::process_memory_mb.maximum.asc",
    {"time", "swap_used_mb"}]];

DefineMemoFunction[ReadIniFile[file_],
  Flatten[StringCases[
    ReadList[file, String],
    key__ ~~ "=" ~~ value___ :> {StringTrim@key -> StringTrim@value}]]];

IniVariable[file_, key_] :=
  Module[{map = ReadIniFile[file]},
    If[!MemberQ[First/@map, key], Throw["Key not found"],
    Return[key /. map]]];

(* These files should be accessed using RunFiles, not directly using
   RunDirectory *)
ReadCores[runName_] :=
  Module[{read, sf1a, sf1b, sf2},

    read[file_, args__] :=
      If[FileType[file] =!= None, ReadList[file, args][[1]], Throw[file, FileNotFound]];

    sf1a = Catch@FindFirstRunFile[runName, "../SIMFACTORY/PROCS"];
    sf1b = Catch@FindFirstRunFile[runName, "../PROCS"];
    sf2 =  Catch@FindFirstRunFile[runName, "../SIMFACTORY/properties.ini"];
    If[FileType[sf1a] =!= None, Return[read[sf1a, Number]]];
    If[FileType[sf1b] =!= None, Return[read[sf1b, Number]]];
    If[FileType[sf2] =!= None, Return[ToExpression@IniVariable[sf2, "procs"]]];
    Throw["Cannot find number of cores in run " <> runName]];

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
