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
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`Profile`",
  "SimulationTools`Providers`",
  "SimulationTools`RunFiles`",
  "SimulationTools`"
 }];

(* Supported *)
ReadSimulationRunTime::usage = "ReadSimulationRuntime[sim] gives the real time in seconds elapsed during the execution of a simulation.";
ReadSimulationCoreCount::usage    = "ReadSimulationCoreCount[simname] gives the number of cores used by a simulation.";

(* Deprecated *)

ReadWalltimeHours;
ReadWalltime = ReadSimulationRunTime;
ReadCores = ReadSimulationCoreCount;

(* Experimental *)
StandardOutputOfRun;

(* Exceptions *)
NoSimulationCoreCountAvailable;
NoSimulationRunTimeAvailable;

Begin["`Private`"];

ReadWalltime[runName_] :=
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

ReadCores[run_] :=
  If[HaveData["RunFiles", FindRunDir[run]],
    CallProvidedFunction["RunFiles","ReadCores",{FindRunDir[run],run}],
    Error[NoSimulationCoreCountAvailable, "Simulation core count not available in \""<>run<>"\""]];

StandardOutputOfRun[runName_String] :=
  Module[{segments, files1, files2},
    segments = FindRunSegments[runName];
    files1 = Map[FileNameJoin[{#, "../"<>Last@FileNameSplit@runName<>".out"}] &, segments];
    files2 = Select[files1, FileType[#] =!= None &]];

End[];
EndPackage[];
