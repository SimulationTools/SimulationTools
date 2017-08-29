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

BeginPackage["SimulationTools`SimFactory`",
 {
  "SimulationTools`Error`",
  "SimulationTools`IniFile`",
  "SimulationTools`RunFiles`",
  "SimulationTools`SimulationProperties`"
 }];

ReadSimFactoryMetadata;
ReadSimulationJobIdentifiers;

Begin["`Private`"];

SimulationTools`SimFactory`RunFiles`HaveData[runDir_,___] :=
  FileType[FileNameJoin[{runDir, "SIMULATION_ID"}]] =!= None ||
  FileType[FileNameJoin[{runDir, "SIMFACTORY"}]] =!= None;

getDataSubDir[output_String] :=
  Module[
    {parFiles,runName,dir},

    (* First, check if there is a directory with the same name as the run name *)
    runName = FileNameTake[output, {-2}];
    dir = FileNameJoin[{output,runName}];
    If[FileType[dir] === Directory,
       Return[runName]];

    (* If this fails, look for the directory containing the parfile *)
    parFiles = FileNames["*.par", {output}, 2];
    parFiles = Select[parFiles, !StringMatchQ[#, __~~"SIMFACTORY"~~__] &];
    If[Length[parFiles] === 0, Return[None]];
    If[Length[parFiles] =!= 1, Error["Found more than one *.par in " <> output]];
    FileNameTake[parFiles[[1]], {-2}]
];

SimulationTools`SimFactory`RunFiles`FindRunDirSegments[dir_String] :=
  Module[
    {restarts, merged, dataSubDir, dataDirs, segments, parentFile, parentSegs, parentSim},
    parentFile = FileNameJoin[{dir,"parent-simulation.txt"}];
    parentSegs = 
    If[FileExistsQ[parentFile],
      parentSim = Import[parentFile,"Text"];
      FindRunSegments[parentSim],
      (* else *)
      {}];

    restarts = Select[FileNames["output-*", dir], ! StringMatchQ[#, "*" <> $PathnameSeparator <> "output-*-*"] &];
    merged = FileNames["merged", dir];
    dataSubDir = getDataSubDir[restarts[[1]]];
    If[dataSubDir === None, Error["Unable to locate data directories in simulation "<>dir]];
    dataDirs = FileNameJoin[{#, dataSubDir}] & /@ restarts;
    dataDirs = Join[merged, dataDirs];
    segments = Select[dataDirs, DirectoryQ];
    Join[parentSegs, segments]
];

SimulationTools`SimFactory`RunFiles`ReadCores[dir_, runName_] :=
  Module[
    {files, file},
    files = Select[{FileNameJoin[{dir, "SIMFACTORY", "PROCS"}],
                    FileNameJoin[{dir, "PROCS"}],
                    FileNameJoin[{dir, "SIMFACTORY", "properties.ini"}]},
                   FileExistsQ];
    If[Length[files] === 0,
       Error[NoSimulationCoreCountAvailable, "Simulation core count not available in \""<>runName<>"\""]];
    file = Last[files];
    If[FileNameTake[file,-1] === "properties.ini",
       ToExpression@IniVariable[FindRunSegments[runName][[-1]] <> $PathnameSeparator <> ".." <> $PathnameSeparator <> "SIMFACTORY" <> $PathnameSeparator <> "properties.ini", "procs"],
       ReadList[file, Number][[1]]]];

(* TODO: refactor with above function *)
SimulationTools`SimFactory`RunFiles`ReadProcesses[dir_, runName_] :=
  Module[
    {files, file},
    files = Select[{FileNameJoin[{dir, "SIMFACTORY", "PROCS"}],
                    FileNameJoin[{dir, "PROCS"}],
                    FileNameJoin[{dir, "SIMFACTORY", "properties.ini"}]},
                   FileExistsQ];
    If[Length[files] === 0,
       Error[NoSimulationCoreCountAvailable, "Simulation process count not available in \""<>runName<>"\""]];
    file = First[files];
    If[FileNameTake[file,-1] === "properties.ini",
       ToExpression@IniVariable[FindRunSegments[runName][[1]] <> $PathnameSeparator <> ".." <> $PathnameSeparator <> "SIMFACTORY" <> $PathnameSeparator <> "properties.ini", "numprocs"],
       ReadList[file, Number][[1]]]];

SimulationTools`SimFactory`RunFiles`ReadSimulationMachine[dir_String, runName_String] :=
  ReadSimFactoryMetadata[runName, "machine"];

ReadSimFactoryMetadata[simName_, key_] :=
  IniVariable[FindRunSegments[simName][[1]] <> $PathnameSeparator <> ".." <> $PathnameSeparator <> "SIMFACTORY" <> $PathnameSeparator <> "properties.ini", key];


ReadSimulationJobIdentifiers[sim_String] :=
 IniVariable[#, "jobid"] & /@ 
  FindSimulationFiles[sim, ".." <> $PathnameSeparator <> "SIMFACTORY" <> $PathnameSeparator <> "properties.ini"];

End[];

EndPackage[];
