(* Copyright 2014 Ian Hinder

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

BeginPackage["SimulationTools`SpECHack`",
 {
   "SimulationTools`ColumnFile`",
   "SimulationTools`ReadHDF5`",
   "SimulationTools`DataTable`",
   "SimulationTools`DataRepresentations`",
   "h5mma`"
 }];

ReadSpECSimulationSpeed;
ReadSpECSimulationProgress;
ReadSpECWallTime;
ReadSpECMemoryUsage;
ReadSpECPsi4;
ReadSpECHorizonCentroid;
$SpECSimulationsDirectory;
$SpECVerbosePrint;

Begin["`Private`"];

getSimsDir[] :=
  If[ValueQ[$SpECSimulationsDirectory],
    $SpECSimulationsDirectory,
    "~/simulations/spectesting"];

findSpECSegments[sim_String] :=
  Module[{runBase,res,simBase,segPatterns},
    {runBase, res} = StringSplit[sim, ":"];
    simBase = FileNameJoin[{getSimsDir[], runBase, "Ev"}];
    segPatterns = {"Lev"~~res~~"_"~~_~~_, 
      "Lev"~~res~~"_Ringdown/Lev"~~res~~"_"~~_~~_}[[1;;2]];
    Map[FileNames[#, simBase] &, segPatterns]];

findSpECFiles[sim_String, file_String] :=
  Map[Flatten, Map[FileNames[FileNameJoin[{"Run",file}], #] &, findSpECSegments[sim], {2}]];

verbosePrint[] := $SpECVerbosePrint === True;

SetAttributes[dot, HoldFirst];
dot[expr_] :=
  (If[verbosePrint[],WriteString["stdout","."]];
    expr);

print[args___] :=
  If[verbosePrint[],WriteString["stdout", StringJoin[ToString/@{args}]]];

withDot[f_] := Function[g, If[verbosePrint[], WriteString["stdout","."]]; f[g]];
  
Options[readSpECASCIIData] = {"SeparateRingdown" -> False};
readSpECASCIIData[sim_String, file_String, OptionsPattern[]] := readSpECASCIIData[sim,file] =
 Module[{runBase, res, filePattern1, simBase, filePattern2, runFiles, 
   dataSegments, merge},

  runFiles = findSpECFiles[sim, file];

  dataSegments = Map[withDot@ReadColumnFile, runFiles, {2}];
  print["\n"];

  merge[datas_List] :=
   Module[{merged},
     merged = Join@@datas;
     If[! And @@ Positive[Differences[merged[[All, 1]]]],
       monotonisePreferLastCompiled[merged],
       merged]];

  If[!OptionValue[SeparateRingdown],
    merge[Join@@dataSegments],
    Map[merge, dataSegments]]];

readSpECTimeInfo[sim_String] := 
  readSpECASCIIData[sim, "TimeInfo.dat"];

ReadSpECSimulationSpeed[sim_String] :=
  ToDataTable[readSpECTimeInfo[sim][[All,{1,8}]]];

ReadSpECSimulationProgress[sim_String] :=
  ToDataTable[readSpECTimeInfo[sim][[All,{2,1}]]];

ReadSpECWallTime[sim_String] :=
  Module[{inspiral,ringdown},
    {inspiral,ringdown} = readSpECASCIIData[sim,"TimeInfo.dat",SeparateRingdown->True][[All,All,{1,4}]];

    If[Length[ringdown] > 0,
      {inspiral,ringdown} = ToDataTable/@{inspiral,ringdown};
      ToDataTable@Join[ToList@inspiral, ToList[ringdown+Last[inspiral]]],
      (* else *)
      ToDataTable[inspiral]]];

ReadSpECMemoryUsage[sim_String] :=
  Module[{memoryTable = readSpECASCIIData[sim,"MemoryInfo.dat"],
    nFields = 7, offset = 5, virtual = 1, resident = 2, total = 4},
    ToDataTable[Map[{#[[1]], Max[#[[offset+resident;;All;;nFields]]]/(#[[offset+total]]/12)} &,
      memoryTable]]];

ReadSpECPsi4[runName_String, l_?NumberQ, m_?NumberQ, rad_String] :=
  Module[{datasetName, runFiles, files, data, psi4, filePattern1, 
    filePattern2, runBase, res, runFiles2, simBase},
   runFiles = Flatten[findSpECFiles[runName, "GW2/rPsi4_FiniteRadii_CodeUnits.h5"],1];

   datasetName = 
    "/R" <> rad <> ".dir/Y_l" <> ToString[l] <> "_m" <> ToString[m] <>
      ".dat";

  files = Map[withDot[Quiet[Check[ReadHDF5[#, {"Datasets", datasetName}],$Failed,h5mma::mlink],h5mma::mlink] &], runFiles];
  files = DeleteCases[files, $Failed];  (* TODO: We should distinguish between "dataset not found" and other errors *)

  print["\n"];

   data = Join@@files;
   If[! And @@ Positive[Differences[data[[All, 1]]]], 
    data = monotonisePreferLastCompiled[data]];
   psi4 = Map[{#[[1]], #[[2]] + I #[[3]]} &, data];
   Return[MakeDataTable[psi4]]];

ReadSpECHorizonCentroid[runName_String, hn_Integer] :=
 Module[{datasetName, runFiles, files, data, psi4, filePattern1, 
   filePattern2, runBase, res, runFiles2, simBase, hnLetter, data2},

   runFiles = findSpECFiles[runName, "ApparentHorizons/Horizons.h5"][[1]];

  hnLetter = 
   If[hn === 1, "A", 
    If[hn === 2, "B", 
     Error["Unknown horizon index " <> ToString[hn]]]];
  datasetName = "/Ah" <> hnLetter <> ".dir/CoordCenterInertial.dat";
  
  files = Map[withDot[Quiet[Check[ReadHDF5[#, {"Datasets", datasetName}],$Failed,h5mma::mlink],h5mma::mlink] &], runFiles];
  files = DeleteCases[files, $Failed];  (* TODO: We should distinguish between "dataset not found" and other errors *)

  print["\n"];
  data = Join@@files;
  If[! And @@ Positive[Differences[data[[All, 1]]]], 
   data = monotonisePreferLastCompiled[data]];
  data2 = 
   ToDataTable /@ 
    Table[Map[{#[[1]], #[[1 + i]]} &, data], {i, 1, 3}];
  Return[data2]];

monotonisePreferLastCompiled =
 Compile[{{l1, _Real, 2}},
  Module[{l = Reverse[l1], output, j, i, n},
   If[Length[l] < 2, Return[Reverse[l]]];
   n = Length[l[[1]]];
   output = ConstantArray[ConstantArray[0., n], Length[l]];
   output[[1]] = l[[1]]; j = 2;
   For[i = 2, i <= Length[l], i = i + 1,
    
    If[l[[i, 1]] < output[[j - 1, 1]],
     output[[j]] = l[[i]];
     j = j + 1]];
   Reverse[output[[1 ;; j - 1]]]], CompilationTarget -> "C", 
  RuntimeOptions -> "Speed"];

End[];
EndPackage[];
