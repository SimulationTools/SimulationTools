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

BeginPackage["SimulationTools`SpEC`",
 {
   "SimulationTools`",
   "SimulationTools`ColumnFile`",
   "SimulationTools`ReadHDF5`",
   "SimulationTools`DataTable`",
   "SimulationTools`DataRepresentations`",
   "SimulationTools`Error`",
   "SimulationTools`FileMemo`",
   "SimulationTools`FileDependencies`",
   "h5mma`",
   "Piraha`",
   "SimulationTools`Utils`",
   If[$VersionNumber >= 10, "GeneralUtilities`", Unevaluated[Sequence[]]]
 }];

ReadSpECSimulationSpeed;
ReadSpECSimulationProgress;
ReadSpECWallTime;
ReadSpECMemoryUsage;
ReadSpECMaxNodeMemoryUsage;
ReadSpECMinEffectiveFreeMemory;
ReadSpECMaxEffectiveUsedMemoryFraction;
ReadSpECPsi4;
ReadSpECPsi4Radii;
ReadSpECStrain;
ReadSpECHorizonCentroid;
ReadSpECHorizonSeparation;
ReadSpECHorizonDisplacement;
ReadSpECInitialDataErrors;
$SpECSimulationsDirectory;
$SpECVerbosePrint;
$ReadSpECPsi4Progress;
FindSpECLevels;
FindSpECEccentricityReductionSimulations;
HaveSpECEvolution;
ReadSpECEccentricity;
FindSpECSimulation;
FindSpECSimulationFile;
FindSpECSubSimulations;
ReadSpECHorizonSpin;
ReadSpECCoreCount;
ReadSpECFinalTime;
ReadSpECADMEnergy;
ReadSpECADMEnergyError;
ReadSpECADMEnergyIterations;
ReadSpECADMAngularMomentum;
ReadSpECInitialADMEnergy;
ReadSpECInitialDataParameter;
FindSpECInitialDataSimulations;
ReadSpECInitialDataIteration;
ReadSpECInitialBindingEnergy;
ReadSpECHorizonMass;
ReadSpECInitialOrbitalFrequency;
ReadSpECInitialSeparation;
ReadSpECHorizonAngularMomentum;
ReadSpECSubdomains;
ReadSpECDomainInfo;
ReadSpECChunkSize;
ReadSpECDampingTime;
ReadSpECAMRTriggerChunkInterval;
ReadSpECAMRTriggerTimes;
ReadSpECGridPoints;
FindSpECSegments;
ReadSXSStrain;
ReadSXSLevels;
FindSpECSimulationFiles;
ReadSpECHDF5Data;
ReadSpECOrbitalOmega;
ReadSpECOrbitalPhase;
ReadSpECSimulationProfileSummary;
DataNotFound;
ReadSXSStrain;
ReadSpECColumnFile;
ReadSpECColumnHeader;
ReadSpECInitialDimensionlessSpin;
ReadSpECInitialMass;
ReadSpECInitialMassRatio;
HaveSpECInitialDataParameters;
ReadSpECASCIIData;
ReadSXSHorizonDisplacement;
ReadSXSOrbitalOmega;
ReadSXSAccumulatedPhase;
ReadSXSMetadataFile;
RotateWaveform;
EulerAngles;
SpECEstimatedInspiralSpeed;
SpECEstimatedMergerTime;
SpECEstimatedRemainingWalltime;
ReadSpECSimulationTerminationReason;
ReadSpECFractionComplete;
PlotSpECSimulationsRemainingTime;

Begin["`Private`"];

(****************************************************************)
(* Misc *)
(****************************************************************)

verbosePrint[] := $SpECVerbosePrint === True;

SetAttributes[dot, HoldFirst];
dot[expr_] :=
  (If[verbosePrint[],WriteString["stdout","."]];
    expr);

print[args___] :=
  If[verbosePrint[],WriteString["stdout", StringJoin[ToString/@{args}]]];

withDot[f_] := Function[g, If[verbosePrint[], WriteString["stdout","."]]; f[g]];

mergeDataTables[ds_List] :=
  ToDataTable[mergeDataLists[ToList/@ds]];

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

monoPreferFirst[d_DataTable] :=
  ToDataTable[monotonisePreferLastCompiled[Reverse[ToList[d]]]];

(****************************************************************)
(* Finding simulations *)
(****************************************************************)

getSimsDir[] :=
  If[ValueQ[$SpECSimulationsDirectory],
    $SpECSimulationsDirectory,
    "/lustre/datura/ianhin/simulations/spectesting"];

(* Given a simulation name or path, return a simulation path *)
FindSpECSimulation[sim_String] :=
  If[StringMatchQ[sim,StartOfString~~("/"|"~")~~__], sim,
    FileNameJoin[{getSimsDir[], sim}]];

findSpECSegments=FindSpECSegments;

FindSpECSegments[sim_String] :=
  Module[{runBase,res,simBase,segPatterns},
    {runBase, res} = Replace[StringSplit[sim, ":"], {a:{_,_} :> a, _ :> Error["Cannot parse simulation name "<>sim<>" into <sim>:<res>"]}];
    simBase = If[StringMatchQ[runBase,StartOfString~~("/"|"~")~~__], FileNameJoin[{runBase,"Ev"}], FileNameJoin[{getSimsDir[], runBase, "Ev"}]];
    segPatterns = {"Lev"~~res~~"_"~~_~~_, 
      "Lev"~~res~~"_Ringdown/Lev"~~res~~"_"~~_~~_}[[1;;2]];
    Map[FileNames[#, simBase] &, segPatterns]];

FindSpECSimulationFiles = findSpECFiles;
findSpECFiles[sim_String, file_String] :=
  Map[Flatten, Map[FileNames[FileNameJoin[{"Run",file}], #] &, findSpECSegments[sim], {2}]];

haveSpECFile[sim_String, file_String] :=
  Flatten[findSpECFiles[sim, file],1] =!= {};

HaveSpECEvolution[sim_String] :=
  Module[{runFiles},
   runFiles = Flatten[findSpECFiles[sim, "GW2/rPsi4_FiniteRadii_CodeUnits.h5"],1];
    runFiles=!={}];

FindSpECLevels[sim_String] :=
  Module[{simBase},
    simBase = If[StringMatchQ[sim,StartOfString~~("/"|"~")~~__], sim, FileNameJoin[{getSimsDir[], sim}]];
    (* Print[FileNameJoin[{simBase,"Ev"}]]; *)
    ToExpression/@Union[Map[StringReplace[StringSplit[FileNameTake[#,-1],"_"][[1]],"Lev"->""] &, FileNames["Lev"~~__~~"_"~~_~~_, FileNameJoin[{simBase,"Ev"}]]]]];

(* This is fairly trivial; it might be redundant *)
FindSpECSimulationFile[sim_String, name_String] :=
  With[{f=FileNameJoin[{FindSpECSimulation[sim], name}]},
    If[FileExistsQ[f], f, Error["File "<>name<>" not found in simulation "<>sim]]];

FindSpECSubSimulations[sim_String, pat_:"*"] :=
  Module[{simPath = FindSpECSimulation[sim], subs1, subs2, subs3},

    (* If this is a top-level simulation *)
    subs1 = Flatten[Map[FileNames[#, simPath] &, {"ID","Ecc*","Ev"}],1];

    (* If this is an Ev or Ringdown simulation *)
    subs2 = FileNames["Lev*_AA", simPath];

    (* If this is a LevN simulation *)
    subs3 = FileNames["Lev*_AA", simPath<>"_Ringdown"];

    Join[subs1, subs2, subs3]]

(****************************************************************)
(* Reading data files *)
(****************************************************************)

mergeDataLists[datas_List] :=
  Module[{merged},
    merged = Join@@datas;
    If[! And @@ Positive[Differences[merged[[All, 1]]]],
      monotonisePreferLastCompiled[merged],
      merged]];
  
Options[readSpECASCIIData] = {"SeparateRingdown" -> False, "SeparateSegments" -> False};
readSpECASCIIData[sim_String, file_String, opts:OptionsPattern[]] := readSpECASCIIData[sim,file,opts] =
 Module[{runFiles, dataSegments},

  runFiles = findSpECFiles[sim, file];

  dataSegments = Map[withDot@ReadColumnFile, runFiles, {2}];
  print["\n"];

  If[!OptionValue[SeparateRingdown],
    If[OptionValue[SeparateSegments],
      Join@@dataSegments,
      (* else *)
      mergeDataLists[Join@@dataSegments]],
    Map[mergeDataLists, dataSegments]]];

ReadSpECHDF5Data[runName_String, filename_String, datasetName_String] :=
 Module[{runFiles, files, data, psi4, filePattern1, 
   filePattern2, runBase, res, runFiles2, simBase, hnLetter, data2},

   (* TODO: combine inspiral and ringdown *)
   runFiles = findSpECFiles[runName, filename][[1]];

   If[runFiles === {}, Return[{}(*ConstantArray[ToDataTable[{}],3]*)] (*Error["Cannot find apparent horizon information in "<>runName]*)];
  
  files = Map[withDot[Quiet[Check[ReadHDF5[#, {"Datasets", datasetName}],$Failed,h5mma::mlink],h5mma::mlink] &], runFiles];
  files = DeleteCases[files, $Failed];  (* TODO: We should distinguish between "dataset not found" and other errors *)

  print["\n"];
  data = Join@@files;
  If[! And @@ Positive[Differences[data[[All, 1]]]], 
   data = monotonisePreferLastCompiled[data]];
  (* data2 =  *)
  (*  ToDataTable /@  *)
  (*   Table[Map[{#[[1]], #[[1 + i]]} &, data], {i, 1, Length[data[[1]]]}]; *)
  Return[data]];

ReadSpECOrbitalOmega[sim_String] :=
  Module[{data},
    data = ReadSpECHDF5Data[sim, "OrbitDiagnostics.h5", "OmegaVector.dat"];
    If[Length[data] === 0, Error["No orbital omega data in "<>sim]];
    {t, omx, omy, omz} = Transpose[data];
    ToDataTable[t, #] & /@ {omx, omy, omz}];

ReadSpECOrbitalPhase[sim_String] :=
  Module[{data},
    data = ReadSpECHDF5Data[sim, "OrbitDiagnostics.h5", "OrbitalPhase.dat"];
    {t, phi} = Transpose[data];
    ToDataTable[t, phi]];

(****************************************************************)
(* Simulation performance information *)
(****************************************************************)

readSpECTimeInfo[sim_String] := 
  readSpECASCIIData[sim, "TimeInfo.dat"];

readSpECChangeNP[sim_String] := 
  readSpECASCIIData[sim, "ChangeNp.dat"];

haveChangeNP[sim_String] :=
  haveSpECFile[sim, "ChangeNp.dat"];

ReadSpECSimulationSpeed[sim_String] :=
  ToDataTable[readSpECTimeInfo[sim][[All,{1,8}]]];

ReadSpECCoreCount[sim_String] :=
  ToDataTable[readSpECTimeInfo[sim][[All,{1,3}]]];

ReadSpECSimulationProgress[sim_String] :=
  Module[{prog},
    prog = readSpECTimeInfo[sim][[All,{2,1}]];
    (* Filter out any times which are less than the start time.  This
       indicates a problem with the clock on the compute node. *)
    prog = Select[prog, #[[1]] >= prog[[1,1]] &];
    ToDataTable[prog]];

ReadSpECWallTime[sim_String] :=
  Module[{inspiral,ringdown},
    {inspiral,ringdown} = readSpECASCIIData[sim,"TimeInfo.dat",SeparateRingdown->True][[All,All,{1,4}]];

    If[Length[ringdown] > 0,
      {inspiral,ringdown} = ToDataTable/@{inspiral,ringdown};
      ToDataTable@Join[ToList@inspiral, ToList[ringdown+Last[inspiral]]],
      (* else *)
      ToDataTable[inspiral]]];

(****************************************************************)
(* Simulation memory usage *)
(****************************************************************)

(* TODO: make this work when the number of processes changes between segments *)
(* Make this call the below functions to give a single total number *)
ReadSpECMemoryUsage[sim_String] :=
  Module[{memoryTable = readSpECASCIIData[sim,"MemoryInfo.dat"],
    nFields = 7, offset = 5, (*virtual = 1, *)resident = 2, total = 4},
    ToDataTable[Map[{#[[1]], Max[#[[offset+resident;;All;;nFields]]]/(#[[offset+total]]/12)} &,
      memoryTable]]];

(* List (one per process) of DataTables with the memory usage as a
   function of time *)
memoryField[memoryTable_List, field_String] :=
  Module[{fieldNumber, nProcs, time, data, nFields = 7, offset = 5,
    timeCol = 1},

    (* TODO: get number of processes and column format from file header; this
       is not very robust at the moment *)
    fieldNumber = Replace[field,
      { "VirtualMemory" -> 1,
        "ResidentMemory" -> 2,
        "PeakResidentMemory" -> 3,
        "SystemTotalRam" -> 4,
        "SystemEffectiveMemFree" -> 5,
        "SystemCommitted" -> 6,
        "SystemCommitLimit" -> 7,
        _ :> Error["Unrecognised memory field "<>field]}];

    nProcs = (Dimensions[memoryTable][[2]] - offset)/nFields;
    time = memoryTable[[All, timeCol]];
    data = 
    memoryTable[[All, offset + fieldNumber ;; All ;; nFields]];
    Map[ToDataTable[time, #] &, Transpose[data]]];

memoryField[memoryTable_List, "SystemEffectiveMemUsed"] :=
  memoryField[memoryTable, "SystemTotalRam"] - memoryField[memoryTable, "SystemEffectiveMemFree"];

memoryField[memoryTable_List, "SystemEffectiveMemUsedFraction"] :=
  1 - memoryField[memoryTable, "SystemEffectiveMemFree"]/memoryField[memoryTable, "SystemTotalRam"];

(* Return a list (one per segment) of lists (one per process) of
   DataTables with the memory usage as a function of time *)
ReadSpECProcessMemoryUsage[sim_String, field_String : "ResidentMemory"] :=
 Module[{memoryTables},
   (* We need to treat each segment separately because it might have a
      different number of processes, and hence a table of different
      dimensions *)
   memoryTables = readSpECASCIIData[sim, "MemoryInfo.dat", SeparateSegments -> True];
   Map[memoryField[#, field] &, memoryTables]];

maxNodeMemoryUsage[procMems_List] :=
 Module[{nodeMems, maxNodeMems, coresPerNode = 12},
  (* TODO: Get number of processes per node from some simulation metadata *)
  nodeMems = Map[Total, Partition[procMems, coresPerNode]];
  maxNodeMems = MapThread[Max, nodeMems]];

ReadSpECMaxNodeMemoryUsage[sim_String] :=
 Module[{},
   procMemsSegs = ReadSpECProcessMemoryUsage[sim];
   maxNodeMemss = Map[maxNodeMemoryUsage, procMemsSegs];
   ToDataTable[mergeDataLists[ToList/@maxNodeMemss]]];

ReadSpECMinEffectiveFreeMemory[sim_String] :=
  mergeDataTables[Map[MapThread[Min, #] &, 
    ReadSpECProcessMemoryUsage[sim, "SystemEffectiveMemFree"]]];

ReadSpECMaxEffectiveUsedMemory[sim_String] :=
  mergeDataTables[Map[MapThread[Max, #] &, 
    ReadSpECProcessMemoryUsage[sim, "SystemEffectiveMemUsed"]]];

ReadSpECMaxEffectiveUsedMemoryFraction[sim_String] :=
  mergeDataTables[Map[MapThread[Max, #] &, 
    ReadSpECProcessMemoryUsage[sim, "SystemEffectiveMemUsedFraction"]]];

(****************************************************************)
(* Waveforms *)
(****************************************************************)

radiusString[r_Integer] :=
  IntegerString[r, 10, 4];

radiusString[r_String] :=
  r;

radiusOfDatasetName[dsName_String] :=
  Replace[StringCases[dsName, "/R"~~x__~~".dir"->x],
    {{s_String} :> s,
      s_ :> Error["Cannot determine radius from dataset name "<>dsName]}];

ReadSpECPsi4Radii[runName_String] :=
  Module[{runFiles},
    runFiles = Flatten[findSpECFiles[runName, "GW2/rPsi4_FiniteRadii_CodeUnits.h5"],1];
    If[runFiles === {},
      {},
      ToExpression/@Union[radiusOfDatasetName/@ReadHDF5[runFiles[[1]]]]]];

ReadSpECPsi4[runName_String, l_?NumberQ, m_?NumberQ, rad_] :=
  Module[{datasetName, runFiles, files, data, psi4, radStr},
  radStr = radiusString[rad];
   runFiles = Flatten[findSpECFiles[runName, "GW2/rPsi4_FiniteRadii_CodeUnits.h5"],1];
If[runFiles==={},Error["No Psi4 data found in "<>runName]];
   datasetName = 
    "/R" <> radStr <> ".dir/Y_l" <> ToString[l] <> "_m" <> ToString[m] <>
      ".dat";

  (* TODO: This progress variable doesn't work due to the interaction
     with withDot.  Unify this system. *)
  files = MapIndexed[withDot[$ReadSpECPsi4Progress=N[#2[[1]]/Length[runFiles]]; Quiet[Check[ReadHDF5[#, {"Datasets", datasetName}],$Failed,h5mma`h5mma::mlink],h5mma`h5mma::mlink] &], runFiles];
  files = DeleteCases[files, $Failed];  (* TODO: We should distinguish between "dataset not found" and other errors *)
  $ReadSpECPsi4Progress=.;
  print["\n"];

   data = Join@@files;
   If[! And @@ Positive[Differences[data[[All, 1]]]], 
    data = monotonisePreferLastCompiled[data]];
   psi4 = Map[{#[[1]], #[[2]] + I #[[3]]} &, data];
   Return[MakeDataTable[psi4]]];

(* TODO: Eliminate duplication with ReadSpECPsi4 *)
ReadSpECStrain[runName_String, l_?NumberQ, m_?NumberQ, rad_] :=
  Module[{datasetName, runFiles, files, data, psi4, radStr},
  radStr = radiusString[rad];
   runFiles = Flatten[findSpECFiles[runName, "GW2/rh_FiniteRadii_CodeUnits.h5"],1];
If[runFiles==={},Error[DataNotFound, "No strain data found in "<>runName]];
   datasetName = 
    "/R" <> radStr <> ".dir/Y_l" <> ToString[l] <> "_m" <> ToString[m] <>
      ".dat";

  files = Map[withDot[Quiet[Check[ReadHDF5[#, {"Datasets", datasetName}],$Failed,h5mma`h5mma::mlink],h5mma`h5mma::mlink] &], runFiles];
  files = DeleteCases[files, $Failed];  (* TODO: We should distinguish between "dataset not found" and other errors *)

If[files === {}, Print["No strain data found in " <> runName <> " (no datasets named "<>datasetName<>")"]; Error[DataNotFound, "No strain data found in " <> runName <> " (no datasets named "<>datasetName<>")"]];

  print["\n"];

   data = Join@@files;
   If[! And @@ Positive[Differences[data[[All, 1]]]], 
    data = monotonisePreferLastCompiled[data]];
   psi4 = Map[{#[[1]], #[[2]] + I #[[3]]} &, data];
   Return[MakeDataTable[psi4]]];

firstOrFail[l_List, msg_String] :=
  If[Length[l] === 0, Error[msg], First[l]];

(****************************************************************)
(* Horizons *)
(****************************************************************)

(* TODO: Eliminate HDF5 file reading/merging duplication *)
ReadSpECHorizonCentroid[runName_String, hn_Integer] :=
 Module[{datasetName, runFiles, files, data, hnLetter, data2},

   runFiles = findSpECFiles[runName, "ApparentHorizons/Horizons.h5"][[1]];

   If[runFiles === {}, Return[ConstantArray[ToDataTable[{}],3]] (*Error["Cannot find apparent horizon information in "<>runName]*)];

  hnLetter = 
   If[hn === 1, "A", 
    If[hn === 2, "B", 
     Error["Unknown horizon index " <> ToString[hn]]]];
  datasetName = "/Ah" <> hnLetter <> ".dir/CoordCenterInertial.dat";
  
  files = Map[withDot[Quiet[Check[ReadHDF5[#, {"Datasets", datasetName}],$Failed,h5mma`h5mma::mlink],h5mma`h5mma::mlink] &], runFiles];
  files = DeleteCases[files, $Failed];  (* TODO: We should distinguish between "dataset not found" and other errors *)

  print["\n"];
  data = Join@@files;
  If[! And @@ Positive[Differences[data[[All, 1]]]], 
   data = monotonisePreferLastCompiled[data]];
  data2 = 
   ToDataTable /@ 
    Table[Map[{#[[1]], #[[1 + i]]} &, data], {i, 1, 3}];
  Return[data2]];

ReadSpECHorizonDisplacement[sim_String] :=
  Subtract@@Table[ReadSpECHorizonCentroid[sim, h], {h,1,2}];

ReadSpECHorizonSeparation[sim_String] :=
  Norm[ReadSpECHorizonDisplacement[sim]];

(* TODO: Eliminate HDF5 file reading/merging duplication *)
ReadSpECHorizonSpin[runName_String, hn_Integer] :=
 Module[{datasetName, runFiles, files, data, hnLetter, data2},

   runFiles = Flatten[findSpECFiles[runName, "ApparentHorizons/Horizons.h5"]];

   If[runFiles === {}, Return[ConstantArray[ToDataTable[{}],3]] (*Error["Cannot find apparent horizon information in "<>runName]*)];

  hnLetter = 
   If[hn === 1, "A", 
    If[hn === 2, "B", 
      If[hn === 3, "C", 
     Error["Unknown horizon index " <> ToString[hn]]]]];
  datasetName = "/Ah" <> hnLetter <> ".dir/chiInertial.dat";
  
  files = Map[withDot[Quiet[Check[ReadHDF5[#, {"Datasets", datasetName}],$Failed,h5mma`h5mma::mlink],h5mma`h5mma::mlink] &], runFiles];
  files = DeleteCases[files, $Failed];  (* TODO: We should distinguish between "dataset not found" and other errors *)

  print["\n"];
  data = Join@@files;
  If[! And @@ Positive[Differences[data[[All, 1]]]], 
   data = monotonisePreferLastCompiled[data]];
  data2 = 
   ToDataTable /@ 
    Table[Map[{#[[1]], #[[1 + i]]} &, data], {i, 1, 3}];
  Return[data2]];

(* TODO: Eliminate HDF5 file reading/merging duplication *)
ReadSpECHorizonAngularMomentum[runName_String, hn_Integer] :=
 Module[{datasetName, runFiles, files, data, hnLetter, data2},

   runFiles = Flatten[findSpECFiles[runName, "ApparentHorizons/Horizons.h5"]];

   If[runFiles === {}, Return[ConstantArray[ToDataTable[{}],3]] (*Error["Cannot find apparent horizon information in "<>runName]*)];

  hnLetter = 
   If[hn === 1, "A", 
    If[hn === 2, "B", 
      If[hn === 3, "C", 
     Error["Unknown horizon index " <> ToString[hn]]]]];
  datasetName = "/Ah" <> hnLetter <> ".dir/DimensionfulInertialSpin.dat";
  
  files = Map[withDot[Quiet[Check[ReadHDF5[#, {"Datasets", datasetName}],$Failed,h5mma`h5mma::mlink],h5mma`h5mma::mlink] &], runFiles];
  files = DeleteCases[files, $Failed];  (* TODO: We should distinguish between "dataset not found" and other errors *)

  print["\n"];
  data = Join@@files;
  If[! And @@ Positive[Differences[data[[All, 1]]]], 
   data = monotonisePreferLastCompiled[data]];
  data2 = 
   ToDataTable /@ 
    Table[Map[{#[[1]], #[[1 + i]]} &, data], {i, 1, 3}];
  Return[data2]];

(* TODO: Eliminate HDF5 file reading/merging duplication *)
ReadSpECHorizonMass[runName_String, hn_Integer] :=
 Module[{datasetName, runFiles, files, data, hnLetter, data2},

   runFiles = findSpECFiles[runName, "ApparentHorizons/Horizons.h5"];
(* Print["runFiles = ", runFiles]; *)

   runFiles = Flatten[runFiles];

   If[runFiles === {}, Return[ToDataTable[{}]] (*Error["Cannot find apparent horizon information in "<>runName]*)];

  hnLetter = 
   If[hn === 1, "A", 
    If[hn === 2, "B", 
      If[hn === 3, "C", 
        Error["Unknown horizon index " <> ToString[hn]]]]];
  datasetName = "/Ah" <> hnLetter <> ".dir/ChristodoulouMass.dat";
  
  files = Map[withDot[Quiet[Check[ReadHDF5[#, {"Datasets", datasetName}],$Failed,h5mma`h5mma::mlink],h5mma`h5mma::mlink] &], runFiles];
  files = DeleteCases[files, $Failed];  (* TODO: We should distinguish between "dataset not found" and other errors *)

  print["\n"];
  data = Join@@files;
  If[! And @@ Positive[Differences[data[[All, 1]]]], 
   data = monotonisePreferLastCompiled[data]];
  data2 = 
   ToDataTable[data];
  Return[data2]];

(****************************************************************)
(* Initial data *)
(****************************************************************)

findSpECInitialDataDir[sim_String] :=
  If[StringMatchQ[sim,StartOfString~~("/"|"~")~~__],
    FileNameJoin[{sim,"ID"}],
    FileNameJoin[{getSimsDir[], sim, "ID"}]];

ReadSpECInitialDataErrors[sim_String] :=
  Module[{idDir = findSpECInitialDataDir[sim]},
    ToDataTable@
    Map[{#[[1]], Norm[#[[3 ;; All ;; 2]]]} &, 
      ReadColumnFile[idDir, "Errors.dat"]]];

ReadSpECADMEnergy[sim_String] :=
  Module[{idDir = findSpECInitialDataDir[sim]},
    ReadColumnFile[idDir,"PhysValues.dat", {1,13}][[-1,2]]];

ReadSpECADMEnergyError[sim_String] :=
  Module[{idDir = findSpECInitialDataDir[sim], eADMs, levels, lowRes, highRes},
    eADMs = Flatten[ReadColumnFile[idDir,"PhysValues.dat", {13}]];
    levels = Flatten[ReadColumnFile[idDir,"Errors.dat", {2}]];

    lowRes = -(LengthWhile[Reverse@levels, (# === levels[[-1]]) &] + 1);
    highRes = -1;

    Norm[Differences[eADMs[[lowRes;;highRes]]]]];

    (* Print[lowRes]; *)
    (* Abs[eADMs[[highRes]] - eADMs[[lowRes]]]]; *)

    (* Abs[Subtract@@(ReadColumnFile[idDir,"PhysValues.dat", {1,13}][[-2;;-1,2]])]]; *)

ReadSpECADMEnergyIterations[sim_String] :=
  Module[{idDir = findSpECInitialDataDir[sim]},
    ReadColumnFile[idDir,"PhysValues.dat", {1,13}][[All,2]]];

ReadSpECInitialDataParameter[sim_String, paramName_String] :=
 
 Module[{path, paramsFile, paramsString, matches, parseScalar, 
   parseArray},
  path = StringSplit[FindSpECSimulation[sim], ":"][[1]];
  paramsFile = FileNameJoin[{path, "ID/EvID/ID_Params.perl"}];
  If[! FileExistsQ[paramsFile], 
   Error["Cannot find initial data parameter file " <> paramsFile <> 
     " for simulation " <> sim]];
  paramsString = Import[paramsFile, "String"];
  
  matches = 
   StringCases[paramsString, 
    StartOfLine ~~ Whitespace ... ~~ (type : ("$" | "@")) ~~ 
      "ID_" ~~ paramName ~~ Whitespace ... ~~ "=" ~~ Whitespace ... ~~ 
      Shortest[val___ ~~ Whitespace ... ~~ ";"] :> {type, val}];
  
  parseScalar[s_String] :=
   If[StringMatchQ[s, "\"" ~~ x__],
    s,
    ImportString[s, "List"][[1]]];
  
  parseArray[s_String] :=
   
   Replace[StringCases[s, "(" ~~ x__ ~~ ")" :> x],
    {{x_} :> 
      parseScalar /@ 
       StringSplit[x, Whitespace ... ~~ "," ~~ Whitespace ...],
     _ :> 
      Error["Array parse error when reading initial data parameter " <>
         paramName <> " from simulation " <> sim]}];
  
  Replace[matches,
   {{{"$", val_}} :> parseScalar[val],
    {{"@", val_}} :> parseArray[val],
    {} :> 
     Error["Initial data parameter " <> paramName <> 
       " not found in simulation " <> sim],
    (l : {{_, _}}) /; Length[l] > 1 :> 
     Error["Initial data parameter " <> paramName <> 
       " found more than once in simulation " <> sim],
    _ :> Error[
      "Parse error when reading initial data parameter " <> 
       paramName <> " from simulation " <> sim]}]]  

FindSpECInitialDataSimulations[sim_String] :=
 Module[{path, idSimPaths},
  path = FindSpECSimulation[sim];
  idSimPaths = FileNames["ID", path, Infinity]];

ReadSpECInitialDataIteration[sim_String] :=
 Module[{errs},
  errs = ReadColumnFile[
    FileNameJoin[{FindSpECSimulation[sim], "private"}], 
    "Errors.dat"];
  errs[[-1, 1]]];

ReadSpECInitialADMEnergy[sim_] :=
 ReadSpECInitialDataParameter[sim, "Eadm"];

ReadSpECInitialBindingEnergy[sim_] :=
 ReadSpECInitialDataParameter[sim, 
   "Eadm"] - (ReadSpECInitialDataParameter[sim, "MA"] + 
    ReadSpECInitialDataParameter[sim, "MB"]);

ReadSpECInitialSeparation[sim_] :=
 ReadSpECInitialDataParameter[sim, "d"];

ReadSpECADMAngularMomentum[sim_] :=
 ReadSpECInitialDataParameter[sim, "Jadm"];

ReadSpECInitialOrbitalFrequency[sim_] :=
 ReadSpECInitialDataParameter[sim, "Omega0"];

ReadSpECInitialDimensionlessSpin[sim_, i_] :=
 ReadSpECInitialDataParameter[sim, {"chiA","chiB"}[[i]]];

ReadSpECInitialMass[sim_, i_] :=
 ReadSpECInitialDataParameter[sim, {"MA","MB"}[[i]]];

ReadSpECInitialMassRatio[sim_] :=
  ReadSpECInitialMass[sim,1] / ReadSpECInitialMass[sim,2];

(****************************************************************)
(* Eccentricity reduction *)
(****************************************************************)

FindSpECEccentricityReductionSimulations[sim_String] :=
  FileNames["Ecc"~~NumberString, FindSpECSimulation[sim]];

ReadSpECEccentricity[sim_String, joinDirArg_ : Automatic] :=
  Module[{simPath, dirNames, eccFiles, eccFile, eMeasured, eMeasuredSS},

    simPath = FindSpECSimulation[sim];

    If[joinDirArg === Automatic,
      dirNames = {"JoinedForEcc", "JoinedForEcc_Lev*"},
      dirNames = {joinDirArg}];

    eccFiles = FileNames["Fit_F2cos2.dat", FileNameJoin[{simPath, "Ev", #}] & /@ dirNames];


    If[Length[eccFiles] === 0,
      Print["Cannot find eccentricity reduction file"];
      Return[None]];

    eccFile = First[eccFiles];

    eMeasured = Import[eccFile,"Table"][[-1,-1]];
    eMeasuredSS = Import[StringReplace[eccFile,"Fit_F2cos2.dat"->"Fit_F2cos2_SS.dat"],"Table"][[-1,-1]];

    If[eMeasuredSS > 0.01, eMeasured, eMeasuredSS]];

(****************************************************************)
(* Misc simulation properties *)
(****************************************************************)

(* Final time -- currently only works for eccentricity reduction simulations *)

ReadSpECFinalTime[sim_String] :=
 Module[{evolutionInputFile, evolutionInput, finalTime},
  evolutionInputFile = 
   Last[Flatten[
     findSpECFiles[sim, 
      "Evolution.input"]]];
  evolutionInput = 
   CleanParseTree@
    ParsePEG[
     (*"/home/ianhin/projects/SimulationTools/Grammars/specinput.peg"*) "specinput.peg", 
     "file", evolutionInputFile];
  finalTime = 
   lookup[lookup[
      lookup[lookup[process[evolutionInput[[1]]], "Terminator"][[1]], 
       "TerminationCriteria"], "EccentricityReduction"], 
     "FinalTime"][[1]];
  If[StringMatchQ[finalTime, NumberString], ToExpression[finalTime], 
   Error["Simulation " <> sim <> " has a final time of " <> 
     finalTime <> " which is not a number"]]]

(****************************************************************)
(* Input files *)
(****************************************************************)

process["assignmentsequence"[as___]] :=
 Map[process, {as}];

process["assignment"["key"[key_], "valuesequence"[values__]]] :=
  key -> process /@ {values};

process[s_String] := s;

process["value"[v_]] := process[v];

process["keywordvalue"["keyword"[k_], 
   a : "assignmentsequence"[___]]] :=
 k[process[a]];

process["keywordvalue"["keyword"[k_], 
   a : "assignmentsequence"[___]]] :=
 k[process[a]];

process["null"["()"]] := Null;

process[x_] := Error["Do not know how to process " <> ToString[x]];

process[x_] := x;

lookup[assoc_List, key_String] :=
 Replace[Cases[assoc, ((key -> value_) | key[value_]) :> value],
  {{x_} :> x,
   _ :> Error["Cannot find key " <> key]}];

(****************************************************************)
(* Grid structure *)
(****************************************************************)

(* TODO: this doesn't work on all input files yet, so can't be used *)
ReadSpECAMRTriggerChunkInterval[sim_String] :=
 Module[{inputFile, input, triggerInterval, amrDriver, changeSpectralGrid},
  inputFile = 
   Last[Flatten[
     findSpECFiles[sim, 
      "AmrDriver.input"]]];

  input = 
   CleanParseTree@
    ParsePEG[
     "specinput.peg", 
     "file", inputFile];

  (* Print["input = ", input]; *)

  (* Print["processed = ", process[input[[1]]]]; *)

  amrDriver = lookup[process[input[[1]]], "AmrDriver"];
  (* Print["amrDriver = ", amrDriver]; *)

  changeSpectralGrid = lookup[amrDriver, "ChangeSpectralGrid"];
  (* Print["changeSpectralGrid = ", changeSpectralGrid]; *)

  triggerInterval = lookup[changeSpectralGrid, "TriggerEveryNChunks"];
  (* Print["triggerInterval = ", triggerInterval]; *)

  If[StringMatchQ[triggerInterval[[1]], NumberString], ToExpression[triggerInterval[[1]]], 
   Error["Simulation " <> sim <> " has a trigger chunk interval of " <> 
     triggerInterval[[1]] <> " which is not a number"]]]

(* Domain Info *)

WithFileMemo[
ReadSpECSegmentDomainInfo[segmentPath_String] := 
 (* ReadSpECSegmentDomainInfo[segmentPath] = *)
  Module[{info, rows, fileName},

    fileName = FileNameJoin[{segmentPath, "Run", "Hist-Domain.txt"}];
    DeclareFileDependency[fileName];

   If[! FileExistsQ[fileName],
    Return[{}]];
Print[{FileNameJoin[{$SimulationToolsInstallationDirectory, "spec-domain-info"}], 
       segmentPath}];

   info = 
    RunSubprocess[{FileNameJoin[{$SimulationToolsInstallationDirectory, "spec-domain-info"}], 
       segmentPath}][[2]];
   rows = DeleteCases[info, s_ /; StringMatchQ[s, "#" ~~ ___]];
   (*Return[rows];*)
   Map[ImportString[#, "Table"][[1]] &, rows]]];

combineDomainInfos[segInfos_List] :=
 Module[{domainInfo},
  domainInfo = segInfos;
  domainInfo = DeleteCases[domainInfo, {}];
  domainInfo = 
   Join[{domainInfo[[1]]}, Map[Rest, domainInfo[[2 ;; All]]]];
  domainInfo = Reverse[Flatten[domainInfo, 1]];
  domainInfo = 
   Reverse[DeleteDuplicates[domainInfo, #1[[1]] === #2[[1]] &]];
  domainInfo];

ReadSpECDomainInfo[sim_String] :=
 Module[{segments},
  segments = 
   Flatten[findSpECSegments[sim], 1];
  combineDomainInfos[
   Map[ReadSpECSegmentDomainInfo, segments]]];

ReadSpECPointDistribution[sim_String] :=
 Module[{domainInfo, f},
  domainInfo = ReadSpECDomainInfo[sim];
  Table[ToDataTable[domainInfo[[All, 1]], 
    Map[f[Drop[#, 2]] &, domainInfo]], {f, {Mean, Min, Max, 
     StandardDeviation}}]];

ReadSpECChunkSize[sim_String] :=
  ToDataTable[readSpECASCIIData[sim, "AdjustSubChunksToDampingTimes.dat"][[All,{1,3}]]];

triggerTimesFromFile[fileName_String] :=
  Module[{s},
    s = Import[fileName, "String"];
    StringCases[s, "ChangeSpectralGrid Called at t="~~Shortest[t__]~~"," :> t,
      IgnoreCase -> True]];

ReadSpECAMRTriggerTimes[sim_String] :=
  Union[
    ToExpression/@
    Flatten[
      triggerTimesFromFile /@ Flatten[
        findSpECFiles[sim, "ChangeSpectralGrid.log"]]]];

(****************************************************************)
(* Grid structure (new implementation) *)
(****************************************************************)

gridFiles[sim_String] :=
 Flatten[FindSpECSimulationFiles[sim, "AdjustGridExtents.h5"][[1]]];

extentDataset[sd_] :=
 StringJoin[sd, "/Extents.dat"];

gridPointsInSubdomain[gridFile_, sdname_] :=
 Module[{datasets},
  (*Print["segfile=",segfile];
  Print["extendDataset = ",extentDataset[sdname]];*)
  datasets = 
   ImportHDF5[gridFile, {"Datasets", extentDataset[sdname]}];
  ToDataTable@Map[{#[[1]], Times @@ #[[{2, 3, 4}]]} &, datasets]];

readSpECGridPointsInFile[gridFile_, sdPattern_: "*"] :=
 Module[{sds, gpsInsds},
  (*Print["gridFile=",gridFile];*)
  sds = subdomainsInFile[gridFile, sdPattern];
  (* Print[sds]; *)
  gpsInsds = gridPointsInSubdomain[gridFile, #] & /@ sds;
  If[Length[sds] === 0,
    ToDataTable[{},{}],
    Total[gpsInsds]]];

subdomainsInFile[gridFile_String, sdPattern_: "*.dir"] :=
 Select[ImportHDF5[gridFile, "Datasets"], StringMatchQ[#, sdPattern] &];

ReadSpECGridPoints[sim_String, sdPattern_: "*.dir"] :=
 Module[{segpoints},
  segpoints = 
   readSpECGridPointsInFile[#, sdPattern] & /@ gridFiles[sim];
  (*Print["segpoints=",segpoints];*)
  
  ToDataTable[
   SimulationTools`SpEC`Private`monotonisePreferLastCompiled[
    Join[Sequence @@ (ToList /@ segpoints)]]]];

ReadSpECSubdomains[sim_String, sdPattern_: "*.dir"] :=
 Module[{subdomains, segpoints, subdomainCounts, subdomainsList},
  segpoints = 
   readSpECGridPointsInFile[#, sdPattern] & /@ gridFiles[sim];
  Print["segpoints = ", segpoints];
  subdomainCounts = 
   Length[subdomainsInFile[#, sdPattern]] & /@ gridFiles[sim];
  Print["subdomainCounts = ", subdomainCounts];
  subdomains = subdomainCounts + 0*segpoints;
  Print["subdomains = ", subdomains];

  (*Print["segpoints=",segpoints];*)
  
  Print["ReadSpECSubdomains: subdomains = ", subdomains];

  subdomainsList = ToList /@ subdomains;

  If[MatchQ[subdomainsList, {_List...}] === False,
    Error["ReadSpECSubdomains: subdomainsList is not a list of lists.  Instead it is "<>ToString[subdomainsList]]];

  ToDataTable[
   SimulationTools`SpEC`Private`monotonisePreferLastCompiled[
    Join[Sequence @@ (subdomainsList)]]]];

(****************************************************************)
(* Control system *)
(****************************************************************)

ReadSpECDampingTime[sim_String, i_Integer] :=
  ToDataTable[readSpECASCIIData[sim, "AdjustSubChunksToDampingTimes.dat", SeparateRingdown->True][[1]][[All,{1,4+i}]]];

ReadSXSStrain[sim_String] :=
 ToDataTable[{#[[1]], #[[2]] + I #[[3]]} & /@ 
   Import[
    FileNameJoin[{sim, 
      "rhOverM_Asymptotic_GeometricUnits.h5"}], {"Datasets", 
     "/Extrapolated_N2.dir/Y_l2_m2.dat"}]];

Options[ReadSXSStrain] = {"FileName" -> "rhOverM_Asymptotic_GeometricUnits.h5"};
ReadSXSStrain[sim_String, l_Integer, m_Integer, ord_Integer, opts:OptionsPattern[]] := 
 (* ReadSXSStrain[sim, l, m, ord] = *)
  Module[{file = sim <> "/"<>OptionValue[FileName]},
   Block[{$status = {sim, l, m, ord}},
    If[! FileExistsQ[file], Error["Cannot find " <> file]];
    ToDataTable @@ ({#1, #2 + I #3} &) @@ 
      Transpose[
       ReadHDF5[
        file, {"Datasets", 
         "/Extrapolated_N" <> ToString[ord] <> ".dir/Y_l" <> 
          ToString[l] <> "_m" <> ToString[m] <> ".dat"}]]]];

ReadSXSLevels[sim_String] :=
  Sort[FileNames["Lev*", sim]];

End[];
EndPackage[];

(****************************************************************)
(* Profile *)
(****************************************************************)

ReadSpECSimulationProfileSummary[sim_String] :=
 Module[{profileFiles, summaryDatasets, dsFile, steps, summaryQ},
  summaryQ[s_String] := StringMatchQ[s, "/Step*.dir/Summary.txt"];
  profileFiles = 
   FindSpECSimulationFiles[sims[[1]], "IncProfiler.h5"];
  summaryDatasets = 
   Flatten[Map[{#, Select[Import[#], summaryQ]} &, profileFiles, {2}],
     1];
  {dsFile, steps} = Last[summaryDatasets];
  ImportHDF5[dsFile, {"Datasets", Last[steps]}]];

(****************************************************************)
(* SXS data format *)
(****************************************************************)

Options[ReadSXSStrain] = {"FileName" -> "rhOverM_Asymptotic_GeometricUnits.h5"};
ReadSXSStrain[sim_String, l_Integer, m_Integer, ord_Integer, opts:OptionsPattern[]] := 
 ReadSXSStrain[sim, l, m, ord] =
  Module[{file = sim <> "/"<>OptionValue[FileName]},
   Block[{$status = {sim, l, m, ord}},
    If[! FileExistsQ[file], Error["Cannot find " <> file]];
    ToDataTable @@ ({#1, #2 + I #3} &) @@ 
      Transpose[
       ImportHDF5[
        file, {"Datasets", 
         "/Extrapolated_N" <> ToString[ord] <> ".dir/Y_l" <> 
          ToString[l] <> "_m" <> ToString[m] <> ".dat"}]]]];

ReadSXSSpin[sim_, hn_Integer] :=
 Module[{t, sx, sy, sz, hnAlpha = {"A", "B"}[[hn]]},
  {t, sx, sy, sz} = 
   Transpose[
    ImportHDF5[
     sim <> "/Horizons.h5", {"Datasets", 
      "/Ah" <> hnAlpha <> ".dir/DimensionfulInertialSpin.dat"}]];
  ToDataTable[t, #] & /@ {sx, sy, sz}];

ReadSXSLevels[sim_String] :=
  Sort[FileNames["Lev*", sim]];

ahCode[i_Integer] :=
 If[1 <= i <= 3, {"A", "B", "C"}[[i]], 
  Error["Invalid AH: " <> ToString[i]]]

ReadSpECHorizonCentroidFromFile[file_String, ah_Integer] :=
 Module[{data},
  If[! FileExistsQ[file], Error["File " <> file <> " does not exist"]];
  data = Transpose@
    Import[file, {"Datasets", 
      "Ah" <> ahCode[ah] <> ".dir/CoordCenterInertial.dat"}];
  Table[ToDataTable[data[[1]], data[[i]]], {i, 2, 4}]];

ReadSXSHorizonDisplacement[run_String] :=
 Module[{file = FileNameJoin[{run, "Horizons.h5"}]},
  ReadSpECHorizonCentroidFromFile[file, 1] - 
   ReadSpECHorizonCentroidFromFile[file, 2]];

toAngularVelocity[sep_List] :=
 Cross[sep, NDerivative[1] /@ sep]/Norm[sep]^2;

ReadSXSOrbitalOmega[sim_String] :=
 toAngularVelocity[ReadSXSHorizonDisplacement[sim]];

accumulatedPhase[om_] :=
 Module[{omNormFn, phi, phiFn},
  omNormFn = Interpolation[Norm[om]];
  phiFn = 
   phi /. NDSolve[{phi'[t] == omNormFn[t], phi[0] == 0}, 
      phi, {t, MinCoordinate[om[[1]]], MaxCoordinate[om[[1]]]}][[1]];
  ToDataTable[ToListOfCoordinates[om[[1]]], 
   phiFn /@ ToListOfCoordinates[om[[1]]]]];

ReadSXSAccumulatedPhase[sim_String] :=
 accumulatedPhase[ReadSXSOrbitalOmega[sim]];

(****************************************************************)
(* SXS metadata format *)
(****************************************************************)

ReadSXSMetadataFile[name_String] :=
 Module[{keyValues, md1, md},
  keyValues = 
   Cases[ParseMetadataFile[name], 
    "element"["key"[k_], v_] :> (k -> v), Infinity];
  md1 = Association[
    Map[#[[1]] -> processValue[#[[2]]] &, keyValues]];
  md = computeExtraMetadata[md1]];

processValue["string"[s_]] := s;

processValue["value"[v_]] := processValue[v];

processValue[
  es : "email_list"["email"[_] ...]] :=
 (List @@ Map[First, es]);

processValue[nel : "name_email_list"[__]] :=
 List @@ Map[{#[[1, 1]], #[[2, 1]]} &, nel];

processValue["number"[x_]] := ImportString[x, "List"][[1]];

processValue[x_] := Error["Don't know how to process " <> ToString[x]]

processValue["keyword"[x_]] := x

nv[s_] :=
 ImportString[StringReplace[s, "," -> " "], "Table"][[1]];

computeExtraMetadata[a_] :=
 Module[{a1, a2},
  a1 = Join[a,
    <|"relaxed-mass-ratio" -> a["relaxed-mass1"]/a["relaxed-mass2"],
     "relaxed-chi1" -> nv[a["relaxed-spin1"]]/a["relaxed-mass1"]^2,
     "relaxed-chi2" -> 
      nv[a["relaxed-spin2"]]/a["relaxed-mass2"]^2|>];
  a2 = Join[a1,
    <|"in-plane-chi1" -> 
      Norm[Cross[Normal[a1["relaxed-chi1"]], 
        Normal[Normalize@nv[a1["relaxed-orbital-frequency"]]]]],
     "in-plane-chi2" -> 
      Norm[
       Cross[Normal[a1["relaxed-chi2"]], 
        Normal[Normalize@nv[a1["relaxed-orbital-frequency"]]]]]|>]];




(****************************************************************)
(* Waveform rotation (provided by Andrea Taracchini)            *)
(****************************************************************)

SmalldWignerArun[\[ScriptL]_, mp_, m_, \[Beta]_] := 
  Module[{k}, 
   If[IntegerQ[\[ScriptL]] && IntegerQ[m] && IntegerQ[mp] && \[ScriptL] >= 0 &&
      m >= -\[ScriptL] && m <= \[ScriptL] && mp >= -\[ScriptL] && 
     mp <= \[ScriptL],
    
    \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(k = 
       Max[{0, m - mp}]\), \(Min[{\[ScriptL] + m, \[ScriptL] - mp}]\)]\(
\*FractionBox[
SuperscriptBox[\((\(-1\))\), \(k\)], \(k!\)] 
\*FractionBox[
SqrtBox[\(\(\((\[ScriptL] + m)\)!\) \(\((\[ScriptL] - 
            m)\)!\) \(\((\[ScriptL] + mp)\)!\) \(\((\[ScriptL] - 
            mp)\)!\)\)], \(\(\((k - m + mp)\)!\) \(\((\[ScriptL] + m - 
           k)\)!\) \(\((\[ScriptL] - mp - k)\)!\)\)] 
\*SuperscriptBox[\(Cos[
\*FractionBox[\(\[Beta]\), \(2\)]]\), \(2  \[ScriptL] + m - mp - 2  k\)] 
\*SuperscriptBox[\(Sin[
\*FractionBox[\(\[Beta]\), \(2\)]]\), \(2  k - m + mp\)]\)\), 
    Print["Wrong indices"]]];
SmalldWignerArun[\[ScriptL]_, mp_, m_, 0] := 
  Limit[SmalldWignerArun[\[ScriptL], mp, m, x], x -> 0];
SmalldWignerArun[\[ScriptL]_, mp_, m_, \[Pi]] := 
  Limit[SmalldWignerArun[\[ScriptL], mp, m, x], x -> \[Pi]];

LandauDWigner[\[ScriptL]_, mp_, m_, \[Alpha]_, \[Beta]_, \[Gamma]_] := 
  If[IntegerQ[\[ScriptL]] && IntegerQ[m] && IntegerQ[mp] && \[ScriptL] >= 0 &&
     m >= -\[ScriptL] && m <= \[ScriptL] && mp >= -\[ScriptL] && 
    mp <= \[ScriptL], 
   Exp[I mp \[Gamma]] SmalldWignerArun[\[ScriptL], mp, m, \[Beta]] Exp[
     I m \[Alpha]], Print["Wrong indices"]];

(* Routine to find Euler angles to go from initial direction to final direction *)

EulerAngles[frameA_, frameB_] :=
 Module[{\[Alpha], \[Beta], \[Gamma], X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3, 
   normframeA, normframeB},
  normframeA = 
   Transpose[#/Sqrt[#[[1]]^2 + #[[2]]^2 + #[[3]]^2] & /@ Transpose[frameA]];
  normframeB = 
   Transpose[#/Sqrt[#[[1]]^2 + #[[2]]^2 + #[[3]]^2] & /@ Transpose[frameB]];
  {{X1, X2, X3}, {Y1, Y2, Y3}, {Z1, Z2, Z3}} = 
   Transpose[normframeB].normframeA // Chop;
  If[Z1 == 0 && Z2 == 0,
   If[Z3 > 0,
    {\[Alpha], \[Beta], \[Gamma]} = {0, 0, ArcTan[X1, X2]},
    {\[Alpha], \[Beta], \[Gamma]} = {0, \[Pi], ArcTan[-X1, X2]}],
   {\[Alpha], \[Beta], \[Gamma]} = {ArcTan[Z1, Z2], ArcCos[Z3], 
     ArcTan[-X3, Y3]}
   ];
  (*Print[{\[Alpha],\[Beta],\[Gamma],X1,X2,X3,Y1,Y2,Y3,Z1,Z2,
  Z3}];*)
  {\[Alpha], \[Beta], \[Gamma]}
  ]

(* Subscript[h, lm] transformation (using Landau's def of the D-matrix) *)
hprime[h_, {\[ScriptL]_, 
    mp_}, {\[CapitalAlpha]_, \[CapitalBeta]_, \[CapitalGamma]_}] := Module[{m},
   Sum[Conjugate[
      LandauDWigner[\[ScriptL], m, 
       mp, \[CapitalAlpha], \[CapitalBeta], \[CapitalGamma]]] h[\[ScriptL], 
      m], {m, -\[ScriptL], \[ScriptL]}]];

RotateWaveform[h_, {l_, m_}, {alpha_, beta_, gamma_}] :=
  hprime[h,{l,m},{alpha,beta,gamma}];

(****************************************************************)
(* Performance prediction                                       *)
(****************************************************************)

SpECEstimatedInspiralSpeed[sim_String] :=
 Module[{speed, tMax, speedChunk},
  speed = ReadSpECSimulationSpeed[sim];
  tMax = MaxCoordinate[speed];
  speedChunk = Slab[speed, 200 ;; Min[tMax, 1000]];
  (*Print[speedChunk];*)
  If[Length[speedChunk] === 0,
   (* TODO: estimate as 4/5 of speed of previous level, if it exists *)
      None,
   Mean[speedChunk]]];

SpECEstimatedMergerTime[sim_String] :=
 Module[{simBase, simLev, mass3, levs, lowerLevs},
  (*Print["Estimating from "<>sim];*)
  mass3 = ReadSpECHorizonMass[sim, 3];
  If[Length[mass3] > 0,
    (* Print["Have AhC"]; *)
   MinCoordinate[mass3],
   (* else *)
  (* Print["Looking for lower levs"]; *)
   {simBase, simLev} = StringSplit[sim, ":"];
   simLev = ToExpression[simLev];
   levs = ToExpression /@ FindSpECLevels[simBase];
   lowerLevs = Select[levs, # < simLev &];
   If[Length[lowerLevs] === 0,
    (* TODO: run PN code to estimate merger time *)
    None,
    SpECEstimatedMergerTime[simBase <> ":" <> ToString[Last[lowerLevs]]]]]];

SpECEstimatedRemainingWalltime[sim_String] :=
 Module[{term, ringdownTime = 800, speedData, tc, speed1, t, speed2},
  (* TODO: check to see if simulation has finished, 
  in which case return 0 *)
  term = ReadSpECSimulationTerminationReason[sim];
  If[term === "FinalTime", Return[0]];
  speedData = ReadSpECSimulationSpeed[sim];
  tc = SpECEstimatedMergerTime[sim];
(* Print["tc = ", tc]; *)
  If[tc === None, Return[None]];
  speed1 = SpECEstimatedInspiralSpeed[sim];
  (* Print["speed1 = ",speed1]; *)
  If[speed1 === None, Return[None]];
  speed2 = speed1/2;
  t = MaxCoordinate[speedData];
  (* Print["t = ",t]; *)
  If[t < tc,
    (* Print["Before tc"]; *)
   (tc - t)/speed1 + ringdownTime/speed2,
    (* Print["After tc"]; *)
   (tc + ringdownTime - t)/speed2]];

ReadSpECSimulationTerminationReason[sim_String] :=
 Module[{termFiles, termFile, termString, matches},
  termFiles = 
   Flatten[FindSpECSimulationFiles[sim, "TerminationReason.txt"]];
  If[termFiles === {}, "None",
   termFile = Last[termFiles];
   termString = StringSplit[Import[termFile, "Text"], "\n"][[1]];
   matches = 
    StringCases[termString, 
     StartOfString ~~ "Termination condition " ~~ cond__ ~~ 
       EndOfString :> cond];
   If[Length[matches] === 1,
    matches[[1]],
    Print[
     "Could not parse termination condition from \"" <> termString <> 
      "\" in " <> sim];
    "Unknown"]]]

ReadSpECFractionComplete[sim_String] :=
 Module[{walltime, completeHours, remainingHours},
  completeHours = Last[ReadSpECWallTime[sim]];
  remainingHours = SpECEstimatedRemainingWalltime[sim];
  If[remainingHours === None,
   None,
   completeHours/(completeHours + remainingHours)]];

PlotSpECSimulationsRemainingTime[sims_List] :=
  Module[{remainingHours, fractionComplete, simNames, completeLabels,
    unfinished, chartRemainingDays, maxDays},

    remainingHours = Map[(Module[{T = SpECEstimatedRemainingWalltime[#]},
      (*Print[#, ": ", 
        If[T =!= None, ToString[T/24] <> " days", "unknown"]];*) T]) &, 
      sims];

    fractionComplete = Map[(Module[{f = ReadSpECFractionComplete[#]},
      (*Print[#, ": ", 
        If[f =!= None, ToString[Round[100 f]] <> "%", "unknown"]];*) 
      f]) &, sims];

    simNames = 
    StringReplace[#, $SpECSimulationsDirectory <> "/" -> ""] & /@ 
    sims;

    unfinished = Map[# =!= 0 &, remainingHours];

    completeLabels = 
    Pick[fractionComplete, unfinished] /. {None :> "", 
      x_?NumberQ :> ToString[Round[100 x]] <> "%"};

    chartRemainingDays = Pick[remainingHours, unfinished]/24;

    maxDays = Max[Select[chartRemainingDays, FreeQ[#, None] &]];

    Show[BarChart[Reverse[chartRemainingDays], BarOrigin -> Left, 
      ChartLabels -> Reverse@Pick[simNames, unfinished], 
      ImageSize -> {400, Length[simNames]*5}, AspectRatio -> Full, 
      Frame -> True, FrameLabel -> {None, "Time remaining (days)"}, 
      GridLines -> Automatic, ChartStyle -> LightBlue, 
      PlotRange -> {{0, maxDays + 2}, All}(*, PlotLabel->"Walltime remaining"*)],
      Graphics[
        MapThread[
          Text[#1, {maxDays + 1, #2}, {-1, 0}] &, {Reverse@completeLabels, 
            Range[1, Length[completeLabels]]}]]]];

End[];
EndPackage[];
