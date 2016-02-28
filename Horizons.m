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

BeginPackage["SimulationTools`Horizons`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Memo`",
  "SimulationTools`RunFiles`"
 }];

ReadIHSpin;
ReadIHSpinX;
ReadIHSpinY;
ReadIHSpinPhase;
ReadIsolatedHorizonSpin(*::usage = "ReadIsolatedHorizonSpin[sim, idx, dir] returns the spin of horizon with index idx in sim in the direction dir (counted from 1) as a DataTable.  This requires output from the QuasiLocalMeasures or IsolatedHorizon thorn to be present in the sim directory.  dir is optional, and if omitted the result will be a vector-valued DataTable."*);
ReadIsolatedHorizonDimensionlessSpin;
ReadIsolatedHorizonSpinPhase;
ReadAHMass(*::usage = "ReadAHMass[sim, idx] returns the irreducible mass of apparent horizon with index idx in sim.  This requires output from the AHFinderDirect thorn."*);
ReadAHRadius(*::usage = "ReadAHRadius[sim, idx] returns the coordinate radius of apparent horizon with index idx in sim.  This requires output from the AHFinderDirect thorn."*);
ReadAHMinRadius;
ReadAHMaxRadius;
ReadAHCentroid(*::usage = "ReadAHCentroid[sim, idx] returns the coordinate centroid of apparent horizon with index idx in sim.  This requires output from the AHFinderDirect thorn."*);
ReadAHCentre;
ReadAHCentroidCoord;
ReadAHColumn;
ReadAHColumns;
ReadAHQuadrupoleXX;
ReadAHQuadrupoleXY;
ReadAHQuadrupoleXZ;
ReadAHQuadrupoleYY;
ReadAHQuadrupoleYZ;
ReadAHQuadrupoleZZ;
ChristodoulouMass;
ReadAHSeparation(*::usage = "ReadAHSeparation[sim] returns the coordinate distance between the centroids of apparent horizons with indices 1 and 2 in sim.  This requires output from the AHFinderDirect thorn."*);
ReadAHPhase;
ReadApparentHorizonPhase;
InitialSpin;
SpinAngle;
InitialSpinAngle;
HaveHorizonData;
HaveIsolatedHorizonSpinData;
HaveChristodoulouMassData;

Begin["`Private`"];

SimulationTools`Horizons`Trackers`ReadCoordinates[runName_String, trackers_List] :=
  Table[SimulationTools`Horizons`Trackers`ReadCoordinates[runName, t], {t, trackers}];

SimulationTools`Horizons`Trackers`ReadCoordinates[runName_, hn_] :=
 Module[{data},
  data = ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2,3,4,5}];
  Table[AddAttributes[ToDataTable[data[[All,1]], data[[All,dir+1]]], {RunName -> runName}], {dir,3}]
];

ReadIHSpin[runName_, hn_] :=
 Module[{data},
  data = ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"];
  ToDataTable[data[[All,1]], data[[All,{2,3,4}]]]
];

ReadIHSpin[runName_, hn_, dir_] :=
 Module[{data},
  data = ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"];
  ToDataTable[data[[All,1]], data[[All,dir+1]]]
];

ReadIHSpinX[runName_, hn_] :=
 Module[{data},
  data = ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"];
  ToDataTable[data[[All,1]], data[[All,2]]]
];

ReadIHSpinY[runName_, hn_] :=
 Module[{data},
  data = ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"];
  ToDataTable[data[[All,1]], data[[All,3]]]
];

ReadIHSpinPhase[runName_, hn_] :=
 Module[{spin, sx, sy},
  spin = ToList[ReadIHSpin[runName, hn]];
  sx = ToDataTable[spin[[All,1]], spin[[All,2,1]]];
  sx = ToDataTable[spin[[All,1]], spin[[All,2,2]]];
  Phase[sx + I sy]];

coordString[dir_] := {"x", "y", "z"}[[dir]];

ReadIsolatedHorizonSpin[runName_, hn_, dir_] :=
  Module[{},
    Which[
    FindSimulationFiles[runName,"isolatedhorizon"~~("-"|"::")~~"ih_scalars..asc"] =!= {},
       MakeDataTable@ReadColumnFile[runName, "isolatedhorizon"~~("-"|"::")~~"ih_scalars..asc",
         {"time", "ih_coordspin"<>coordString[dir]<>"["<>ToString[hn]<>"]"}],
    FindSimulationFiles[runName,"quasilocalmeasures"~~("-"|"::")~~"qlm_scalars..asc"] =!= {},
       MakeDataTable@ReadColumnFile[runName, "quasilocalmeasures"~~("-"|"::")~~"qlm_scalars..asc",
         {"time", "qlm_coordspin"<>coordString[dir]<>"["<>ToString[hn]<>"]"}],
    FindSimulationFiles[runName,"ih_coordspin"<>coordString[dir]<>"["<>ToString[hn]<>"]..asc"] =!= {},
       MakeDataTable@ReadColumnFile[runName, 
         "ih_coordspin"<>coordString[dir]<>"["<>ToString[hn]<>"]..asc", {9, 13}],
    True,
      Error["ReadIsolatedHorizonSpin: Cannot find isolated horizon data in: "<> runName]]];

ReadIsolatedHorizonSpin[runName_, hn_] :=
  Module[{spins},
    spins = Table[ReadIsolatedHorizonSpin[runName, hn, dir], {dir, 1, 3}];
    MapThreadData[{#1,#2,#3} &, spins]];

ReadIsolatedHorizonDimensionlessSpin[runName_, hn_] :=
  Apply[Divide, SimulationTools`DataRegion`Private`resampled[
    {ReadIsolatedHorizonSpin[runName, hn], ChristodoulouMass[runName, hn+1, hn]^2}]];

ReadIsolatedHorizonSpinPhase[runName_, hn_] :=
  MakeDataTable@Phase@ToList@MapData[Drop[#,-1]&, ReadIsolatedHorizonSpin[runName, hn]];

ReadAHMass[runName_, hn_] :=
 Module[{},
  MakeDataTable[
   ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2, 27}]]];

DefineMemoFunction[ReadAHRadius[runName_, hn_],
 Module[{},
  MakeDataTable[
   ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2, 8}]]]];

DefineMemoFunction[ReadAHMaxRadius[runName_, hn_],
 Module[{},
  MakeDataTable[
   ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2, 7}]]]];

DefineMemoFunction[ReadAHMinRadius[runName_, hn_],
 Module[{},
  MakeDataTable[
   ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2, 6}]]]];

DefineMemoFunction[ReadAHColumn[runName_, hn_, col_],
 Module[{},
  MakeDataTable[
   ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2, col}]]]];

(* TODO: this assumes the length of cols is 3 *)
DefineMemoFunction[ReadAHColumns[runName_, hn_, cols_List],
 Module[{data},
  data = ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", Prepend[cols,2]];
  AddAttributes[ToDataTable[data[[All, 1]], data[[All, {2,3,4}]]], {RunName -> runName}]
]];

ReadAHQuadrupoleXX[runName_, hn_] :=
  ReadAHColumn[runName, hn, 9];

ReadAHQuadrupoleXY[runName_, hn_] :=
  ReadAHColumn[runName, hn, 10];

ReadAHQuadrupoleXZ[runName_, hn_] :=
  ReadAHColumn[runName, hn, 11];

ReadAHQuadrupoleYY[runName_, hn_] :=
  ReadAHColumn[runName, hn, 12];

ReadAHQuadrupoleYZ[runName_, hn_] :=
  ReadAHColumn[runName, hn, 13];

ReadAHQuadrupoleZZ[runName_, hn_] :=
  ReadAHColumn[runName, hn, 14];

DefineMemoFunction[ReadAHCentroid[runName_, hn_],
 Module[{data},
  data = ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2,3,4,5}];
  AddAttributes[ToDataTable[data[[All, 1]], data[[All, {2,3,4}]]], {RunName -> runName}]
]];

ReadAHSeparation[runName_String] :=
  Module[{x0, x1, rad, l},
    x0 = ReadAHCentroid[runName, 1];
    x1 = ReadAHCentroid[runName, 2];
    l = Min[Length[x0],Length[x1]];
    rad = MapThreadData[Norm[#1-#2] &, {Take[x0,l], Take[x1,l]}];
    Return[rad];
  ];

DefineMemoFunction[ReadAHCentroidCoord[runName_, hn_, dir_],
 Module[{list},
   list = ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2,2+dir}];
   Return[MakeDataTable[list, {RunName -> runName}]]]];

DefineMemoFunction[ReadAHCentre[runName_, hn_],
 Module[{list, t, min, max, cen},
   list = ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp",
     {2,Sequence@@Range[15,20]}];
   t = list[[All,1]];
   min = list[[All,{2,4,6}]];
   max = list[[All,{3,5,7}]];
   cen = 1/2(min+max);
   Map[ToDataTable[t, #] &, Transpose[cen]]]];

HaveChristodoulouMassData[run_, ahn_, ihn_] :=
  HaveHorizonData[run,ahn] && HaveIsolatedHorizonSpinData[run,ihn];

ChristodoulouMass[run_, ahn_, ihn_] :=
 Module[{mIrr, S},
  mIrr = ReadAHMass[run, ahn];
  S = MapData[Norm, ReadIsolatedHorizonSpin[run, ihn]];
  (* TODO: We don't need to resample here; it would be sufficient to
     restrict mIrr to the grid of S, as mIrr must exist everywhere S
     does *)
  {mIrr, S} = ResampleDataTables[{mIrr, S}];
  Sqrt[mIrr^2 + S^2/(4 mIrr^2)]];

ReadAHPhase[runName_String] :=
  Module[{x0, x1, xyTrans, l},
    x0 = ReadAHCentroid[runName, 1];
    x1 = ReadAHCentroid[runName, 2];
    l = Min[Length[x0],Length[x1]];
    xyTrans = MapThreadData[Take[#1-#2,2] &, {Take[x0,l], Take[x1,l]}]; (* Project into xy plane *)
    Return[Phase[xyTrans]];
  ];

ReadApparentHorizonPhase[sim_String, ah_Integer] :=
 Module[{data, x, y},
  data = ReadColumnFile[sim,
    "BH_diagnostics.ah" <> ToString[ah] <> ".gp", {2, 3, 4}];
  x = ToDataTable[data[[All, {1, 2}]]];
  y = ToDataTable[data[[All, {1, 3}]]];
  SimulationTools`Trackers`Private`xyToAzimuth[{x, y}]];

(* TODO: This doesn't look right *)
InitialSpin[run_, i_] :=
 First@DepVar@SpinAngle[run, i];

SpinAngle[run_, idx_] :=
  MapData[AnglesOfVector[#][[2]] &, ReadIHSpin[run, idx]];

InitialSpinAngle[run_, i_] :=
 First@DepVar@SpinAngle[run, i];

InitialSpinAngle[run_] :=
 Module[{S0, S1},
  S0 = First@DepVar@ReadIHSpin[run, 0];
  S1 = First@DepVar@ReadIHSpin[run, 1];
  If[Norm@S0 > Norm@S1,
   AnglesOfVector[S0][[2]],
   AnglesOfVector[S1][[2]]]];

InitialSpin[run_] :=
 Module[{S0, S1},
  S0 = First@DepVar@ReadIHSpin[run, 0];
  S1 = First@DepVar@ReadIHSpin[run, 1];
  If[Norm@S0 > Norm@S1,
   S0,
   S1]];

HaveHorizonData[run_, i_] :=
  FileIsInRun[run, "BH_diagnostics.ah"<>ToString[i]<>".gp"];

HaveIsolatedHorizonSpinData[run_, i_] :=
  FileIsInRun[run, "quasilocalmeasures"~~("-"|"::")~~"qlm_scalars..asc"] || FileIsInRun[run, "isolatedhorizon"~~("-"|"::")~~"ih_scalars..asc"];

End[];

EndPackage[];
