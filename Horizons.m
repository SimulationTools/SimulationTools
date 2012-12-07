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
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
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
InitialSpin;
SpinAngle;
InitialSpinAngle;
HaveHorizonData;
HaveIsolatedHorizonSpinData;
HaveChristodoulouMassData;

Begin["`Private`"];

SimulationTools`Horizons`Trackers`ReadCoordinates[runName_, i_] :=
  Table[ReadAHCentroidCoord[runName, i, dir], {dir, 1, 3}];

ReadIHSpin[runName_, hn_] :=
 MakeDataTable[Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}} &, 
  ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"]]];

ReadIHSpin[runName_, hn_, dir_] :=
 MakeDataTable[Map[{#[[1]], #[[dir+1]]} &, 
  ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"]]];

ReadIHSpinX[runName_, hn_] :=
 MakeDataTable[Map[{#[[1]], #[[2]]} &, 
  ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"]]];

ReadIHSpinY[runName_, hn_] :=
 MakeDataTable[Map[{#[[1]], #[[3]]} &, 
  ReadColumnFile[runName, "ihspin_hn_" <> ToString[hn] <> ".asc"]]];

ReadIHSpinPhase[runName_, hn_] :=
 Module[{spin, sx, sy},
  spin = ReadIHSpin[runName, hn];
  sx = MakeDataTable[Map[{#[[1]], #[[2]][[1]]} &, ToList[spin]]];
  sy = MakeDataTable[Map[{#[[1]], #[[2]][[2]]} &, ToList[spin]]];
  Phase[sx + I sy]];

coordString[dir_] := {"x", "y", "z"}[[dir]];

ReadIsolatedHorizonSpin[runName_, hn_, dir_] :=
  Module[{},
    Which[
    FindRunFile[runName,"isolatedhorizon::ih_scalars..asc"] =!= {},
       MakeDataTable@ReadColumnFile[runName, "isolatedhorizon::ih_scalars..asc", 
         {"time", "ih_coordspin"<>coordString[dir]<>"["<>ToString[hn]<>"]"}],
    FindRunFile[runName,"quasilocalmeasures::qlm_scalars..asc"] =!= {},
       MakeDataTable@ReadColumnFile[runName, "quasilocalmeasures::qlm_scalars..asc",
         {"time", "qlm_coordspin"<>coordString[dir]<>"["<>ToString[hn]<>"]"}],
    True,
       MakeDataTable@ReadColumnFile[runName, 
         "ih_coordspin"<>coordString[dir]<>"["<>ToString[hn]<>"]..asc", {9, 13}]]];

ReadIsolatedHorizonSpin[runName_, hn_] :=
  Module[{spins},
    spins = Table[ReadIsolatedHorizonSpin[runName, hn, dir], {dir, 1, 3}];
    MapThreadData[{#1,#2,#3} &, spins]];

ReadIsolatedHorizonDimensionlessSpin[runName_, hn_] :=
  ReadIsolatedHorizonSpin[runName, hn] ~Div~ ChristodoulouMass[runName, hn+1, hn]^2;

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
 Module[{list, list2},
   list = ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", Prepend[cols,2]];
   list2 = Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}} &, list];
   Return[MakeDataTable[list2, {RunName -> runName}]]]];

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
 Module[{list, list2},
   list = ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2,3,4,5}];
   list2 = Map[{#[[1]], {#[[2]], #[[3]], #[[4]]}} &, list];
   Return[MakeDataTable[list2, {RunName -> runName}]]]];

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

HaveChristodoulouMassData[run_, ahn_, ihn_] :=
  HaveHorizonData[run,ahn] && HaveIsolatedHorizonSpinData[run,ihn];

ChristodoulouMass[run_, ahn_, ihn_] :=
 Module[{mIrr, S},
  mIrr = ReadAHMass[run, ahn];
  S = MapData[Norm, ReadIsolatedHorizonSpin[run, ihn]];
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
  FileIsInRun[run, "quasilocalmeasures::qlm_scalars..asc"] || FileIsInRun[run, "isolatedhorizon::ih_scalars..asc"];

End[];

EndPackage[];
