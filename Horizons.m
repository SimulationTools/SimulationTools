
BeginPackage["Horizons`", {"RunFiles`", "DataTable`", "Memo`"}];

ReadIHSpin;
ReadIHSpinX;
ReadIHSpinY;
ReadIHSpinPhase;
ReadIsolatedHorizonSpin;
ReadAHMass;
ReadAHRadius;
ReadAHMinRadius;
ReadAHMaxRadius;
ReadAHCentroid;
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
ReadAHSeparation;

Begin["`Private`"];

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
    If[FindRunFile[runName,"isolatedhorizon::ih_scalars..asc"] =!= {},
       MakeDataTable@ReadColumnFile[runName, "isolatedhorizon::ih_scalars..asc", 
         {"time", "ih_coordspin"<>coordString[dir]<>"["<>ToString[hn]<>"]"}],
       MakeDataTable@ReadColumnFile[runName, 
         "ih_coordspin"<>coordString[dir]<>"["<>ToString[hn]<>"]..asc", {9, 13}]]];

ReadIsolatedHorizonSpin[runName_, hn_] :=
  Module[{spins},
    spins = Table[ReadIsolatedHorizonSpin[runName, hn, dir], {dir, 1, 3}];
    MapThreadData[{#1,#2,#3} &, spins]];

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

DefineMemoFunction[ReadAHColumns[runName_, hn_, cols_List],
 Module[{},
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
 Module[{},
   list = ReadColumnFile[runName, "BH_diagnostics.ah"<>ToString[hn]<>".gp", {2,2+dir}];
   Return[MakeDataTable[list, {RunName -> runName}]]]];

ChristodoulouMass[run_, ahn_, ihn_] :=
 Module[{mIrr, S},
  mIrr = ReadAHMass[run, ahn];
  S = MapData[Norm, ReadIsolatedHorizonSpin[run, ihn]];
  {mIrr, S} = ResampleDataTables[{mIrr, S}];
  Sqrt[mIrr^2 + S^2/(4 mIrr^2)]];

End[];

EndPackage[];
