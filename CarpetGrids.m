(* Copyright 2010-2015 Ian Hinder and Barry Wardell

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

BeginPackage["SimulationTools`CarpetGrids`",
 {
  "SimulationTools`Error`",
  "SimulationTools`Parameters`",
  "SimulationTools`RunFiles`",
  "SimulationTools`Segments`"
 }];

ReadCarpetGridStructure;
BBox;
SymmetricBBoxDifference;
CheckIdenticalGrids;
ReadCarpetGridBBoxes;
$UnequalBBoxes;
$GridTime;
$BBoxTolerance;

Begin["`Private`"];

bboxOfString[s_] :=
 Module[{x1, x2, dx},
  BBox @@ (parseCoordVec3 /@ 
     StringCases[s, compStringPatternRule][[1, 1 ;; 3]])];

compStringPatternRule = 
  "(" ~~ x1 : coordVec3 ~~ ":" ~~ x2 : coordVec3 ~~ ":" ~~ dx : coordVec3 ~~ 
    "/" ~~ i1 : coordVec3 ~~ ":" ~~ i2 : coordVec3 ~~ "/" ~~ i3 : coordVec3 ~~
     "/" ~~ n : NumberString ~~ ")" :> {x1, x2, dx, i1, i2, i3, n};

compStringPattern = 
  "(" ~~ coordVec3 ~~ ":" ~~ coordVec3 ~~ ":" ~~ coordVec3 ~~ "/" ~~ 
   coordVec3 ~~ ":" ~~ coordVec3 ~~ "/" ~~ coordVec3 ~~ "/" ~~ NumberString ~~
    ")";

parseCoordVec3[s_String] :=
 StringCases[s, coordVec3PatternRule][[1]]

coordVec3 = 
  "[" ~~ StringExpression @@ Riffle[ConstantArray[NumberString, 3], ","] ~~ 
   "]";

coordVec3PatternRule = 
  "[" ~~ x1 : NumberString ~~ "," ~~ x2 : NumberString ~~ "," ~~ 
    x3 : NumberString ~~ "]" :> ToExpression /@ {x1, x2, x3};

parseIterationTable[itTable_] :=
 Module[{itData, levels},
  itData = Select[itTable, Length[#] === 5 && NumberQ[#[[4]]] &];
  levels = SplitBy[itData, #[[3]] &];
  {itTable[[1, 2]], parseLevelTable /@ levels}];

parseLevelTable[levTable_] :=
 Module[{},
  {levTable[[1, 3]], bboxOfString /@ levTable[[All, 5]]}];

parseGridTable[table_] :=
 Module[{iterationPositions, itTable},
  iterationPositions = 
   Append[Position[table, "iteration"][[All, 1]], Length[table]];
  itTable = 
   Table[table[[
     iterationPositions[[i]] ;; iterationPositions[[i + 1]] - 1]], {i, 1, 
     Length[iterationPositions] - 1}];
  Reverse[DeleteDuplicates[
    Reverse[parseIterationTable /@ itTable], #1[[1]] == #2[[1]] &]]];

ReadCarpetGridStructure = readGridStructure;

readGridStructure[sim_String, fileName_: "carpet-grid.asc"] :=
 Module[{files, table},
  files = FindSimulationFiles[sim, fileName];
  If[Length[files] > 1, 
   Print["WARNING: readGridStructure only reads the first segment of multi-segment simulations"]];
  table = Import[files[[1]], "Table"];
  parseGridTable[table]];

readGhostSize[sim_String] :=
 ToExpression[ReadSimulationParameter[sim, "Driver::ghost_size"]];

readTaperFactor[sim_String] :=
 If[StringMatchQ[
   ReadSimulationParameter[sim, "Carpet::use_tapered_grids", "no"], "yes"], 2,
   1];

readTimeIntegratorSubsteps[sim_String] :=
 ToExpression[ReadSimulationParameter[sim, "MoL::MoL_Intermediate_Steps"]];

readBufferSize[sim_String] :=
 readGhostSize[sim] (readTaperFactor[sim] readTimeIntegratorSubsteps[sim] - 1);

enlargeBBox[BBox[x1_, x2_, dx_], n_] :=
 BBox[x1 - n dx, x2 + n dx, dx];

enclosingBBox[bs_List] :=
 Module[{},
  BBox[Table[Min[bs[[All, 1, i]]], {i, 1, Length[bs[[1, 1]]]}], 
   Table[Max[bs[[All, 2, i]]], {i, 1, Length[bs[[1, 1]]]}], bs[[1, 3]]]];

(******************************)
(* Set operations on bboxsets *)
(******************************)

$BBoxTolerance = 10.^-6;

(* Intersection *)

intersection[b1_BBox, b2_BBox] :=
 Module[{min, max},
  min = MapThread[Max, {b1[[1]], b2[[1]]}];
  max = MapThread[Min, {b1[[2]], b2[[2]]}];
  BBox[min, max, b1[[3]]]];

intersection[b_BBox, bs : {_BBox ...}] :=
 normalise@Map[intersection[b, #] &, bs];

intersection[bs1 : {_BBox ...}, bs2 : {_BBox ...}] :=
 normalise@Flatten[Map[intersection[#, bs2] &, bs1], 1];

intersection[bboxess_List] :=
 normalise@Fold[intersection, First[bboxess], Rest[bboxess]];

(* Complement *)

checkBBoxSet[bs : {_BBox ...}] := bs;

checkBBoxSet[x_] := Error["Not a BBoxSet: " <> ToString[x]];

complement[b2_BBox, b1_BBox] :=
 checkBBoxSet@normalise[(checkNonZero[b1];
    If[Length[b1[[1]]] === 1,
     {BBox[b2[[1]], b1[[1]] - b2[[3]], b2[[3]]], 
      BBox[b1[[2]] + b2[[3]], b2[[2]], b2[[3]]]},
     {BBox[b2[[1]], {b1[[1, 1]] - b2[[3, 1]], Sequence @@ b2[[2, 2 ;; All]]}, 
       b2[[3]]], 
      Sequence @@ 
       Map[raiseDim[#, {b1[[1, 1]], b2[[2, 1]], b2[[3, 1]]}] &, 
        complement[dropDim[b2], dropDim[b1]]], 
      BBox[{b1[[2, 1]] + b1[[3, 1]], Sequence @@ b2[[1, 2 ;; All]]}, b2[[2]], 
       b2[[3]]]}])];

complement[b_BBox, bs_List] :=
 checkBBoxSet@intersection[Map[complement[b, #] &, bs]];

dropDim[b_BBox] :=
 (checkNonZero[b];
  Drop[#, 1] & /@ b);

raiseDim[b_BBox, {min_, max_, dx_}] :=
 BBox[Prepend[b[[1]], min], Prepend[b[[2]], max], Prepend[b[[3]], dx]];

checkNonZero[b_BBox] :=
 If[Length[b[[1]]] === 0, Error["BBox is zero-dimensional"], b];

(* Comparison *)

bboxesEqual[bs1_List, 
  bs2_List] :=
 (Length[symmetricBBoxDifference[bs1, bs2]] === 0);

SymmetricBBoxDifference = symmetricBBoxDifference;

symmetricBBoxDifference[bs1_List, bs2_List] :=
 Module[{enc1, enc2},
  enc1 = enlargeBBox[enclosingBBox[bs1], 1];
  enc2 = enlargeBBox[enclosingBBox[bs2], 1];
  Join[normalise@intersection[complement[enc1, bs1], bs2], 
   normalise@intersection[complement[enc2, bs2], bs1]]];

(* Clipping *)

clipZ[{x_, y_, z_}] :=
 {x, y, If[z >= 0., z, 0.]};

clipZ[BBox[x1_, x2_, dx_]] :=
 BBox[clipZ[x1], clipZ[x2], dx];

clipX[{x_, y_, z_}] :=
 {If[x >= 0., x, 0.], y, z};

clipX[BBox[x1_, x2_, dx_]] :=
 BBox[clipX[x1], clipX[x2], dx];

clipY[{x_, y_, z_}] :=
 {x, If[y >= 0., y, 0.], z};

clipY[BBox[x1_, x2_, dx_]] :=
 BBox[clipY[x1], clipY[x2], dx];

dropLastBBoxDim[BBox[x1_, x2_, dx_]] :=
 BBox[Drop[x1, -1], Drop[x2, -1], Drop[dx, -1]];

(* Shrinking *)

shrinkBBoxes[bboxes_, n_, clipz_: False, clipx_: False, clipy_: False] :=
 Module[{enc},
  enc = enlargeBBox[enclosingBBox[bboxes], 2 n];
  If[clipz, enc = clipZ[enc]];
  If[clipy, enc = clipY[enc]];
  If[clipx, enc = clipX[enc]];
  complement[enc, 
   normalise@Map[enlargeBBox[#, n] &, complement[enc, bboxes]]]];

rb12[bs_] := shrinkBBoxes[bs, 12, True];

rb20[bs_] := shrinkBBoxes[bs, 20, True];

(* Normalisation *)

(*isEmptyQ[b_BBox]:=
Or@@(#<10^-7&/@(b[[2]]-b[[1]]))*)

isEmptyQ[b_BBox] :=
 Or @@ MapThread[#2 - #1 < -#3 + $BBoxTolerance &, List @@ b];

removeEmpty[bs : {_BBox ...}] :=
 Select[bs, (! isEmptyQ[#]) &];

normalise[bs : {_BBox ...}] :=
 removeEmpty[Union[bs]];

(* Plotting *)

plotBBox3D[bbox_BBox] :=
 Cuboid[bbox[[1]], bbox[[2]]];

plotBBox[b_BBox] :=
 Table[Point[{x, y}], {x, b[[1, 1]], b[[2, 1]], b[[3, 1]]}, {y, b[[1, 2]], 
   b[[2, 2]], b[[3, 2]]}];

plotBBox[b_BBox] :=
 {Table[
   Line[{{b[[1, 1]], y}, {b[[2, 1]], y}}], {y, b[[1, 2]], 
    b[[2, 2]] + $BBoxTolerance, b[[3, 2]]}],
  Table[Line[{{x, b[[1, 2]]}, {x, b[[2, 2]]}}], {x, b[[1, 1]], 
    b[[2, 1]] + $BBoxTolerance, b[[3, 1]]}]};

plotBBoxPoints[b_BBox] :=
 Table[Point[{x, y}], {x, b[[1, 1]], b[[2, 1]], b[[3, 1]]}, {y, b[[1, 2]], 
   b[[2, 2]], b[[3, 2]]}];

plotBBoxes[bs : {_BBox ...}, opts___] :=
 Graphics[{Opacity[0.1], plotBBox /@ bs}, opts, AspectRatio -> Automatic, 
  Axes -> True, ImageSize -> 400];

toXYPlane[bs : {_BBox ...}] :=
 dropLastBBoxDim /@ intersection[
   BBox[{-100000., -100000., 0.}, {100000., 100000., 0.}, bs[[1, 3]]],
   bs];

toXYPlaneInt[bs : {_BBox ...}, iOriginZ_Integer] :=
 dropLastBBoxDim /@ intersection[
   BBox[{-100000, -100000, iOriginZ}, {100000, 100000, iOriginZ}, 
    bs[[1, 3]]],
   bs];

readIterationTimeStep[sim_String] :=
 ReadTimeStep[sim, 
  RefinementLevel -> ReadMaxRefinementLevels[sim] - 1];

(* Checking identical *)

CheckIdenticalGrids[sims : {_String ...}, 
  gridFileName_: "carpet-grid.asc"] :=

 (* TODO: add an option to control clipping of symmetry
    boundaries. Currently z-reflection is assumed *)
 
 Module[{grids, its, rls, dt, buffers = readBufferSize/@sims},
  grids = Map[readGridStructure[#, gridFileName] &, sims];
  (*Print[grids[[1,1]]];*)
  its = Min[Length /@ grids];
  rls = Length[grids[[1, 1, 2]]];
  dt = readIterationTimeStep[sims[[1]]];
  $UnequalBBoxes = {};
  Do[
   (* Print[{i, rl}]; *)
   Module[{gs = 
      MapThread[normalise[
         shrinkBBoxes[#1, #2, True, False, False (* FIXME: 
          assuming bitant *)]] &, {grids[[All, i, 2, rl, 2]],buffers}], equal},
    (* Print[gs]; *)
    (*Print[
    i];*)
    $GridTime = FullForm[(grids[[1, i, 1]] - (If[i == 1, 0, 1])) dt];
    equal = Map[bboxesEqual[First[gs], #] &, Rest[gs]];
    (*Print[equal];*)
    If[! And @@ equal,
     Print["Grid equality check failed at t = ", $GridTime, 
      " on refinement level ", rl - 1];
     AppendTo[$UnequalBBoxes, gs];
     Print[MatrixForm[Outer[bboxesEqual, gs, gs, 1]]];
     ]],
   {i, 1, its}, {rl, rls, 2, -1}];];

ReadCarpetGridBBoxes[sim_String, t_, rl_, removeBuffers_: True] :=
 Module[{grids, dt, it, indices, i, buffers = readBufferSize[sim], fullBBoxes},
  grids = readGridStructure[sim];
  dt = readIterationTimeStep[sim];
  it = Round[t/dt];
  indices = Position[grids[[All, 1]], If[it == 0, it, it + 1]];
  If[indices === {},
   Error["No grid information for t = " <> ToString[t] <> " in " <> sim]];
  i = indices[[1, 1]];
  fullBBoxes = grids[[i, 2, rl + 1, 2]];
  If[removeBuffers, normalise[shrinkBBoxes[fullBBoxes, buffers, True]],
   normalise[fullBBoxes]]];

End[];
EndPackage[];
