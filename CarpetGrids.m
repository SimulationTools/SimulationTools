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
  "SimulationTools`Plotting`",
  "SimulationTools`Segments`",
  "SimulationTools`Grids`",
  "SimulationTools`Utils`"
 }];

ReadCarpetGridStructure;
BBox;
SymmetricBBoxDifference;
CheckIdenticalGrids;
ReadCarpetGridBBoxes;
RegriddingIterations;
CheckIdenticalRegridding;
PlotCarpetBBoxes;
PlotCarpetBBoxes2D;
GridPlot2D;

$UnequalBBoxes;
$GridTime;
$BBoxTolerance;

Begin["`Private`"];

numberString = NumberString | (NumberString ~~ "e" ~~ NumberString);

coordVec3 = 
  "[" ~~ StringExpression @@ Riffle[ConstantArray[numberString, 3], ","] ~~ 
   "]";

compStringPatternRule = 
  "(" ~~ x1 : coordVec3 ~~ ":" ~~ x2 : coordVec3 ~~ ":" ~~ dx : coordVec3 ~~ 
    "/" ~~ i1 : coordVec3 ~~ ":" ~~ i2 : coordVec3 ~~ "/" ~~ i3 : coordVec3 ~~
     "/" ~~ n : numberString ~~ ")" :> {x1, x2, dx, i1, i2, i3, n};

compStringPattern = 
  "(" ~~ coordVec3 ~~ ":" ~~ coordVec3 ~~ ":" ~~ coordVec3 ~~ "/" ~~ 
   coordVec3 ~~ ":" ~~ coordVec3 ~~ "/" ~~ coordVec3 ~~ "/" ~~ numberString ~~
    ")";

stringToDouble[s_] :=
  (* (  Print["stringToDouble: ", s]; *)
  (*   Abort[]; *)
  (* Carpet writes one too many digit for this to work. You get $Failed(bignum). *)
    (* Internal`StringToDouble[s]; *)
  If[StringMatchQ[s,NumberString],
    ToExpression[s],
    (* This is much slower than StringToDouble *)
    ImportString[s,"List"][[1]]];


coordVec3PatternRule = 
  "[" ~~ x1 : numberString ~~ "," ~~ x2 : numberString ~~ "," ~~ 
    x3 : numberString ~~ "]" :> stringToDouble /@ {x1, x2, x3};

bboxOfString[s_] :=
 Module[{},
  BBox @@ (parseCoordVec3 /@ 
     StringCases[s, compStringPatternRule][[1, 1 ;; 3]])];

parseCoordVec3[s_String] :=
 StringCases[s, coordVec3PatternRule][[1]];

parseIterationTable[itTable_] :=
 Module[{itData, levels},
  (* Only do map 0 for now *)
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
    parseIterationTable /@ Select[itTable,#[[3,1]]===0&]];

ReadCarpetGridStructure = readGridStructure;

ReadCarpetGridStructure::usage = "ReadCarpetGridStructure[sim] returns the grid structure from simulation sim.  The format is {{it1, {{rl1, bboxes}, {rl2, bboxes}, ...}}, {it2, {{rl1, bboxes}, ...}}, ...}.  The grid structure is output only for iteration 0, recovery iterations, and iterations when it changed.  A general iteration i has the grid structure of the largest reported iteration less than or equal to i."

readGridStructure[sim_String, fileName_: "carpet-grid.asc"] :=
 Module[{files, tables, gss, gs(*, decIt, fixIterations*)},
  files = FindSimulationFiles[sim, fileName];
  tables = Import[#, "Table"] & /@ files;

  gss = parseGridTable/@tables;

  (* Carpet outputs the grid structure for level 0 and calls it
     iteration 0 at the start of each segment. Drop this output. TODO:
     check that it is iteration 0 with only one refinement level, in
     case this is fixed in Carpet in future. *)
  gss = Map[Drop[#,1]&, gss];

  (* Carpet regrids at the start of an iteration.  This means the
     regridding iterations are always regrid_every * i + 1.  The
     initial iteration, either from initial data or recovery, is also
     output in the grid structure file.  This was discussed in email
     thread "Re: Carpet::grid_coordinates_filename bug", private
     communication between Erik Schnetter, Seth Hopper and Ian Hinder,
     May 2014, and also
     https://trac.einsteintoolkit.org/ticket/1234. *)

  (* IH now believes that Carpet's output is correct, and decrementing
     the iteration numbers is inconsistent. *)

  (* decIt[singleIt_] := {singleIt[[1]]-1, singleIt[[2]]}; *)
  (* fixIterations[itGss_List] := *)
  (*   Join[{itGss[[1]]}, decIt/@Rest[itGss]]; *)

  (* gss = fixIterations/@gss; *)

  (* The grid structure returned here associated with iteration i is
     the grid structure which was set at the beginning of iteration i.
     If there is output of grid variables at iteration i, that output
     will have the grid structure returned here. Suppose we have grid
     structure output for iterations i1 and 12.  The i1 grid
     structure is valid for iterations i1 <= i < i2. *)

  gs = Flatten[gss,1];
  gs];

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

intersection[{}] := {};

intersection[{bboxes}] := bboxes;

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
  If[bs1 === {}, Return[bs2]];
  If[bs2 === {}, Return[bs1]];
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
 (* TODO: For clipping, these bboxes must currently be 3D *)
 Module[{enc},
   If[bboxes==={}, Return[{}]]; (* Not sure why this isn't handled correctly below *)
   If[n===0, Return[bboxes]];
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

plotBBoxes = PlotCarpetBBoxes;

Options[PlotCarpetBBoxes] = Join[Options[Graphics], {"PreDirectives" -> {}}];
PlotCarpetBBoxes[bs : {_BBox ...}, opts:OptionsPattern[]] :=
  Module[{pre=OptionValue[PreDirectives]},
 Graphics[{Opacity[0.1], pre , plotBBox /@ bs}, FilterRules[{opts}, Options[Graphics]],
   AspectRatio -> Automatic, 
   Axes -> True, ImageSize -> 400]];

Options[PlotCarpetBBoxes2D] = Options[PlotCarpetBBoxes];
PlotCarpetBBoxes2D[bs : {_BBox ...}, opts:OptionsPattern[]] :=
  PlotCarpetBBoxes[toXYPlane[bs], opts];

toXYPlane[bs : {_BBox ...}] :=
 If[bs === {}, {},
 dropLastBBoxDim /@ intersection[
   BBox[{-100000., -100000., 0.}, {100000., 100000., 0.}, bs[[1, 3]]],
   bs]];

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
 
 Module[{grids, its, rls, dt, buffers = readBufferSize/@sims, i, rl},
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

RegriddingIterations[gs_List, regridEvery_] :=
  Select[gs[[All,1]], Mod[#, regridEvery] === 1 &]-1;

readRegriddingIterationInterval[sim_String] :=
 ToExpression[ReadSimulationParameter[sim, "CarpetRegrid2::regrid_every"]];

CheckIdenticalRegridding[sims : {_String ...}, 
  gridFileName_: "carpet-grid.asc"] :=

 (* TODO: add an option to control clipping of symmetry
    boundaries. Currently z-reflection is assumed *)
 
 Module[{grids, dts, regEvs, regIts, regTimes1, regTimes, regAnyTimes,
   buffers = readBufferSize/@sims, maxRL, gridsAt, checkIdenticalGrids,
   checkIdenticalGridsAtTime, diffs, tRef},

  grids = Map[readGridStructure[#, gridFileName] &, sims];

  (* maxRL = Max[Flatten[grids[[All,All,2,All,1]]]]; *)

  maxRL = 9; (* TODO: fix this *)
Print["WARNING: maxRL = 9"];
  dts = readIterationTimeStep/@sims;
(* Print["dts = ", dts]; *)
  regEvs = readRegriddingIterationInterval/@sims;
(* Print["regEvs = ", regEvs]; *)
  (* Print[regEvs]; *)

  regIts = MapThread[RegriddingIterations, {grids, regEvs}];
(* Print["regIts = ", regIts]; *)
  (* All the times that each of the simulations regrid *)
  regTimes1 = regIts dts;
(* Print["regTimes1 = ", regTimes1]; *)
  (* Eliminate small variations *)

  tRef = regTimes1[[1,1]];
  If[Rationalize[tRef] === 0, Print["Adjusting"]; tRef = regTimes1[[1,2]]];
(* Print["tRef = ", tRef]; *)
  regTimes = Rationalize[regTimes1 / tRef] tRef;

  (* All the times that any of the simulations regrid *)
  regAnyTimes = Union@@regTimes;

  $UnequalBBoxes = {};

  gridsAt[gs_List, it_Integer, rl_Integer] :=
   Module[{is,i},
     (* Print["gridsAt"]; *)
     is = Position[gs[[All,1]], it];
     If[is==={}, {},
       i=is[[1,1]];
     (* Print["i=",i]; *)
       If[gs[[i,1]] =!= it, Error["Error in finding grid"]];
       gs[[i, 2, rl+1, 2]]]];

  checkIdenticalGrids[bboxLists_List, {t_, rl_Integer}] :=
   Module[{gs, equal, matrix},
     (* Print["t = ", t]; *)
     gs = MapThread[
       normalise[shrinkBBoxes[#1, #2, True, False, False (* FIXME: assuming bitant *)]] &,
       {bboxLists,buffers}];
     (* Print[1]; *)
     equal = Map[bboxesEqual[First[gs], #] &, Rest[gs]];
     (* Print[2]; *)
     If[! And @@ equal,
       (* Print[3]; *)
       matrix = Outer[bboxesEqual, gs, gs, 1];
       Sow[{t, rl, matrix, gs}, CheckIdenticalRegridding];
       Print["Grid equality check failed at t = ", N@t, 
         " on refinement level ", rl];
       AppendTo[$UnequalBBoxes, {N@t,rl,gs}];
       Print[MatrixForm[matrix]];
       ]];

  checkIdenticalGridsAtTime[t_] :=
   Module[{newIts, rl},
     newIts = Round[t/dts]+1;
     Do[
       (* Print["Checking rl ", rl, " at time ", t]; *)
       Global`$status = StringJoin[ToString/@{"Checking rl ", rl, " at time ", N@t}];
       checkIdenticalGrids[MapThread[gridsAt[#1,#2,rl] &, {grids, newIts}], {t, rl}],
       {rl, 2 (* TODO: include RL0, but don't remove buffer zones *), maxRL}]];

  (* range = 300;;-1;;1; *)
  range = All;

  Print["Checking up to t = ", regAnyTimes[[range]][[-1]]];

  reaped = Reap[checkIdenticalGridsAtTime /@ regAnyTimes[[range]], CheckIdenticalRegridding];

  diffs = reaped[[2,1]];
(* Print["after Reap"]; *)
  {regAnyTimes[[range]], diffs}];




ReadCarpetGridBBoxes[sim_String, t_, rl_, removeBuffers_: True] :=
 Module[{grids, dt, it, i, buffers = readBufferSize[sim], fullBBoxes},
  grids = readGridStructure[sim];
  dt = readIterationTimeStep[sim];
  it = Round[t/dt];
  its = grids[[All,1]];

  (* NB: using t = regrid_every * dt will give you the grid structure
     BEFORE regridding at that time, because regridding happens at the
     start of the following iteration. *)
  i = LengthWhile[its, # <= it &];

  gridsIt = grids[[i, 2]];
  (* Print["Grid structure at iteration ", grids[[i,1]]]; *)
  rls = gridsIt[[All,1]];
  If[rls =!= Range[0,Length[rls]-1],
    Error["Unexpected refinement levels: "<>ToString[rls]]];

  If[rl+1 > Length[rls], 
    (* TODO: decide whether asking for an RL which doesn't exist should
       raise an error *)
    {},
    (* Print["rl = ", gridsIt[[rl + 1, 1]]]; *)
    fullBBoxes = gridsIt[[rl + 1, 2]];
    If[removeBuffers, normalise[shrinkBBoxes[fullBBoxes, buffers, True]],
      normalise[fullBBoxes]]]];

(****************************************************************)
(* Plotting 2D grids *)
(****************************************************************)

Options[GridPlot2D] = 
  Join[Options[Graphics], {"RefinementLevelRange" -> All, "BufferSize" -> Automatic,
    "RefinementLevelLegend" -> False}];

(* readGrids *)

readGrids[sim_] := 
  (* To do: add file dependency tracking *)
 readGrids[sim] = 
  ReadCarpetGridStructure[sim];

(* processBBoxes *)

processBBoxes[bboxes_List, bufferSize_] :=
  (* TODO: currently assumes z reflection symmetry *)
 normalise[toXYPlane[shrinkBBoxes[bboxes, bufferSize, True]]];

(* plotRL *)

Options[plotRL] = Options[GridPlot2D];
plotRL[{rl_Integer, bboxes_List}, opts:OptionsPattern[]] :=
 plotBBoxPoints /@ 
  processBBoxes[bboxes, If[rl===0,0,OptionValue[BufferSize]]];

(* plotIteration *)

Options[plotIteration] = Options[GridPlot2D];

plotIteration[{it_Integer, rls_List}, opts:OptionsPattern[]] :=
  Module[{rlRange = OptionValue[RefinementLevelRange]},
    Thread[{Take[PresentationPlotStyles[[rlRange]], 
      Length[rls[[rlRange]]]], plotRL[#,opts] & /@ rls[[rlRange]]}]];

(* refinementLevelLegend *)

Options[refinementLevelLegend] = Options[GridPlot2D];
refinementLevelLegend[grids_, opts:OptionsPattern[]] :=
  PointLegend[PresentationPlotStyles, 
    Map["RL " <> ToString[#] &, grids[[2, All, 1]][[OptionValue[RefinementLevelRange]]]]];

(* GridPlot2D *)

GridPlot2D[grids_List, opts : OptionsPattern[]] :=
  If[OptionValue[RefinementLevelLegend],
    Legended[#, refinementLevelLegend[grids,opts]] &,
    Identity] @ Graphics[{AbsolutePointSize[0.5], plotIteration[grids, opts]},
      FilterRules[{opts}, Options[Graphics]],
      Frame -> True, FrameLabel -> {"x", "y"}, LabelStyle -> "Medium"];

GridPlot2D[sim_String, t_, opts : OptionsPattern[]] :=
 Module[{grids, dt, it, i,
   buffers = readBufferSize[sim], 
   its},
  grids = readGrids[sim];
   (* TODO: Show an empty plot in this case *)
   If[Length[grids] === 0, Error["No grid information for simulation "<>sim]];
  dt = readIterationTimeStep[sim];
  it = Round[t/dt];
  its = grids[[All, 1]];
  i = LengthWhile[its, # <= it &];
  GridPlot2D[grids[[i]], opts, BufferSize->buffers]];

End[];
EndPackage[];
