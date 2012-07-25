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

BeginPackage["Grids`",
 {
  "BHCoordinates`",
  "Error`",
  "Horizons`",
  "Memo`",
  "NR`",
  "Parameters`",
  "RunFiles`"
 }];

ReadTimeStep::usage = "ReadTimeStep[sim] returns a list of the time step sizes on each refinement level in sim." <>
  "ReadTimeStep[sim, level] reads the time step on a specific refinement level in sim.";
ReadGridSpacings::usage = "ReadGridSpacings[sim] returns a list of the grid spacings in each direction on each refinement level in sim." <>
  "ReadGridSpacings[sim, level] reads the grid spacing in each direction on a specific refinement level in sim.";

ReadTimeRange::usage = "ReadTimeRange[sim] reads the range of times at which data is available for sim.";

ReadMaxRefinementLevels::usage = "ReadMaxRefinementLevels[sim] returns the maximum number of refinement levels present in sim.";


(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

PlotGridsAndAH2D(*::usage = "PlotGridsAndAH2D[sim] generates a 2D plot of the grid structure and apparent horizons in which the timestep can be interactively changed."*);
ReadGridStructure(*::usage = "ReadGridStructure[sim] computes the grid structure for sim. For each refinement level it returns a list of the form {level, box radii, coarse spacing, number of points, courant factor, level exists every}."*);

ReadInnerBoundary(*::usage = "ReadInnerBoundary[sim] reads the position of the inner boundary of the Llama spherical patches of sim."*);
ReadOuterBoundary(*::usage = "ReadOuterBoundary[sim] reads the position of the outer boundary of the Llama spherical patches of sim."*);
ReadAngularPoints(*::usage = "ReadAngularPoints[sim] reads the number of angular points in the Llama spherical patches of sim."*);


(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

AnimateGrids(*::usage = "AnimateGrids[simname] creates an animation of the grid structure of a sim"*);
LoadGrids;
PlotGrids;
PlotGrid;
GridsBoxes;
ReadCarpetGrids;
PlotCarpetGrids2D;

FinestGridSpacing;
ReadCoarseGridSpacing;
ReadCoarseTimeStep;
ReadFineTimeStep;
GridSpacingOnLevel;
BoxRadiiOnLevel;
BoxRadiiForCentre;
CountRefinementLevels;
RefinementLevels;
TimeRefinementFactors;
CourantFactorOnLevel;
LevelExistsEvery;
RadialPoints;
GridStructure;

Begin["`Private`"];

(**********************************************************)
(* ReadTimeStep                                           *)
(**********************************************************)

SyntaxInformation[ReadTimeStep] =
 {"ArgumentsPattern" -> {_, ___}};

ReadTimeStep[run_] :=
 Module[{method, dt, dtfac, trf},
  method = LookupParameter[run, "Time::timestep_method", "courant_static"];

  Which[
   StringMatchQ[method, "given", IgnoreCase -> True],
    dt = ToExpression[LookupParameter[run, "Time::timestep", $Failed]];,
   StringMatchQ[method, "courant_static", IgnoreCase -> True],
    dtfac = ToExpression[LookupParameter[run, "Time::dtfac", $Failed]];
    If[dtfac === $Failed,
      dt = $Failed;
    ,
      dt = dtfac * Min[ReadGridSpacings[run]];
    ];,
   True,
    dt = $Failed
  ];

  If[dt === $Failed, Return[$Failed]];

  trf = TimeRefinementFactors[run];

  dt/trf
];

ReadTimeStep[run_, level_] :=
  ReadTimeStep[run][[level+1]];


(**********************************************************)
(* ReadGridSpacings                                       *)
(**********************************************************)

Options[ReadGridSpacings] = {
	"Map"             -> Automatic,
    "RefinementLevel" -> Automatic
  };

SyntaxInformation[ReadGridSpacings] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};

ReadGridSpacings[run_, OptionsPattern[]] :=
 Module[{rl, h0},
  If[OptionValue[Map] =!= Automatic,
  	Error["Specifying a map is currently not supported."];
  ];

  rl  = OptionValue[RefinementLevel] /. Automatic -> 0;

  If[rl >= ReadMaxRefinementLevels[run],
  	Error["Refinement level " <> ToString[rl] <> " does not exist."];
  ];

  h0 = ToExpression[LookupParameter[run, "Coordinates::h_cartesian", $Failed]];

  If[h0 =!= $Failed,
    h0 = {h0, h0, h0};
  ,
    h0 = ToExpression /@
    	 {LookupParameter[run, "CoordBase::dx", $Failed],
          LookupParameter[run, "CoordBase::dy", $Failed],
          LookupParameter[run, "CoordBase::dz", $Failed]};
  ];

  h0 / 2^rl
];


(**********************************************************)
(* ReadAngularPoints                                      *)
(**********************************************************)

SyntaxInformation[ReadAngularPoints] =
 {"ArgumentsPattern" -> {_}};

ReadAngularPoints[run_] :=
  Module[{},
    ToExpression[LookupParameter[run, "Coordinates::n_angular"]]
  ];


(**********************************************************)
(* ReadTimeRange                                          *)
(**********************************************************)

SyntaxInformation[ReadTimeRange] =
 {"ArgumentsPattern" -> {_}};

(* TODO: Make this more flexible by not relying on PunctureTracker *)
ReadTimeRange[run_] :=
  Module[{pairs, first, last},
   pairs = ReadColumnFile[run, "puncturetracker::pt_loc..asc", {1, 9}];
   first = pairs[[1, 2]];
   last = pairs[[-1, 2]];
   {first, last}];


(**********************************************************)
(* ReadInnerBoundary                                      *)
(**********************************************************)

SyntaxInformation[ReadInnerBoundary] =
 {"ArgumentsPattern" -> {_}};

ReadInnerBoundary[run_] :=
  ToExpression[LookupParameter[run, "Coordinates::sphere_inner_radius"]];


(**********************************************************)
(* ReadOuterBoundary                                      *)
(**********************************************************)

SyntaxInformation[ReadOuterBoundary] =
 {"ArgumentsPattern" -> {_}};

(* TODO: Add support for Cartesian grids *)
ReadOuterBoundary[run_] :=
  ToExpression[LookupParameter[run, "Coordinates::sphere_outer_radius"]];


(**********************************************************)
(* ReadMaxRefinementLevels                                *)
(**********************************************************)

SyntaxInformation[ReadMaxRefinementLevels] =
 {"ArgumentsPattern" -> {_}};

ReadMaxRefinementLevels[run_String] :=
  ToExpression[LookupParameter[run, "Carpet::max_refinement_levels"]];








(****************************************************************)
(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)
(****************************************************************)

(**********************************************************)
(* PlotGridsAndAH2D                                       *)
(**********************************************************)

DefineMemoFunction[PlotGridsAndAH2D[sim_String],
  Module[{radius1, xpos1, ypos1, radius2, xpos2, ypos2, grids, dt},

  (* Get horizon position and radius for both black holes *)
  radius1 = Interpolation[ReadAHRadius[sim, 1]];
  xpos1 = Interpolation[ReadBHCoordinate[sim, 0, 1]];
  ypos1 = Interpolation[ReadBHCoordinate[sim, 0, 2]];
  radius2 = Interpolation[ReadAHRadius[sim, 2]];
  xpos2 = Interpolation[ReadBHCoordinate[sim, 1, 1]];
  ypos2 = Interpolation[ReadBHCoordinate[sim, 1, 2]];

  (* Get grid information *)
  grids = ReadCarpetGrids[sim];
  grids = rescaleGrids[grids, ReadCoarseGridSpacing[sim]];

  (* We need the timestep to relate iteration number to time *)
  dt = ReadFineTimeStep[sim];

  Show[Graphics[PlotCarpetGrids2D[grids, dt, #]],
    Graphics[Disk[{xpos1[#], ypos1[#]}, radius1[#]]],
    Graphics[Disk[{xpos2[#], ypos2[#]}, radius2[#]]],
    PlotLabel -> "t=" <> ToString[#]] &

  ]
];


(**********************************************************)
(* ReadGridStructure                                      *)
(**********************************************************)

ReadGridStructure[runName_String] :=
  Map[{#, BoxRadiiOnLevel[runName, #],
       GridSpacingOnLevel[runName, #],
       RadialPoints[runName, #],
       CourantFactorOnLevel[runName, #],
       LevelExistsEvery[runName, #]} &, RefinementLevels[runName]];



(****************************************************************)
(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)
(****************************************************************)

LoadGrids[runName_] :=
 Module[{allLines, linesByt1, linesByt2, its, ts},
  allLines = ReadColumnFile[runName, "grid.asc"];
  its = Union[Map[#[[1]] &, allLines]];
  ts = Union[Map[#[[2]] &, allLines]];
  linesByt1 = Split[allLines, #1[[1]] == #2[[1]] &]; 
  linesByt2 = Map[First[#][[2]] -> # &, linesByt1]];

gridAtTime[grids_, t_] :=
 Module[{gs},
  gs = Last[Select[grids, #[[1]] <= t &]][[2]]]

boxFromLine[{it_, t_, rl_, c_, x1_, y1_, z1_, x2_, y2_, z2_}] :=
 Cuboid[{x1, y1, z1}, {x2, y2, z2}];

reflectLineZ[{it_, t_, rl_, c_, x1_, y1_, z1_, x2_, y2_, 
   z2_}] :=
 {it, t, rl, c, x1, y1, -z2, x2, y2, z2};

rotateLine[{it_, t_, rl_, c_, x1_, y1_, z1_, x2_, y2_, z2_}] :=
 If[y1 == -y2 && x1 == 0,
  {{it, t, rl, c, -x2, y1, z1, x2, y2, z2}},
  {{it, t, rl, c, x1, y1, z1, x2, y2, z2},
   {it, t, rl, c, -x2, -y2, z1, -x1, -y1, z2}}];

simBoxes[grids_, t_] :=
 Module[{grids2, boxes, boxes2, lines2},
  grids2 = gridAtTime[grids, t];
(*  lines2 = Flatten[Map[rotateLine[reflectLineZ[#]] &, grids2], 1]; *)
  lines2 = grids2;
  boxes = Map[boxFromLine, lines2];
  boxes2 = Flatten[boxes, 1]];

GridsBoxes[grids_, t_] :=
  simBoxes[grids, t];

PlotGrids[grids_, t_, R_] :=
 Graphics3D[{Blue,EdgeForm[Thin],Opacity[0.1], simBoxes[grids, t]}, 
  PlotRange -> {{-R, R}, {-R, R}, {-R, R}}];

AnimateGrids[runName_String] :=
  AnimateGrids[runName, 25, 1, 30];

AnimateGrids[runName_String, R_?NumberQ, dt_?NumberQ, fps_?NumberQ] :=
 Module[{grids, tMin, tMax, t},
  grids = LoadGrids[runName];
  tMin = First[grids][[1]];
  tMax = Last[grids][[1]];
  Animate[PlotGrids[grids, t, R], {t, tMin, tMax, dt}, 
   ControlPlacement -> Bottom, AnimationRate -> fps, 
   AnimationRunning -> False, AnimationRepetitions -> 1]];

ReadCarpetGridsFromFile[fileName_] :=
 Module[{lines, readIteration, its,
    itNos, itVals, itPairs, itPairs1, itPairs2, trToVec},

  trToVec[str_] :=
    ToExpression /@ StringSplit[str, ","];

  readIteration[itLines_] :=
   Module[{readLine},
    readLine[line_] :=
     Module[{vecs},
      vecs = 
       StringCases[line[[5]], 
         "([" ~~ Shortest[x1__] ~~ "]:[" ~~ Shortest[x2__] ~~ 
           "]" :> {x1, x2}][[1]];
      trToVec /@ vecs];
    readLine /@ 
     Select[itLines, 
      Length[#] == 5 && StringQ[#[[5]]] && 
        StringTake[#[[5]], 1] == "(" &]];
  
  lines = ReadColumnFile[fileName];

(*  lines = ReadColumnFile[run, fileName];*)
  its = SplitBy[lines, Length[#] >= 1 && #[[1]] == "iteration" &];
  itNos = Take[its, {1, Length[its], 2}];
  itVals = Take[its, {2, Length[its], 2}];
  itPairs1 = MapThread[List, {Last[#[[1]]] & /@ itNos, itVals}];
  itPairs = SplitBy[itPairs1, #[[1]] &];
  (* Select only map 0 *)
  
  itPairs2 = Map[First, itPairs];
  Map[{#[[1]], readIteration[#[[2]]]} &, itPairs2]
  ];

mergeFiles[files_List] :=
  Module[{file1, fileEndIndex, rest, rest0, restIndex, truncated},
    If[Length[files] == 0, Return[{}]];
    If[Length[files] == 1, Return[First[files]]];

    file1 = First[files];

    (* The first column of the files will be taken to be an
    index. Usually this will be an interation number or a coordinate
    time. *)
    fileEndIndex = First[Last[file1]];
    rest = mergeFiles[Rest[files]];

    rest0 = First[rest];
    restIndex = First[rest0];

    (* The most common case: no overlap *)
    If[restIndex > fileEndIndex,
      Return[Join[file1, rest]]];

    (* We have some overlap *)
    truncated = Select[file1, (First[#] < restIndex) &];
    Return[Join[truncated, rest]];
  ];

rescaleGrids[grids_, spacing_] :=
 Module[{range, rescaledgrids},
  If[Depth[grids]==6,
    rescaledgrids = Map[rescaleGrids[#, spacing]&, grids];
  ,
    range = Max[grids[[2]]] - Min[grids[[2]]];
    rescaledgrids = MapAt[spacing (#-range/2)&, grids, 2];
  ];

  rescaledgrids
];

DefineMemoFunction[ReadCarpetGrids[run_, fileName_:"carpet-grid.asc"],
 Module[{fileNames, files},
  fileNames = FindRunFile[run, fileName];
  If[files === {}, Error["No file " <> fileName <> " found in simulation " <> run]];
  files = Map[ReadCarpetGridsFromFile, fileNames];
  mergeFiles[files]]
];

plotBox2D[{v1_, v2_}] :=
 {Blue,EdgeForm[Thin],Opacity[0.1], {Rectangle[Take[v1, 2], Take[v2, 2]]}}

plotIteration2D[{it_, boxes_}] :=
 {Map[plotBox2D, boxes]}

PlotCarpetGrids2D[grids_List, dtFine_?NumberQ, t_?NumberQ] :=
  Module[{},
   plotIteration2D[Last[Select[grids, #[[1]] <= t/dtFine &]]]]

PlotCarpetGrids2D[run_String] :=
  Module[{grids, dt, tRange},
  grids = ReadCarpetGrids[run];
  dt = ReadFineTimeStep[run];
  tRange = ReadTimeRange[run];
  Manipulate[Graphics[PlotCarpetGrids2D[grids, dt, t]], {{t, tRange[[1]], "t"}, tRange[[1]], tRange[[2]]}]];

PlotCarpetGrids2D[run_String, t_?NumberQ] :=
  Module[{grids, dt, tRange},
  grids = ReadCarpetGrids[run];
  dt = ReadFineTimeStep[run];
  tRange = ReadTimeRange[run];
  Graphics[PlotCarpetGrids2D[grids, dt, t]]];


GridSpacingOnLevel[runName_, l_] :=
  Module[{h0},
    h0 = ToExpression[LookupParameter[runName, "CoordBase::dx",
      LookupParameter[runName, "Coordinates::h_cartesian"]]];
    h0 / 2^l
  ];

ReadCoarseGridSpacing[runName_] :=
  Module[{h0},
    h0 = ToExpression[LookupParameter[runName, "CoordBase::dx",
      LookupParameter[runName, "Coordinates::h_cartesian", $Failed]]]
  ];

FinestGridSpacing[runName_] :=
  GridSpacingOnLevel[runName, Max[RefinementLevels[runName]]];

BoxRadiiOnLevel[runName_, l_] :=
  Module[{params, coarseRadius},
    If[l == 0,
      coarseRadius = LookupParameter[runName, "CoordBase::xmax",
            {ReadInnerBoundary[runName], ReadOuterBoundary[runName]}];
      If[Head[coarseRadius != List], coarseRadius = {coarseRadius}];
      Return[coarseRadius]
    ];
    params = FindParameters[runName, "regridboxes::centre_*_radius" ~~ (Whitespace | "") ~~ "["<>ToString[l]<>"]"];

    If[params === {},
      params = FindParameters[runName, "carpetregrid2::radius_*" ~~ (Whitespace | "") ~~ "["<>ToString[l]<>"]"]];

    Map[ToExpression[LookupParameter[runName, #]] &, params]
  ];

BoxRadiiForCentre[runName_, c_] :=
  Module[{params},
    (* If[l == 0, Return[{ToExpression[LookupParameter[runName, "CoordBase::xmax"]]}]]; *)
    params = FindParameters[runName, "carpetregrid2::radius_"<>ToString[c] ~~ (Whitespace | "") ~~ "[*]"];

    Prepend[Map[{ToExpression@StringReplace[#, __ ~~ "[" ~~ n__ ~~ "]" -> n],
         ToExpression[LookupParameter[runName, #]]} &, params], {0, ReadOuterBoundary[runName]}]
  ];

CountRefinementLevels[runName_String] :=
  ToExpression[LookupParameter[runName, "Carpet::max_refinement_levels"]];

CountRefinementLevels[runName_String, i_] :=
  ToExpression[LookupParameter[runName,
                               "CarpetRegrid2::num_levels_"<>ToString[i]]];

RefinementLevels[runName_String] :=
  Table[l, {l, 0, CountRefinementLevels[runName]-1}];

RadialPoints[runName_String, level_Integer] :=
   Round[BoxRadiiOnLevel[runName, level] / GridSpacingOnLevel[runName, level]];

GridStructure[runName_String] :=
  Map[{#, BoxRadiiOnLevel[runName, #],
       GridSpacingOnLevel[runName, #], 
       RadialPoints[runName, #],
       CourantFactorOnLevel[runName, #],
       LevelExistsEvery[runName, #]} &, RefinementLevels[runName]];

ReadCoarseTimeStep[runName_] :=
  Module[{dt},
    dt = ToExpression[LookupParameter[runName, "Time::timestep", $Failed]]
  ];

TimeRefinementFactors[runName_String] :=
  Module[{s1, s2, s3s, facs},
    s1 = LookupParameter[runName, "Carpet::time_refinement_factors"];
    s2 = StringCases[s1, ("[" ~~ facs__ ~~ "]") -> facs][[1]];
    s3s = StringSplit[s2, ","];
    facs = Map[ToExpression, s3s];
    Return[facs];
  ];

TimeRefinementFactor[runName_String, level_] :=
  TimeRefinementFactors[runName][[level+1]];

CourantFactorOnLevel[runName_String, level_] :=
  Module[{dtfac, trf, srf},
    dtfac = ToExpression[LookupParameter[runName, "Time::dtfac",
        ReadCoarseTimeStep[runName] / ReadCoarseGridSpacing[runName]]];
    trf = TimeRefinementFactor[runName, level];
    srf = 2^level;
    Return[dtfac * srf/trf];
  ];

LevelExistsEvery[runName_String, level_Integer] :=
  TimeRefinementFactor[runName, CountRefinementLevels[runName]-1] / 
    TimeRefinementFactor[runName, level];

ReadFineTimeStep[run_] :=
 Module[{pairs, it1, it2, t1, t2, dtFine},
  pairs = 
   Take[ReadColumnFile[run, "puncturetracker::pt_loc..asc", {1, 9}], 
    2];
  {{it1, t1}, {it2, t2}} = pairs;
  dtFine = (t2 - t1)/(it2 - it1)];

End[];

EndPackage[];
