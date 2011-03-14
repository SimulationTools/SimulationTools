(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["Grids`", {"BHCoordinates`", "Horizons`", "Memo`", "NR`", "RunFiles`"}];

AnimateGrids::usage = "AnimateGrids[runname] creates an animation of the grid structure of a run";
LoadGrids;
PlotGrids;
PlotGrid;
GridsBoxes;
ReadCarpetGrids;
PlotCarpetGrids2D;
PlotGridsAndAH2D::usage = "PlotGridsAndAH2D[run] generates a 2D plot of the grid structure and apparent horizons in which the timestep can be interactively changed.";
RescaleGrids::usage = "RescaleGrids[grids, coordinatesize] rescales 'grids' such that the coarsest grid has spacing 'coordinatesize'.";

Begin["`Private`"];

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

RescaleGrids[grids_, spacing_] :=
 Module[{range, rescaledgrids},
  If[Depth[grids]==6,
    rescaledgrids = Map[RescaleGrids[#, spacing]&, grids];
  ,
    range = Max[grids[[2]]] - Min[grids[[2]]];
    rescaledgrids = MapAt[spacing (#-range/2)&, grids, 2];
  ];

  rescaledgrids
];

DefineMemoFunction[ReadCarpetGrids[run_, fileName_:"carpet-grid.asc"],
 Module[{fileNames, files},
  fileNames = FindRunFile[run, fileName];
  If[files === {}, Throw["No file " <> fileName <> " found in simulation " <> run]];
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
  grids = RescaleGrids[grids, ReadCoarseGridSpacing[sim]];

  (* We need the timestep to relate iteration number to time *)
  dt = ReadFineTimeStep[sim];

  Show[Graphics[PlotCarpetGrids2D[grids, dt, #]],
    Graphics[Disk[{xpos1[#], ypos1[#]}, radius1[#]]],
    Graphics[Disk[{xpos2[#], ypos2[#]}, radius2[#]]],
    PlotLabel -> "t=" <> ToString[#]] &

  ]
];

PlotCarpetGrids2D[run_String, t_?NumberQ] :=
  Module[{grids, dt, tRange},
  grids = ReadCarpetGrids[run];
  dt = ReadFineTimeStep[run];
  tRange = ReadTimeRange[run];
  Graphics[PlotCarpetGrids2D[grids, dt, t]]];

End[];

EndPackage[];
