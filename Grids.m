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

BeginPackage["SimulationTools`Grids`",
 {
  "SimulationTools`BHCoordinates`",
  "SimulationTools`ColumnFile`",
  "SimulationTools`DataTable`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`Error`",
  "SimulationTools`Horizons`",
  "SimulationTools`Memo`",
  "SimulationTools`Parameters`",
  "SimulationTools`Plotting`",
  "SimulationTools`RunFiles`",
  "SimulationTools`Segments`"
 }];

ReadTimeStep::usage = "ReadTimeStep[sim] returns the time step size in sim.";
ReadGridSpacings::usage = "ReadGridSpacings[sim] returns a list of the grid spacing in each direction in sim.";
ReadTimeRange::usage = "ReadTimeRange[sim] reads the range of times at which data is available for sim.";
ReadMaxRefinementLevels::usage = "ReadMaxRefinementLevels[sim] returns the maximum number of refinement levels present in sim.";


(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

PlotGridsAndAH2D(*::usage = "PlotGridsAndAH2D[sim] generates a 2D plot of the grid structure and apparent horizons in which the timestep can be interactively changed."*);
ReadGridStructure(*::usage = "ReadGridStructure[sim] computes the grid structure for sim. For each refinement level it returns a list of the form {level, box radii, coarse spacing, number of points, courant factor, level exists every}."*);

ReadInnerBoundary(*::usage = "ReadInnerBoundary[sim] reads the position of the inner boundary of the Llama spherical patches of sim."*);
ReadOuterBoundary(*::usage = "ReadOuterBoundary[sim] reads the position of the outer boundary of the Llama spherical patches of sim."*);
ReadOuterBoundaries(*::usage = "ReadOuterBoundary[sim] reads the position of the outer boundary of the Llama spherical patches of sim."*);
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
ReadRefinementLevelsOfCentre;
ReadRadiusOfCentre;
ReadGridSpacingOfCentre;
ReadHorizonRadiusGridCells;
HorizonGridCellsPlot;

Begin["`Private`"];

(**********************************************************)
(* ReadTimeStep                                           *)
(**********************************************************)

Options[ReadTimeStep] = {
	"Map"             -> Automatic,
    "RefinementLevel" -> Automatic
  };

DocumentationBuilder`OptionDescriptions["ReadTimeStep"] = {
    "Map"             -> "The map to read from.",
    "RefinementLevel" -> "The refinement level to read from."
  };

SyntaxInformation[ReadTimeStep] =
 {"ArgumentsPattern" -> {_, OptionsPattern[]}};

ReadTimeStep[run_, OptionsPattern[]] :=
 Module[{method, dt, dtfac, trf, rl},
  If[OptionValue[Map] =!= Automatic,
  	Error["Specifying a map is currently not supported."];
  ];

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

  rl  = OptionValue[RefinementLevel] /.
   {Automatic -> 0, Max -> Length[trf]-1};

  If[rl >= ReadMaxRefinementLevels[run],
  	Error["Refinement level " <> ToString[rl] <> " does not exist."];
  ];

  dt/trf[[rl+1]]
];


(**********************************************************)
(* ReadGridSpacings                                       *)
(**********************************************************)

Options[ReadGridSpacings] = {
	"Map"             -> Automatic,
    "RefinementLevel" -> Automatic
  };

DocumentationBuilder`OptionDescriptions["ReadGridSpacings"] = {
    "Map"             -> "The map to read from.",
    "RefinementLevel" -> "The refinement level to read from."
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

  h0 = If[multipatchQ[run],
    ConstantArray[ToExpression[LookupParameter[run, "Coordinates::h_cartesian", $Failed]],3],
    ToExpression /@
    	 {LookupParameter[run, "CoordBase::dx", $Failed],
          LookupParameter[run, "CoordBase::dy", $Failed],
          LookupParameter[run, "CoordBase::dz", $Failed]}];

  h0 / 2^rl];


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

ReadTimeRange[run_] :=
 Module[{segs, times},
  segs = FindRunSegments[run];
  times = Flatten[Cases[SegmentCoordinateTimeInterval /@ segs, Except[None]]];
  {Min[times], Max[times]}
];

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

ReadOuterBoundary[run_] :=
  ToExpression[LookupParameter[run, "Coordinates::sphere_outer_radius"]];


(**********************************************************)
(* ReadOuterBoundaries                                    *)
(**********************************************************)

SyntaxInformation[ReadOuterBoundaries] =
 {"ArgumentsPattern" -> {_}};

ReadOuterBoundaries[run_] :=
 Module[{xmin, xmax, ymin, ymax, zmin, zmax},
  {xmin, xmax, ymin, ymax, zmin, zmax} =
    ToExpression[LookupParameter[run, "CoordBase::"<>#]]& /@
      {"xmin", "xmax", "ymin", "ymax", "zmin", "zmax"};
  {{xmin, xmax}, {ymin, ymax}, {zmin, zmax}}
];


(**********************************************************)
(* ReadMaxRefinementLevels                                *)
(**********************************************************)

SyntaxInformation[ReadMaxRefinementLevels] =
 {"ArgumentsPattern" -> {_}};

ReadMaxRefinementLevels[run_String] :=
  ToExpression[LookupParameter[run, "Carpet::max_refinement_levels", 1]];








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
  fileNames = FindSimulationFiles[run, fileName];
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

PlotCarpetGrids2D[run_String, opts:(_Rule...)] :=
  Module[{grids, dt, tRange},
  grids = ReadCarpetGrids[run];
  dt = ReadFineTimeStep[run];
  tRange = ReadTimeRange[run];
  Manipulate[Graphics[PlotCarpetGrids2D[grids, dt, t],opts], {{t, tRange[[1]], "t"}, tRange[[1]], tRange[[2]]}]];

PlotCarpetGrids2D[run_String, t_?NumberQ, opts:(_Rule...)] :=
  Module[{grids, dt, tRange},
  grids = ReadCarpetGrids[run];
  dt = ReadFineTimeStep[run];
  tRange = ReadTimeRange[run];
  Graphics[PlotCarpetGrids2D[grids, dt, t]], opts];


GridSpacingOnLevel[runName_, l_] :=
  Module[{h0},
    h0 =
    If[!multipatchQ[runName],
      ToExpression[LookupParameter[runName, "CoordBase::dx"]],
      ToExpression[LookupParameter[runName, "Coordinates::h_cartesian"]]];
    h0 / 2^l];

ReadCoarseGridSpacing[runName_] :=
  Module[{h0},
    h0 = If[!multipatchQ[runName],
      ToExpression[LookupParameter[runName, "CoordBase::dx"]],
      ToExpression[LookupParameter[runName, "Coordinates::h_cartesian"]]]];

FinestGridSpacing[runName_] :=
  GridSpacingOnLevel[runName, Max[RefinementLevels[runName]]];

multipatchQ[sim_String] :=
  LookupParameter[sim, "CartGrid3D::type"] === "multipatch";

BoxRadiiOnLevel[runName_, l_] :=
  Module[{params, coarseRadius},
    If[l == 0,
      coarseRadius = If[multipatchQ[runName],
            {ReadInnerBoundary[runName], ReadOuterBoundary[runName]},
            ToExpression@LookupParameter[runName, "CoordBase::xmax"]],
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
  Module[{s1, s2, s3s, facs, maxRLs},
    s1 = LookupParameter[runName, "Carpet::time_refinement_factors", Automatic];

    If[s1 === Automatic,
      Table[2^rl, {rl, 0, ReadMaxRefinementLevels[runName] - 1}],
      (* else *)
      s2 = StringCases[s1, ("[" ~~ facs__ ~~ "]") -> facs][[1]];
      s3s = StringSplit[s2, ","];
      Map[ToExpression, s3s]]];

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
   Take[ReadColumnFile[run, "puncturetracker"~~("-"|"::")~~"pt_loc..asc", {1, 9}], 
    2];
  {{it1, t1}, {it2, t2}} = pairs;
  dtFine = (t2 - t1)/(it2 - it1)];

ReadRefinementLevelsOfCentre[sim_String, c_Integer] :=
 ToDataTable[
  ReadColumnFile[sim, 
   "carpetregrid2-num_levels..asc", {"time", 
    "num_levels[" <> ToString[c - 1] <> "]"}]];

ReadRadiusOfCentre[sim_String, c_Integer, rl_] := 
  readRadiusOfCentre[sim, c, rl] =
  Module[{data, thisRL, withoutNone, tStart = SessionTime[]},
    data = 
    ReadColumnFile[sim, 
      "carpetregrid2-radii.x.asc", {"time", "ix", 
        "radius[" <> ToString[c - 1] <> "]"}];
    thisRL = Select[data, #[[2]] == rl &][[All, {1, 3}]];
    withoutNone = thisRL /. (-1 -> None);
    ToDataTable[withoutNone]]

ReadGridSpacingOfCentre[sim_String, cen_Integer] :=
 ReadCoarseGridSpacing[sim]/
  2^(ReadRefinementLevelsOfCentre[sim, cen] - 1);

ReadHorizonRadiusGridCells[sim_String, ah_Integer] :=
 Module[{centreOfAH, dx, ahrMin},
  (* Assume that the CarpetRegrid2 centres and AH indices coincide.  
  How to check this? *)
  centreOfAH[ahp_Integer] :=
   ahp;
  dx = ReadGridSpacingOfCentre[sim, centreOfAH[ah]];
  ahrMin = ReadAHMinRadius[sim, ah];
  ahrMin/dx // WithResampling];

HorizonGridCellsPlotData[sims : {_String ...}] :=
  If[FindRunFile[sims[[1]], "carpetregrid2-num_levels..asc"] === {},
    Table[Table[ToDataTable[{{0,0},{1,0}}], {sim, sims}], {ah, 
      1, 2}],
    (* else *)
    Table[Table[Quiet[ReadHorizonRadiusGridCells[sim, ah],Interpolation::inhr], {sim, sims}], {ah, 
      1, 2}]];

HorizonGridCellsPlot[sims : {_String ...}] :=
  HorizonGridCellsPlot[sims, HorizonGridCellsPlotData[sims]]

HorizonGridCellsPlot[sims_List, cellss_List] :=
  Module[{n, styles, labels},
    n = Length[sims];
    styles = Take[PresentationPlotStyles, n];
    (* Only show labels for first AH.  Otherwise it is too cluttered. *)
    labels = 
    Flatten[Table[
      Table[sim <> " AH" <> ToString[ah], {sim, sims}], {ah, 1, 1}], 1];
    PresentationListLinePlot[Flatten[cellss, 1], PlotRange -> {0, All}, 
      PlotLegend -> labels,
      PlotStyle -> Join[styles, Map[{#, Dashed} &, styles]],
      LegendPosition -> {Left, Bottom},
      FrameLabel -> {"t/M", 
        "\!\(\*SubscriptBox[\(r\), \(AH\)]\)/\!\(\*SubscriptBox[\(h\), \
          \(fine\)]\)"}]]

End[];

EndPackage[];
