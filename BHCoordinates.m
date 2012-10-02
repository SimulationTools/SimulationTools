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

BeginPackage["SimulationTools`BHCoordinates`",
 {
  "SimulationTools`RunFiles`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`Providers`"
 }];

(* This package is deprecated.  Please use the Binary package instead. *)

ReadBHCoordinates(*::usage = "ReadBHCoordinates[sim, i] returns a DataTable containing the coordinates of the black hole labeled i, where i is typically 0 or 1"*);
ReadBHCoordinate(*::usage = "ReadBHCoordinate[sim, i, d] returns a DataTable containing the d'th coordinate of the black hole labeled i, where i is typically 0 or 1.  d can be 1, 2 or 3 for the x, y and z directions."*);
ReadBHTrajectory(*::usage = "ReadBHTrajectory[sim] returns a list of the x, y coordinates of the relative trajectory of the first two black holes.  \nReadBHTrajectory[sim, i] returns a list of the x, y coordinates of the trajectory of the black hole numbered i (i is typically 0 or 1)."*);
ReadBHTrajectories(*::usage = "ReadBHTrajectories[sim] returns a list containing the first two black hole trajectories.  Each trajectory is a list of the x, y coordinates of the trajectory of the black hole."*);
ReadBHSeparation(*::usage = "ReadBHSeparation[sim] returns a DataTable of the coordinate separation of black holes 0 and 1 as a function of time."*);
ReadBHPhase(*::usage = "ReadBHPhase[sim] returns a DataTable of the phase in radians of the relative orbit of black holes 0 and 1 projected into the xy plane as a function of time"*);
ReadBHRadius(*::usage = "ReadBHRadius[sim, i] returns a DataTable of the coordinate distance of black hole i from the origin as a function of time."*);
ReadBHPhaseOfFrequency(*::usage = "ReadBHPhaseOfFrequency[sim] returns a DataTable of the phase in radians of the relative orbit of black holes 0 and 1 projected into the xy plane as a function of its time derivative.  NB: The independent variable in the returned DataTable might not be monotonically increasing, and might cause problems if used with other SimulationTools functions."*);
ReadBHInclination(*::usage = "ReadBHInclination[sim] returns a DataTable of the angle in radians made by the relative orbit of black holes 0 and 1 in sim with the origin of coordinates."*);
ReadBHSpeed(*::usage = "ReadBHSpeed[sim, i] returns a DataTable of the coordinate speed of the black hole labeled i in sim as a function of time."*);
BHCoordinateMergerTime(*::usage = "BHCoordinateMergerTime[sim,eps] returns the time at which the BHs in sim reach a separation of eps (eps defaults to 0.01 if omitted)."*);
InitialSeparation;
InitialPosition(*::usage = "InitialPosition[sim, bh] returns a vector containing the initial coordinate position of BH numbered bh"*);

Begin["`Private`"];

(* General BH coordinates *)


(* TODO: Add input range checking to all these functions *)
Options[ReadBHCoordinates] = {Method -> Automatic};
ReadBHCoordinates[run_, tracker_, opts:OptionsPattern[]] :=
  CallProvidedFunction["BHCoordinates","ReadBHCoordinates",{run,tracker}, OptionValue[Method]];

ReadBHCoordinate[runName_String, tracker_Integer, coord_Integer] :=
  Module[{coords},
    If[tracker < 0,
      Error["ReadBHCoordinate: BH number must be non-negative"]];
    If[coord < 1 || coord > 3,
      Error["ReadBHCoordinate: Coordinate direction must be 1, 2 or 3"]];
    coords = ReadBHCoordinates[runName, tracker];
    MapData[#[[coord]]&, coords]];

ReadBHTrajectory[runName_String, tracker_Integer] :=
  Map[Take[Last[#], 2] &, 
    ToList[ReadBHCoordinates[runName, tracker]]];

ReadBHTrajectory[runName_String] :=
  Module[{coords1,coords2,rel},
    coords1 = ReadBHCoordinates[runName, 0];
    coords2 = ReadBHCoordinates[runName, 1];
    rel = coords1 - coords2;
    Map[Take[Last[#], 2] &, ToList[rel]]];

ReadBHTrajectories[runName_String] :=
  {ReadBHTrajectory[runName, 0], ReadBHTrajectory[runName, 1]};

ReadBHSeparation[runName_String] :=
  Module[{x0, x1, rad, l},
    x0 = ReadBHCoordinates[runName, 0];
    x1 = ReadBHCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    rad = MapThreadData[Norm[#1-#2] &, {Take[x0,l], Take[x1,l]}];
    Return[rad];
  ];

ReadBHRadius[runName_String, i_] :=
  Module[{x0, rad},
    x0 = ReadBHCoordinates[runName, i];
    rad = MapData[Norm[#] &, x0];
    Return[rad];
  ];

ReadBHPhase[runName_String] :=
  Module[{x0, x1, l, xyTrans},
    x0 = ReadBHCoordinates[runName, 0];
    x1 = ReadBHCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    xyTrans = MapThreadData[Take[#1-#2,2] &, {Take[x0,l], Take[x1,l]}]; (* Project into xy plane *)
    Return[Phase[xyTrans]];
  ];

ReadBHInclination[runName_String] :=
  Module[{x0, x1, l, x0p, x1p, rp, theta},
    x0 = ReadBHCoordinates[runName, 0];
    x1 = ReadBHCoordinates[runName, 1];
    l = Min[Length[x0],Length[x1]];
    x0p = Take[x0,l];
    x1p = Take[x1,l];
    rp = x0p - x1p;
    theta = MapData[If[Norm[#] > 10^-4, ArcCos[#[[3]]/Norm[#]], 0.0] &, rp];
    Return[Pi/2.0 - theta]];


ReadBHPhase[runName_String, i_] :=
  Module[{x0, xyTrans},
    x0 = ReadBHCoordinates[runName, i];
    xyTrans = MapData[Take[#,2] &, x0]; (* Project into xy plane *)
    Return[Phase[xyTrans]];
  ];

ReadBHPhaseOfFrequency[run_] :=
  Module[{phaseFreq, phaseFreqDeps, phaseOfFreq},
    phaseFreq =
     IntersectDataTables[{ReadBHPhase[run],
       NDerivative@ReadBHPhase[run]}];
    phaseFreqDeps = Reverse[DepVar /@ phaseFreq];
    phaseOfFreq = MakeDataTable[MapThread[List, phaseFreqDeps]]];

ReadBHSpeed[run_, bh_] :=
 Norm@NDerivative[ReadBHCoordinates[run, bh]];

BHCoordinateMergerTime[run_, eps_:0.01] :=
 Module[{sep},
  sep = ReadBHSeparation[run];
  Select[ToList@sep, #[[2]] > eps &][[-1, 1]]]

DefineMemoFunction[InitialSeparation[run_],
  First@DepVar@ReadBHSeparation[run]];

DefineMemoFunction[InitialPosition[run_, bh_],
  First@DepVar@ReadBHCoordinates[run, bh]];

End[];

EndPackage[];
