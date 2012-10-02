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

BeginPackage["SimulationTools`Tracks`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`NR`",
  "SimulationTools`RunFiles`"
 }];

AnimateTracks;
ReadTracks;
PlotTracks;

Begin["`Private`"];

ReadTracks[runName_, tracker_] :=
 Module[{xTb, yTb, zTb, xFn, yFn, zFn},
  xTb = ReadMinTrackerCoordinate[runName, tracker, 1];
  yTb = ReadMinTrackerCoordinate[runName, tracker, 2];
  zTb = ReadMinTrackerCoordinate[runName, tracker, 3];
  xFn = Interpolation[xTb];
  yFn = Interpolation[yTb];
  zFn = Interpolation[zTb];
  Function[t, {xFn[t], yFn[t], zFn[t]}]];

track[x_, t_] :=
 Line[Table[x[tp], {tp, 0, t, 1}]];

PlotTracks[x1_, x2_, t_] := 
 Graphics3D[{Black, track[x1, t], track[x2, t], Red, 
   Sphere[x1[t], 0.5], Sphere[x2[t], 0.5]}, 
  PlotRange -> {{-10, 10}, {-10, 10}, {-1, 1}} (*,ViewVector->vv[t],
  ViewCenter->{0,0,0} *), Background -> White, Boxed -> True];

AnimateTracks[runName_] :=
  Module[{t1,t2,tMin,tMax,coord},
    coord = ReadMinTrackerCoordinate[runName, 0, 1];
    tMin = First[ToList[coord]][[1]];
    tMax = Last[ToList[coord]][[1]];
    t1 = ReadTracks[runName,0];
    t2 = ReadTracks[runName,1];
    Animate[PlotTracks[t1,t2,t], {t, tMin, tMax}, 
      ControlPlacement -> Bottom, AnimationRate -> 100, 
      AnimationRunning -> False, AnimationRepetitions -> 1]];

End[];

EndPackage[];
