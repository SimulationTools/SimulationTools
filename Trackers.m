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

BeginPackage["SimulationTools`Trackers`",
 {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`"
 }];

ReadTrackerCoordinates::usage = "ReadTrackerCoordinates[sim, tracker] gives the Cartesian coordinates of the tracker as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker is a list of the form {type, index}.\nReadTrackerCoordinates[sim, tracker1, tracker2] gives the Cartesian components of the vector joining the two trackers as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker1 and tracker2 are lists of the form {type, index}.";
ReadTrackerVelocity::usage = "ReadTrackerVelocity[sim, tracker] gives a list of the Cartesian components of the velocity of the tracker as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker is a list of the form {type, index}.\nReadTrackerVelocity[sim, tracker1, tracker2] gives a list of the Cartesian components of the relative velocity of the two trackers as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker1 and tracker2 are lists of the form {type, index}.";
ReadTrackerRadius::usage = "ReadTrackerRadius[sim, tracker] gives the spherical polar radial coordinate of a tracker as a DataTable as a function of time.\nReadTrackerRadius[sim, tracker1, tracker2] gives the distance between the two trackers.";
ReadTrackerAzimuth::usage = "ReadTrackerAzimuth[sim, tracker] gives the spherical polar azimuthal coordinate (phi) of a tracker as a DataTable as a function of time.\nReadTrackerAzimuth[sim, tracker1, tracker2] gives the azimuthal angle of the vector joining the two trackers.";
ReadTrackerInclination::usage = "ReadTrackerInclination[sim, tracker] gives the spherical polar inclination coordinate (theta) of a tracker as a DataTable as a function of time.\nReadTrackerInclination[sim, tracker1, tracker2] gives the inclination angle of the vector joining the two trackers.";

Begin["`Private`"];

trackerPattern = {_String, (_Integer|_List)};

ReadTrackerCoordinates[run_String, tracker:trackerPattern] :=
  Symbol["SimulationTools`"<>tracker[[1]]<>"`Trackers`ReadCoordinates"][run, tracker[[2]]];

ReadTrackerCoordinates[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  ReadTrackerCoordinates[run, tracker1] - ReadTrackerCoordinates[run, tracker2];

ReadTrackerVelocity[run_String, tracker:trackerPattern] :=
 Module[{coords},
  coords = ReadTrackerCoordinates[run, tracker];
  Map[NDerivative[1], coords, {ArrayDepth[coords]}]
];

ReadTrackerVelocity[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
 Module[{coords},
  coords = ReadTrackerCoordinates[run, tracker1, tracker2];
  Map[NDerivative[1], coords, {ArrayDepth[coords]}]
];

ReadTrackerRadius[run_String, tracker:trackerPattern] :=
 Module[{coords},
  coords = ReadTrackerCoordinates[run, tracker];
  Map[Norm, coords, {ArrayDepth[coords]-1}]
];

ReadTrackerRadius[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
 Module[{coords},
  coords = ReadTrackerCoordinates[run, tracker1, tracker2];
  Map[Norm, coords, {ArrayDepth[coords]-1}]
];

xyToAzimuth[{x1_DataTable, y1_DataTable}] :=
  UnwrapPhaseVector[ArcTan@@RestrictedToCommonInterval[{x1,y1}]];

ReadTrackerAzimuth[run_String, tracker:trackerPattern] :=
 Module[{coords},
  coords = ReadTrackerCoordinates[run, tracker];
  Map[xyToAzimuth[#[[{1,2}]]]&, coords, {ArrayDepth[coords]-1}]
];

ReadTrackerAzimuth[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
 Module[{coords},
  coords = ReadTrackerCoordinates[run, tracker1, tracker2];
  Map[xyToAzimuth[#[[{1,2}]]]&, coords, {ArrayDepth[coords]-1}]
];

inclination[{x_DataTable, y_DataTable, z_DataTable}] :=
 Module[{R},
  R = {x,y,z};
  (* In BHCoordinates, we avoided infinities at the origin:
     MapData[If[Norm[#] > 10^-4, ArcCos[#[[3]]/Norm[#]], 0.0] &, rp]; *)
  ArcCos[z/Norm[R]]
];

ReadTrackerInclination[run_String, tracker:trackerPattern] :=
 Module[{coorda},
  coords = ReadTrackerCoordinates[run, tracker];
  Map[inclination, coords, {ArrayDepth[coords]-1}]
];

ReadTrackerInclination[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
 Module[{coorda},
  coords = ReadTrackerCoordinates[run, tracker1, tracker2];
  Map[inclination, coords, {ArrayDepth[coords]-1}]
];

End[];
EndPackage[];
