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
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Memo`",
  "SimulationTools`RunFiles`"
 }];

ReadTrackerCoordinates::usage = "ReadTrackerCoordinates[sim, tracker] gives the Cartesian coordinates of the tracker as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker is a list of the form {type, index}.\nReadTrackerCoordinates[sim, tracker1, tracker2] gives the Cartesian components of the vector joining the two trackers as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker1 and tracker2 are lists of the form {type, index}.";
ReadTrackerVelocity::usage = "ReadTrackerVelocity[sim, tracker] gives a list of the Cartesian components of the velocity of the tracker as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker is a list of the form {type, index}.\nReadTrackerVelocity[sim, tracker1, tracker2] gives a list of the Cartesian components of the relative velocity of the two trackers as a function of time.  The result is a list of DataTables, one for each coordinate direction. tracker1 and tracker2 are lists of the form {type, index}.";
ReadTrackerRadius::usage = "ReadTrackerRadius[sim, tracker] gives the spherical polar radial coordinate of a tracker as a DataTable as a function of time.\nReadTrackerRadius[sim, tracker1, tracker2] gives the distance between the two trackers.";
ReadTrackerAzimuth::usage = "ReadTrackerAzimuth[sim, tracker] gives the spherical polar azimuthal coordinate (phi) of a tracker as a DataTable as a function of time.\nReadTrackerAzimuth[sim, tracker1, tracker2] gives the azimuthal angle of the vector joining the two trackers.";
ReadTrackerInclination::usage = "ReadTrackerInclination[sim, tracker] gives the spherical polar inclination coordinate (theta) of a tracker as a DataTable as a function of time.\nReadTrackerInclination[sim, tracker1, tracker2] gives the inclination angle of the vector joining the two trackers.";

Begin["`Private`"];

trackerPattern = {_String, _Integer};

ReadTrackerCoordinates[run_String, tracker:{type_, i_}] :=
  Symbol["SimulationTools`"<>type<>"`Trackers`ReadCoordinates"][run, i];

ReadTrackerCoordinates[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  ReadTrackerCoordinates[run, tracker1] - ReadTrackerCoordinates[run, tracker2];

ReadTrackerVelocity[run_String, tracker:trackerPattern] :=
  NDerivative[1]/@ReadTrackerCoordinates[run, tracker];

ReadTrackerVelocity[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  NDerivative[1]/@ReadTrackerCoordinates[run, tracker1, tracker2];

ReadTrackerRadius[run_String, tracker:trackerPattern] :=
  Norm@ReadTrackerCoordinates[run, tracker];

ReadTrackerRadius[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  Norm@ReadTrackerCoordinates[run, tracker1, tracker2];

xyToAzimuth[{x1_DataTable, y1_DataTable}] :=
  UnwindPhaseVector[ArcTan[x1,y1]];

ReadTrackerAzimuth[run_String, tracker:trackerPattern] :=
  xyToAzimuth[ReadTrackerCoordinates[run, tracker][[{1,2}]]];

ReadTrackerAzimuth[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  xyToAzimuth[(ReadTrackerCoordinates[run, tracker1] - ReadTrackerCoordinates[run, tracker2])[[{1,2}]]];

ReadTrackerInclination[run_String, tracker:trackerPattern] :=
  Module[{R,x,y,z},
    R = {x,y,z} = ReadTrackerCoordinates[run, tracker];
    (* In BHCoordinates, we avoided infinities at the origin:
       MapData[If[Norm[#] > 10^-4, ArcCos[#[[3]]/Norm[#]], 0.0] &, rp]; *)
    ArcCos[z]/Norm[R]];

ReadTrackerInclination[run_String, tracker1:trackerPattern, tracker2:trackerPattern] :=
  Module[{R,x,y,z},
    R = {x,y,z} = ReadTrackerCoordinates[run, tracker1] - ReadTrackerCoordinates[run, tracker2];
    (* In BHCoordinates, we avoided infinities at the origin:
       MapData[If[Norm[#] > 10^-4, ArcCos[#[[3]]/Norm[#]], 0.0] &, rp]; *)
    ArcCos[z]/Norm[R]];

End[];
EndPackage[];
