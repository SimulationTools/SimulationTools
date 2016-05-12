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

BeginPackage["SimulationTools`Segments`",
 {
  "SimulationTools`ColumnFile`",
  "SimulationTools`RunFiles`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  If[$VersionNumber >= 10, "GeneralUtilities`", Unevaluated[Sequence[]]]
 }];

SegmentStartDate;
SegmentEndDate;
SegmentDuration;
RunDutyCycle;
ReadSimulationMeanWaitTime;
SegmentCoordinateTimeInterval;
SegmentStartTimes;
ReadSimulationSegmentCoordinateTimeIntervals;
SegmentProgress;
ReadSegmentProgress;
ReadSimulationSegmentProgress;
SegmentQueueDate;
SegmentQueueTime;
SimulationStateFractions;
ReadSegmentQueueTime;
ReadSimulationSegmentQueueTimes;

Begin["`Private`"];

SegmentEndDate[dir_] :=
 Module[{file},
  file = FileNameJoin[{dir, "/carpet-timing..asc"}];
  If[FileType[file] === None,
    file = FileNameJoin[{dir, "/carpet::timing..asc"}]];
  FileDate[file]
 ];

SegmentStartDate[dir_] :=
  Module[{parFile},
    parFile = First[FileNames["*.par", dir]];
    FileDate[parFile]];

SegmentDuration[dir_] :=
  DateDifference[SegmentStartDate[dir], SegmentEndDate[dir], "Second"][[1]];

RunDutyCycle[run_] :=
  Module[{segs = FindRunSegments[run], totalRunTime, totalElapsedTime},
    totalRunTime = Plus@@(SegmentDuration /@ segs);
    totalElapsedTime = DateDifference[SegmentStartDate[First[segs]],
                                      SegmentEndDate[Last[segs]], "Second"][[1]];
    totalRunTime / totalElapsedTime //N];

ReadSimulationMeanWaitTime[sim_String] :=
  Module[{segs = FindRunSegments[sim], totalRunTime, totalElapsedTime},
    totalRunTime = Plus@@(SegmentDuration /@ segs);
    totalElapsedTime = DateDifference[SegmentStartDate[First[segs]],
                                      SegmentEndDate[Last[segs]], "Second"][[1]];
    totalRunTime / totalElapsedTime //N];

ReadSimulationSegmentQueueTimes[sim_String] :=
  ReadSegmentQueueTime /@ FindRunSegments[sim];

ReadSegmentQueueTime[segment_String] :=
  Module[{outputDir, queueDate, runDate},
    (* This should really be in SimFactory.m *)
    (* This will only work with SimFactory 2, not SimFactory 3 *)
    outputDir = FileNameDrop[segment,-1];
    queueDate = FileDate[FileNameJoin[{outputDir, "SIMFACTORY", "SubmitScript"}]];
    runDate = FileDate[FileNameJoin[{outputDir, "SIMFACTORY", "RunScript"}]];
    DateDifference[queueDate,runDate, "Hour"]]

ReadSimulationSegmentCoordinateTimeIntervals[sim_String] :=
  Module[{segs},
    segs = FindRunSegments[sim];
    DeleteCases[SegmentCoordinateTimeInterval/@segs, None]];

SegmentCoordinateTimeInterval[segdir_] :=
 Module[{times, formalineFile},
   formalineFile = FileNameJoin[{segdir,"formaline-jar.txt"}];
   If[FileExistsQ[formalineFile],
     Replace[StringCases[Import[formalineFile, "String"], 
       StartOfLine ~~ "cctk_time=" ~~ n : Shortest[__] ~~ EndOfLine :> 
       ToExpression@n], {
         l:{__} :> {First[l], Last[l]},
         {} :> None,
         x_ :> Error["Unrecognised segment times: "<>ToString[x]]}],
     (* else *)
     times =
     Catch[First /@ ReadColumnFile[segdir, "carpet"~~("-"|"::")~~"timing..asc", {"time"}]];
     If[! ListQ[times], Return[None], Return[{times[[1]], times[[-1]]}]]]];

SegmentStartTimes[run_] :=
 Module[{segs = FindRunSegments[run]},
  First /@
   Select[SegmentCoordinateTimeInterval /@ segs, # =!= None &]];

SegmentStartTimes[run_] :=
 Module[{segs = FindRunSegments[run]},
  First /@
   Select[SegmentCoordinateTimeInterval /@ segs, # =!= None &]];

ReadSegmentProgress[seg_String] :=
  Module[{start, end, runtime},
    If[FindSimulationFiles[seg, "carpet"~~("-"|"::")~~"timing..asc"] === {}, 
      Return[ToDataTable[{}]]];
    start = AbsoluteTime[SegmentStartDate[seg]];
    end = AbsoluteTime[SegmentEndDate[seg]];
    runtime = 
    start + ToDataTable@
    ReadColumnFile[seg, 
      "carpet"~~("-"|"::")~~"timing..asc", {"time", "time_total"}];
    ToDataTable[ToListOfData[runtime], ToListOfCoordinates[runtime]]];

ReadSimulationSegmentProgress[sim_String] :=
  Select[ReadSegmentProgress /@ FindRunSegments[sim], Length[#] > 0 &];

SegmentQueueDate[seg_String] :=
  FileDate[FileNameJoin[{seg, "/../runscript.sh"}]];

SegmentQueueTime[seg_String] :=
  QuantityMagnitude[
    DateDifference[SegmentQueueDate[seg], SegmentStartDate[seg], 
      "Second"]];

SimulationStateFractions[sim_String] :=
  Module[{segs, queueDates, queueTimes, startDates, endDates, 
    simStartDate, simEndDate, simDuration, totalRunTime, 
    totalQueueTime, totalStoppedTime},
    segs = Select[FindRunSegments[sim], 
      FindSimulationFiles[#, "carpet"~~("-"|"::")~~"timing..asc"] =!= {} &];
    queueDates = AbsoluteTime /@ SegmentQueueDate /@ segs;
    queueTimes = SegmentQueueTime /@ segs;
    startDates = AbsoluteTime /@ SegmentStartDate /@ segs;
    endDates = AbsoluteTime /@ SegmentEndDate /@ segs;
    simStartDate = First[queueDates];
    simEndDate = Last[endDates];
    simDuration = simEndDate - simStartDate;
    totalRunTime = Total[endDates - startDates];
    totalQueueTime = Total[queueTimes];
    totalStoppedTime = simDuration - totalQueueTime - totalRunTime;
    Association["Running" -> N[totalRunTime/simDuration],
    "Queued" -> N[totalQueueTime/simDuration],
    "Stopped" -> N[totalStoppedTime/simDuration]]];

End[];
EndPackage[];
