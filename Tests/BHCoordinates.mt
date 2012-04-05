(* Mathematica Test File *)

RunDirectory = $NRMMATestRunDirectory;

TestReferenceDirectory = FileNameJoin[{FileNameDrop[FindFile["nrmma`"],-2],"Data/TestReference"}];

(* ReadBHCoordinates *)

Test[
  ToList[ReadBHCoordinates[$NRMMATestRun, 0]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHCoordinates-0.m"}]]
  ,
  TestID->"ReadBHCoordinates-0"
    ]

Test[
  ToList[ReadBHCoordinates[$NRMMATestRun, 1]][[{1,2,-1}]]
  ,
  {{0, {-3, 0, 0}},
   {0.5, {-2.99948598445407, -0.0115049097956219, -1.27623664921498*^-20}}, 
   {300, {-5.93161911032243*^-18, -2.88757513001964*^-17, 8.32639525962098*^-20}}}
  ,
  TestID->"ReadBHCoordinates-1"
    ]

Test[
  ToList[ReadBHCoordinate[$NRMMATestRun, 0, 1]][[{1,2,-1}]]
  ,
  {{0, 3}, {0.5, 2.99948598445407}, 
   {300, 5.93161911032243*^-18}},
  TestID->"ReadBHCoordinate"
    ]

Test[
  ReadBHTrajectory[$NRMMATestRun, 0][[{1,2,-1}]]
  ,
  {{3, 0}, {2.99948598445407, 0.0115049097956219}, 
   {5.93161911032243*^-18, 2.88757513001964*^-17}},
  TestID->"ReadBHTrajectory"
    ]

Test[
  ReadBHTrajectories[$NRMMATestRun][[All,{1,2,-1}]]
  ,
  {{{3, 0}, {2.99948598445407, 0.0115049097956219}, 
    {5.93161911032243*^-18, 2.88757513001964*^-17}}, 
   {{-3, 0}, {-2.99948598445407, -0.0115049097956219}, 
    {-5.93161911032243*^-18, -2.88757513001964*^-17}}},
  TestID->"ReadBHTrajectories"
    ]

Test[
  ToList[ReadBHSeparation[$NRMMATestRun]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHSeparation.m"}]]
  ,
  TestID->"ReadBHSeparation"
    ]

Test[
  ToList[ReadBHRadius[$NRMMATestRun,0]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHRadius.m"}]]
  ,
  TestID->"ReadBHRadius"
    ]

Test[
  ToList[ReadBHPhase[$NRMMATestRun,0]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHPhase-0.m"}]]
  ,
  TestID->"ReadBHPhase-0"
    ]

Test[
  ToList[ReadBHInclination[$NRMMATestRun]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHInclination.m"}]]
  ,
  TestID->"ReadBHInclination"
    ]

Test[
  ToList[ReadBHPhase[$NRMMATestRun]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHPhase.m"}]]
  ,
  TestID->"ReadBHPhase"
    ]

Test[
  ToList[ReadBHPhaseOfFrequency[$NRMMATestRun]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHPhaseOfFrequency.m"}]]
  ,
  TestID->"ReadBHPhaseOfFrequency"
    ]

Test[
  ToList[ReadBHSpeed[$NRMMATestRun,0]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHSpeed.m"}]]
  ,
  TestID->"ReadBHSpeed"
    ]

Test[
  BHCoordinateMergerTime[$NRMMATestRun]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"BHCoordinateMergerTime.m"}]]
  ,
  TestID->"BHCoordinateMergerTime"
    ]

Test[
  InitialSeparation[$NRMMATestRun]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"InitialSeparation.m"}]]
  ,
  TestID->"InitialSeparation"
    ]

Test[
  InitialPosition[$NRMMATestRun,0]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"InitialPosition.m"}]]
  ,
  TestID->"InitialPosition"
    ]
