(* Mathematica Test File *)

RunDirectory = $NRMMATestRunDirectory;

TestReferenceDirectory = FileNameJoin[{FileNameDrop[FindFile["nrmma`"],-2],"Tests/TestReference"}];

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
