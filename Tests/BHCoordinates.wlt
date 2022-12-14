Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

TestReferenceDirectory = FileNameJoin[{$SimulationToolsInstallationDirectory,"Data/TestReference"}];

(* ReadBHCoordinates *)

VerificationTest[
  ToList[ReadBHCoordinates[$SimulationToolsTestSimulation, 0]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHCoordinates-0.m"}]]
  ,
  TestID->"ReadBHCoordinates-0"
    ]

VerificationTest[
  ToList[ReadBHCoordinates[$SimulationToolsTestSimulation, 1]][[{1,2,-1}]]
  ,
  {{0, {-3, 0, 0}},
   {0.5, {-2.99948598445407, -0.0115049097956219, -1.27623664921498*^-20}}, 
   {300, {-5.93161911032243*^-18, -2.88757513001964*^-17, 8.32639525962098*^-20}}}
  ,
  TestID->"ReadBHCoordinates-1"
    ]

VerificationTest[
  ToList[ReadBHCoordinate[$SimulationToolsTestSimulation, 0, 1]][[{1,2,-1}]]
  ,
  {{0, 3}, {0.5, 2.99948598445407}, 
   {300, 5.93161911032243*^-18}},
  TestID->"ReadBHCoordinate"
    ]

VerificationTest[
  ReadBHTrajectory[$SimulationToolsTestSimulation, 0][[{1,2,-1}]]
  ,
  {{3, 0}, {2.99948598445407, 0.0115049097956219}, 
   {5.93161911032243*^-18, 2.88757513001964*^-17}},
  TestID->"ReadBHTrajectory"
    ]

VerificationTest[
  ReadBHTrajectories[$SimulationToolsTestSimulation][[All,{1,2,-1}]]
  ,
  {{{3, 0}, {2.99948598445407, 0.0115049097956219}, 
    {5.93161911032243*^-18, 2.88757513001964*^-17}}, 
   {{-3, 0}, {-2.99948598445407, -0.0115049097956219}, 
    {-5.93161911032243*^-18, -2.88757513001964*^-17}}},
  TestID->"ReadBHTrajectories"
    ]

VerificationTest[
  ToList[ReadBHSeparation[$SimulationToolsTestSimulation]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHSeparation.m"}]]
  ,
  TestID->"ReadBHSeparation"
    ]

VerificationTest[
  ToList[ReadBHRadius[$SimulationToolsTestSimulation,0]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHRadius.m"}]]
  ,
  TestID->"ReadBHRadius"
    ]

VerificationTest[
  ToList[ReadBHPhase[$SimulationToolsTestSimulation,0]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHPhase-0.m"}]]
  ,
  TestID->"ReadBHPhase-0"
    ]

VerificationTest[
  ToList[ReadBHInclination[$SimulationToolsTestSimulation]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHInclination.m"}]]
  ,
  TestID->"ReadBHInclination"
    ]

VerificationTest[
  ToList[ReadBHPhase[$SimulationToolsTestSimulation]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHPhase.m"}]]
  ,
  TestID->"ReadBHPhase"
    ]

VerificationTest[
  ToList[ReadBHPhaseOfFrequency[$SimulationToolsTestSimulation]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHPhaseOfFrequency.m"}]]
  ,
  TestID->"ReadBHPhaseOfFrequency"
    ]

VerificationTest[
  ToList[ReadBHSpeed[$SimulationToolsTestSimulation,0]][[{1,2,-1}]]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"ReadBHSpeed.m"}]]
  ,
  TestID->"ReadBHSpeed"
    ]

VerificationTest[
  BHCoordinateMergerTime[$SimulationToolsTestSimulation]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"BHCoordinateMergerTime.m"}]]
  ,
  TestID->"BHCoordinateMergerTime"
    ]

VerificationTest[
  InitialSeparation[$SimulationToolsTestSimulation]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"InitialSeparation.m"}]]
  ,
  TestID->"InitialSeparation"
    ]

VerificationTest[
  InitialPosition[$SimulationToolsTestSimulation,0]
  ,
  Get[FileNameJoin[{TestReferenceDirectory,"InitialPosition.m"}]]
  ,
  TestID->"InitialPosition"
    ]
