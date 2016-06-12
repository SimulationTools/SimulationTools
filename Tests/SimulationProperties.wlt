Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

(* ReadSimulationCoreCount *)

VerificationTest[
  ReadSimulationCoreCount[$SimulationToolsTestSimulation]
  ,
  12
  ,
  TestID->"ReadSimulationCoreCount"
    ]



(* ReadSimulationRunTime *)

VerificationTest[
  ReadSimulationRunTime[$SimulationToolsTestSimulation]
  ,
  18776.927876949347
  ,
  TestID->"ReadSimulationRunTime"
    ]
