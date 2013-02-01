(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

(* ReadSimulationCoreCount *)

Test[
  ReadSimulationCoreCount[$SimulationToolsTestSimulation]
  ,
  12
  ,
  TestID->"ReadSimulationCoreCount"
    ]



(* ReadSimulationRunTime *)

Test[
  ReadSimulationRunTime[$SimulationToolsTestSimulation]
  ,
  18776.927876949347
  ,
  TestID->"ReadSimulationRunTime"
    ]
