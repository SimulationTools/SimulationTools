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
  12
  ,
  TestID->"ReadSimulationRunTime"
    ]
