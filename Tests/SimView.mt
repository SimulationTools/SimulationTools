(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

(****************************************************************)
(* SimulationOverview                                           *)
(****************************************************************)

Test[
    Head[SimulationOverview[$SimulationToolsTestSimulation]]
    ,
    Grid
    ,
    TestID->"SimulationOverview"
]
