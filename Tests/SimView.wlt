Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

(****************************************************************)
(* SimulationOverview                                           *)
(****************************************************************)

VerificationTest[
    Head[SimulationOverview[$SimulationToolsTestSimulation]]
    ,
    Grid
    ,
    TestID->"SimulationOverview"
]
