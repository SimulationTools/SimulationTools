(* Mathematica Test File *)

$SimulationPath = $NRMMATestSimulationDirectory;

(****************************************************************)
(* ReadTimeStep                                                 *)
(****************************************************************)

Test[
	ReadTimeStep[$NRMMATestSimulation]
	,
	{0.5, 0.5, 0.25, 0.125, 0.0625, 0.03125, 0.015625, 0.0078125, 0.00390625, 0.001953125}
	,
	TestID->"ReadTimeStep"
]

Test[
	ReadTimeStep[$NRMMATestSimulation, 3]
	,
	0.125
	,
	TestID->"ReadTimeStep for refinement level"
]


(****************************************************************)
(* ReadGridSpacings                                             *)
(****************************************************************)

Test[
	ReadGridSpacings[$NRMMATestSimulation]
	,
	{2., 2., 2.}
	,
	TestID->"ReadGridSpacings"
]

Test[
	ReadGridSpacings[$NRMMATestSimulation, RefinementLevel -> 3]
	,
	{0.25, 0.25, 0.25}
	,
	TestID->"ReadGridSpacings for refinement level"
]


(****************************************************************)
(* ReadTimeRange                                                *)
(****************************************************************)

Test[
	ReadTimeRange[$NRMMATestSimulation]
	,
	{0, 300}
	,
	TestID->"ReadTimeRange"
]


(****************************************************************)
(* ReadMaxRefinementLevels                                      *)
(****************************************************************)

Test[
	ReadMaxRefinementLevels[$NRMMATestSimulation]
	,
	7
	,
	TestID->"ReadMaxRefinementLevels"
]

