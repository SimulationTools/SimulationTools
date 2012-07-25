(* Mathematica Test File *)

$SimulationPath = $NRMMATestSimulationDirectory;

(****************************************************************)
(* ReadTimeStep                                                 *)
(****************************************************************)

Test[
	ReadTimeStep[$NRMMATestSimulation]
	,
	0.5
	,
	TestID->"ReadTimeStep"
]

Test[
	ReadTimeStep[$NRMMATestSimulation, RefinementLevel -> 3]
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

