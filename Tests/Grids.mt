(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

(****************************************************************)
(* ReadTimeStep                                                 *)
(****************************************************************)

Test[
	ReadTimeStep[$SimulationToolsTestSimulation]
	,
	0.5
	,
	TestID->"ReadTimeStep"
]

Test[
	ReadTimeStep[$SimulationToolsTestSimulation, RefinementLevel -> 3]
	,
	0.125
	,
	TestID->"ReadTimeStep for refinement level"
]


(****************************************************************)
(* ReadGridSpacings                                             *)
(****************************************************************)

Test[
	ReadGridSpacings[$SimulationToolsTestSimulation]
	,
	{2., 2., 2.}
	,
	TestID->"ReadGridSpacings"
]

Test[
	ReadGridSpacings[$SimulationToolsTestSimulation, RefinementLevel -> 3]
	,
	{0.25, 0.25, 0.25}
	,
	TestID->"ReadGridSpacings for refinement level"
]


(****************************************************************)
(* ReadTimeRange                                                *)
(****************************************************************)

Test[
	ReadTimeRange[$SimulationToolsTestSimulation]
	,
	{0, 300}
	,
	TestID->"ReadTimeRange"
]


(****************************************************************)
(* ReadMaxRefinementLevels                                      *)
(****************************************************************)

Test[
	ReadMaxRefinementLevels[$SimulationToolsTestSimulation]
	,
	7
	,
	TestID->"ReadMaxRefinementLevels"
]

