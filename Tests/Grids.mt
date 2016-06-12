Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

(* Mathematica Test File *)

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

(****************************************************************)
(* ReadTimeStep                                                 *)
(****************************************************************)

VerificationTest[
	ReadTimeStep[$SimulationToolsTestSimulation]
	,
	0.5
	,
	TestID->"ReadTimeStep"
]

VerificationTest[
	ReadTimeStep[$SimulationToolsTestSimulation, RefinementLevel -> 3]
	,
	0.125
	,
	TestID->"ReadTimeStep for refinement level"
]


(****************************************************************)
(* ReadGridSpacings                                             *)
(****************************************************************)

VerificationTest[
	ReadGridSpacings[$SimulationToolsTestSimulation]
	,
	{2., 2., 2.}
	,
	TestID->"ReadGridSpacings"
]

VerificationTest[
	ReadGridSpacings[$SimulationToolsTestSimulation, RefinementLevel -> 3]
	,
	{0.25, 0.25, 0.25}
	,
	TestID->"ReadGridSpacings for refinement level"
]


(****************************************************************)
(* ReadTimeRange                                                *)
(****************************************************************)

VerificationTest[
	ReadTimeRange[$SimulationToolsTestSimulation]
	,
	{0, 300}
	,
	TestID->"ReadTimeRange"
]


(****************************************************************)
(* ReadMaxRefinementLevels                                      *)
(****************************************************************)

VerificationTest[
	ReadMaxRefinementLevels[$SimulationToolsTestSimulation]
	,
	7
	,
	TestID->"ReadMaxRefinementLevels"
]

