(* Mathematica Test File *)

RunDirectory = $NRMMATestRunDirectory;

(****************************************************************)
(* ReadTimeStep                                                 *)
(****************************************************************)

Test[
	ReadTimeStep[$NRMMATestRun]
	,
	{0.5, 0.5, 0.25, 0.125, 0.0625, 0.03125, 0.015625, 0.0078125, 0.00390625, 0.001953125}
	,
	TestID->"ReadTimeStep"
]

Test[
	ReadTimeStep[$NRMMATestRun, 3]
	,
	0.125
	,
	TestID->"ReadTimeStep for refinement level"
]


(****************************************************************)
(* ReadGridSpacing                                              *)
(****************************************************************)

Test[
	ReadGridSpacing[$NRMMATestRun]
	,
	{2., 1., 0.5, 0.25, 0.125, 0.0625, 0.03125}
	,
	TestID->"ReadGridSpacing"
]

Test[
	ReadGridSpacing[$NRMMATestRun, 3]
	,
	0.25
	,
	TestID->"ReadGridSpacing for refinement level"
]


(****************************************************************)
(* ReadTimeRange                                                *)
(****************************************************************)

Test[
	ReadTimeRange[$NRMMATestRun]
	,
	{0, 300}
	,
	TestID->"ReadTimeRange"
]


(****************************************************************)
(* ReadMaxRefinementLevels                                      *)
(****************************************************************)

Test[
	ReadMaxRefinementLevels[$NRMMATestRun]
	,
	7
	,
	TestID->"ReadMaxRefinementLevels"
]

