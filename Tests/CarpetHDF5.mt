(* Mathematica Test File *)

RunDirectory = $NRMMATestRunDirectory;

var1D = "phi.x.h5";
var2D = "phi.xy.h5";
var3D = "phi.file_0.h5";

(* ReadIterations *)

Module[{},
	Test[
		ReadIterations[$NRMMATestRun, var3D]
	,
		{0, 8192, 16384}
	,
		TestID->"ReadIterations"
	]]
