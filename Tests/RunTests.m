AppendTo[$Path, "/Applications/Wolfram Workbench.app/configuration/org.eclipse.osgi/bundles/13/1/.cp/MathematicaSourceVersioned/Head"];

<< MUnit`
<< SimulationTools`

tests = {
		"BHCoordinates",
		"CarpetHDF5",
		"DataAnalysis",
		"DataRegion",
		"DataRegionDeprecated",
		"DataTable",
		"DataTableNew",
		"GridFunctions",
		"Grids",
		"Memo",
		"NR",
		"RunFiles",
		"SimView",
		"Waveforms"
};

(Print["\n"]; TestRun[#<>".mt", Loggers -> {VerbosePrintLogger[]}, TestRunTitle -> #]) & /@ tests;


