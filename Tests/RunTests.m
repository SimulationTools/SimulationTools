AppendTo[$Path, "/Applications/Wolfram Workbench.app/configuration/org.eclipse.osgi/bundles/13/1/.cp/MathematicaSourceVersioned/Head"];

<< MUnit`
<< SimulationTools`

tests = {
  "BHCoordinates",
  "CarpetHDF5",
  "DataRegion",
  "DataRegionDeprecated",
  "DataTable",
  "DataTableNew",
  "GridFunctions",
  "Memo",
  "NR",
  "RunFiles",
  "Waveforms"
};

(Print["\n"]; TestRun[#<>".mt", Loggers -> {VerbosePrintLogger[]}, TestRunTitle -> #]) & /@ tests;


