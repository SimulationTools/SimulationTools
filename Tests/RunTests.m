AppendTo[$Path, "/Applications/Wolfram Workbench.app/configuration/org.eclipse.osgi/bundles/13/1/.cp/MathematicaSourceVersioned/Head"];

SetOptions["stdout", PageWidth -> Infinity];

<< MUnit`
<< SimulationTools`

tests = Map[FileBaseName[#] &, FileNames["*.mt"]];

results = (Print["\n"]; TestRun[#<>".mt", Loggers -> {VerbosePrintLogger[]}, TestRunTitle -> #]) & /@ tests;
Print[];

pass = Pick[tests, results];
fail = Pick[tests, Map[Not,results]];

Print["Passing packages: ", pass];
Print["Failing packages: ", fail];
