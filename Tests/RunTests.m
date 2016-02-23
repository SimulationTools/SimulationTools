
(* This script should be run from the SimulationTools/Tests directory like this:

     math -run '<<RunTests.m' </dev/null

*)

workbenchLocations = {"/Applications/Workbench.app/configuration/org.eclipse.osgi/bundles/13/1/.cp/MathematicaSourceVersioned/Head", "/Applications/Wolfram Workbench.app/configuration/org.eclipse.osgi/bundles/13/1/.cp/MathematicaSourceVersioned/Head"};

AppendTo[$Path,
  Replace[Select[workbenchLocations, DirectoryQ],
    {{}    :> (Print["Wolfram Workbench not found"]; Abort[]),
      {x_, ___} :> x}]];

SetOptions["stdout", PageWidth -> Infinity];

<< MUnit`
<< SimulationTools`

tests = Map[FileBaseName[#] &, DeleteCases[FileNames["*.mt"],"SimulationTools.mt"]];

results = (Print["\n"]; TestRun[#<>".mt", Loggers -> {VerbosePrintLogger[]}, TestRunTitle -> #]) & /@ tests;
Print[];

pass = Pick[tests, results];
fail = Pick[tests, Map[Not,results]];

Print["Passing packages: ", pass];
Print["Failing packages: ", fail];
