
(* This script should be run from the SimulationTools/Tests directory like this:

     math -run '<<RunTests.m' </dev/null

*)

SetOptions["stdout", PageWidth -> Infinity];

If[FindFile["MUnit`"] === $Failed,

If[FindFile["MUnitRunner`"] =!= $Failed,
   Get["MUnitRunner`"],
   (* else *)
   workbenchLocations = {"/Applications/Workbench.app/configuration/org.eclipse.osgi/bundles/13/1/.cp/MathematicaSourceVersioned/Head", "/Applications/Wolfram Workbench.app/configuration/org.eclipse.osgi/bundles/13/1/.cp/MathematicaSourceVersioned/Head"};
   AppendTo[$Path,
            Replace[Select[workbenchLocations, DirectoryQ],
                    {{}    :> (Print["Wolfram Workbench not found"]; Abort[]),
                     {x_, ___} :> x}]]];
  ];

<< MUnit`
<< SimulationTools`

tests = If[Length[$ScriptCommandLine] > 1,
           Drop[$ScriptCommandLine,1],
           Map[FileBaseName[#] &, Get["AllTests.mt"][[1]]]];

Print["tests = ",tests];

results = (Print["\n"]; TestRun[#<>".mt",
                                Loggers -> {VerbosePrintLogger[]},
                                TestRunTitle -> #]) & /@ tests;
Print[];

pass = Pick[tests, results];
fail = Pick[tests, Map[Not,results]];
Print[];
Print["Passing packages: ", StringJoin[Riffle[pass," "]]];
Print["Failing packages: ", StringJoin[Riffle[fail," "]]];
Print[];
If[Length[fail] === 0 && Length[pass] > 0,
   Print["All tests passed"];
   Quit[0],
   Print[Length[fail], " tests failed"];
  Quit[1]];
