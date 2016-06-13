
(* This script should be run from the SimulationTools/Tests directory like this:

     math -run '<<RunTests.m' </dev/null

*)

(* workbenchLocations = {"/Applications/Workbench.app/configuration/org.eclipse.osgi/bundles/13/1/.cp/MathematicaSourceVersioned/Head", "/Applications/Wolfram Workbench.app/configuration/org.eclipse.osgi/bundles/13/1/.cp/MathematicaSourceVersioned/Head"}; *)

(* AppendTo[$Path, *)
(*   Replace[Select[workbenchLocations, DirectoryQ], *)
(*     {{}    :> (Print["Wolfram Workbench not found"]; Abort[]), *)
(*       {x_, ___} :> x}]]; *)


$allowedMessages = {};

testMessageHandlerArgs[x_] :=
  If[Last[x] && !MemberQ[$allowedMessages,x[[1,1]]],
     x /. HoldPattern[_[Message[id_, args___], _]] :>
     (Print["Caught messages"];
      Print[ToString@StringForm[id,args], {}];
      Abort[])];

Internal`AddHandler["Message", testMessageHandlerArgs];

SetOptions["stdout", PageWidth -> Infinity];

$Path = Join[{"../.."},$Path];

Print["Running tests for SimulationTools found at ", FindFile["SimulationTools`"]];

<< MUnitRunner`
<< SimulationTools`

tests = Map[FileBaseName[#] &, FileNames["*.wlt"]];


myLogger[] :=
  With[{logger = Unique["RunTests`Private`logger"]},
    Module[{wasAbortedOrFatal = False},
      logger /: LogMessage[logger, msg_String] :=
      Sow[TestMessage[msg], TestLog];

      logger /: LogFatal[logger, msg_String] :=
      Sow[TestFatal[msg], TestLog];

      logger /: LogSuccess[logger, tr_?TestResultQ] :=
      Sow[TestSuccess[tr], TestLog];

      logger /: LogFailure[logger, tr_?TestResultQ] :=
      Sow[TestFailure[tr], TestLog];

      logger /: LogError[logger, tr_] :=
      Sow[TestError[tr,TestLog]];

      logger /: LogWasAborted[logger, wasAborted_] :=
      (If[wasAborted, wasAbortedOrFatal = True]);

      logger /: LogWasFatal[logger, wasFatal_] :=
      (If[wasFatal, wasAbortedOrFatal = True]);

      logger /: LogEnd[logger, testCnt_, (*successCnt*)_, failCnt_, msgFailCnt_, skippedTestCnt_, errorCnt_, (*abort*)_] :=
      (
        If[wasAbortedOrFatal, Sow[TestInterrupted,TestLog]];
        Sow[TestCounts[testCnt,failCnt,msgFailCnt,skippedTestCnt,errorCnt], TestLog]);
      logger]]


(* TODO: don't abort all tests if one of them raises an Error *)

(* TODO: report all possible results in JUnit *)

(* TODO: tidy up the way the logger produces its results *)

(* MUnit should deal with messages while the tests are running *)
Internal`RemoveHandler["Message", testMessageHandlerArgs];

fullResults = (Print["\n"]; Reap[TestRun[#<>".wlt", Loggers -> {VerbosePrintLogger[], myLogger[]}, TestRunTitle -> #], TestLog]) & /@ tests;

(* Restore our handler *)
Internal`AddHandler["Message", testMessageHandlerArgs];

results = fullResults[[All,1]];

(* WriteJUnit[package_ -> results_] := *)
(*   Map[ *)
(*     XMLElement["testcase",  *)
(*       {"name"->TestID[#], *)
(*         "classname"->package}, *)
(*       If[FailureMode[#]==="Failure",{XMLElement["failure",{},{}]},{}]] &, Cases[results[[1]],r:(_TestSuccess|_TestFailure|_TestError):>r[[1]]]]; *)

WriteJUnitPackage[package_ -> results_] :=
  Map[
    WriteJUnitTestCase[#, package] &,
    Cases[results[[1]],r:(_TestSuccess|_TestFailure|_TestError):>r[[1]]]];

WriteJUnit[results_] :=
  XMLElement["testsuite", {},
    Flatten[Map[WriteJUnitPackage, results],1]];

WriteJUnitTestCase[result_, package_] :=
  XMLElement["testcase", 
    {"name"->TestID[result],
     "classname"->package},
    If[FailureMode[result]==="Failure",
      {XMLElement["failure",{},WriteJUnitFailureMessage[result]]},{}]];

WriteJUnitFailureMessage[tr_] :=
  Module[{str, msg = TestFailureMessage[tr]},
    str = 
    StringJoin[Riffle[
      {"Test number " <> ToString[TestIndex[tr], OutputForm] <>
        " with TestID " <> ToString[TestID[tr], OutputForm] <> " had a failure.",
        "Input: " <> ToString[TestInput[tr], InputForm],
        "Expected output: " <> ToString[ExpectedOutput[tr], InputForm],
        "Actual output: " <> ToString[ActualOutput[tr], InputForm],
        If[msg =!= "", "** " <> ToString[msg] <> " **", ""]}, "\n"]];
    {XMLObject["CDATASection"][str]}];

Print[];

(* Print[Thread[tests->fullResults[[All,2]]]]; *)

xml=WriteJUnit[Thread[tests->fullResults[[All,2]]]];

Export["../../testresults.xml", xml, "XML"];

pass = Pick[tests, results];
fail = Pick[tests, Map[Not,results]];

Print["Passing packages: ", pass];
Print["Failing packages: ", fail];
