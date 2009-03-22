
BeginPackage["Timers`", {"CactusTimers`", "RunFiles`"}];

(* Interface with CactusTimers package *)

ReadTimerFromRun;
ChartTimersFromRun;

Begin["`Private`"];

TimersFile[runName_] :=
 FileInRun[runName, "AllTimers.0000.txt"];

ReadTimerFromRun[runName_String, timerName_String] :=
  Module[{},
  ReadTimer[ParseTimersFileFromRun[runName], timerName]];

ParseTimersFileFromRun[runName_String] :=
  ParseTimersFile[TimersFile[runName]];

ChartTimersFromRun[runName_String] :=
  ChartTimers[TimersFile[runName]];

(* TimerScaling[run1_String, run2_String, n1_Integer, n2_Integer, tName_String] := *)
(*   Module[{ts1, ts2, t1, t2, s}, *)
(*     ts1 = IndependentTimers[run1]; *)
(*     ts2 = IndependentTimers[run2]; *)
(*     t1 = readTimer[ts1, tName]; *)
(*     t2 = readTimer[ts2, tName]; *)
(*     s = t2 * n2 / (n1 * t1); *)
(*     Return[s]; *)
(*   ]; *)

(* TimerPieChart[runName_] := *)
(*   TimerPieChart[runName, 20]; *)

(* TimerPieChart[runName_String, n_Integer] := *)
(*   Module[{timers}, *)
(*     timers = IndependentTimers[ParseTimersFile[FileInRun[runName, "AllTimers.0000.txt"]]]; *)
(*     Return[chartTimers[largestTimers[evolutionTimers[timers],n]]]]; *)

(* IndependentTimers[runName_String] := *)
(*   Module[{}, *)
(*     Return[IndependentTimers[ParseTimersFile[FileInRun[runName, "AllTimers.0000.txt"]]]]; *)
(*   ]; *)

End[];

EndPackage[];
