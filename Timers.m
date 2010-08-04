
BeginPackage["Timers`", {"CactusTimers`", "RunFiles`", "Memo`"}];

(* Interface with CactusTimers package *)

ReadTimerFromRun;
ChartTimersFromRun;
TimersFile;
IndependentTimersFromRun;
ParseTimersFileFromRun;
CTGammaTimerCollections =
{"CTGamma RHS" -> {"CTGEvolution: CTGEvolution_CalcRHS_detg",
    "CTGGauge: ShiftGammaDriver", "CTGGauge: LapseBonaMasso"},
  "CTGamma SYNC" -> {"CTGEvolution: CTGEvolution_SelectBoundaries",
    "CTGConstraints: CTGConstraints_SelectBoundaries",
    "CTGGauge: CTGGauge_SelectBoundaries_GammaDriver",
    "CTGGauge: CTGGauge_SelectBoundaries_Lapse"} ,
  "CTGamma radiative bc" -> {"CTGEvolution: \
CTGEvolution_ApplyRadiativeBC",
    "CTGGauge: CTGGauge_ApplyRadiativeBC_GammaDriver"}};

Begin["`Private`"];

TimersFile[runName_] :=
 FindRunFile[runName, "AllTimers.000000.txt"][[1]];

ReadTimerFromRun[runName_String, timerName_String] :=
  Module[{},
  ReadTimer[ParseTimersFileFromRun[runName], timerName]];

DefineMemoFunction[ParseTimersFileFromRun[runName_String],
  ParseTimersFile[TimersFile[runName]]];

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

IndependentTimersFromRun[runName_String] :=
  Module[{},
    Return[IndependentTimers[ParseTimersFileFromRun[runName]]];
  ];

End[];

EndPackage[];
