
BeginPackage["Timers`", {"CactusTimers`", "RunFiles`", "Memo`"}];

(* Interface with CactusTimers package *)

ReadTimerFromRun;
ChartTimersFromRun;
TimersFile;
IndependentTimersFromRun;
ParseTimersFileFromRun;
ChartEvolutionTimersFromRun;
CollectTimers;

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

TimersFilesInRun[runName_] :=
  Module[{},
    files = FindRunFilesFromPattern[runName, "AllTimers.*.txt"]
  ];

padInteger[i_, n_] :=
  Module[{str},
   str = ToString[i];
   StringJoin[Table["0", {a, n - StringLength[str]}]] <> str];

TimersFile[runName_, i_, segment_:1] :=
  Module[{segments, fileName},
    fileName = "AllTimers."<> padInteger[i, 6] <> ".txt";
    segments = FindRunFile[runName, fileName];
    If[Length[segments] === 0, Throw["Cannot find timers file " <> fileName <> " in run " <> runName]];
    If[segment > Length[segments], Throw["Cannot find " <> ToString[segment] <> " segments in run " <> runName]];
    segments[[segment]]];

ReadTimerFromRun[runName_String, timerName_String] :=
  Module[{},
  ReadTimer[ParseTimersFileFromRun[runName], timerName]];

DefineMemoFunction[ParseTimersFileFromRun[runName_String],
  ParseTimersFile[TimersFile[runName]]];

ChartTimersFromRun[runName_String] :=
  ChartTimers[TimersFile[runName]];

ChartTimersFromRun[runName_String, core_:0, segment_Integer:1] :=
  ChartTimers[TimersFile[runName, core, segment]];

IndependentTimersFromRun[runName_String] :=
  Module[{},
    Return[IndependentTimers[ParseTimersFileFromRun[runName]]];
  ];

ChartEvolutionTimersFromRun[runName_, nTimers_:20 (* ,core_:0,segment_:1 *)] :=
 Module[{},
  ChartTimers[
   LargestTimers[EvolutionTimers[IndependentTimersFromRun[runName]],
    nTimers]]]

collectTimers1[timers_, collectionName_ -> (members_List)] :=
 Module[{cts, ctsDel, tSum, tRep},
  cts = Select[timers, MemberQ[members, #[[1]]] &];
  ctsDel = Complement[timers, cts];
  tSum = Apply[Plus, Map[Last, cts]];
  tRep = ctsDel~Join~{{collectionName, tSum}};
  Return[tRep]
  ]

CollectTimers[timers_, cols_List] :=
 If[Length[cols] === 0, timers,
  CollectTimers[collectTimers1[timers, First[cols]], Drop[cols, 1]]]

End[];

EndPackage[];
