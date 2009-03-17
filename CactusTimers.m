
BeginPackage["CactusTimers`"];

ChartTimers::usage = "ChartTimers[timers] gives a pie chart of the timers.  Note that it only makes sense if the timers are independent.\nChartTimers[filename, n] gives a pie chart of the top n independent timers in file 'filename'.  If omitted, n defaults to 15.";

IndependentTimers::usage = "IndependentTimers[timers] converts a list of raw Cactus timers into a set of timers whose values add to the total run time.\nIndependentTimers[filename] computes the independent timers directly from a timer file.";

ParseTimersFile::usage = "ParseTimersFile[filename] reads a timer file produced using the output_all_timers parameter of TimerReport and returns a list of the form {{timer, value}, ...}.  It takes the last entry in the timer file, corresponding to the full time of the simulation.";

LargestTimers::usage = "LargestTimers[timers, n] gives a sorted list of the n - 1 largest timers.  Appended to the end of this list is an entry called 'Rest' which is the sum of the timers which were not included.";

TotalTime::usage = "TotalTime[timers] gives the total time from a set of independent timers.  Note that if the set is not independent, the output is meaningless.";

TimerScaling::usage = "TimerScaling[{timers1, timers2, ...}, {n1, n2, ...}, timername] returns a table of {n, s} pairs where n is the number of processors and s = (t n) / (n1 t1) where t is the time measured by timer 'timername'.";

ListTimers::usage = "ListTimers[timers] gives a list of all the timer names";
FindTimers::usage = "FindTimers[timers, pattern] gives a list of the timer names which match pattern.";

Begin["`Private`"];

(* Internal functions *)

stringToReal[s_]:=
  Module[{stream,x},
    stream=StringToStream[s];
    x=Read[stream,Real];
    Return[x]];

isScheduleTimer[n_]:=
  !StringMatchQ[n,"Evolve::"~~__]&&
  !StringMatchQ[n,"Initialise::"~~__]&&
  !StringMatchQ[n,"CallFunction::"~~__]&&
  !StringMatchQ[n,"Evolve"]&&!StringMatchQ[n,"Initialise"]&&
  !StringMatchQ[n,"CallFunction"]&&!StringMatchQ[n,"CCTK total time"]&&
  !StringMatchQ[n,"Shutdown"]&&!StringMatchQ[n,"Time this run"];

isScheduleTimer[n_]:=
  StringMatchQ[n,"["~~__];

virtualTimers[ts_]:=
  Module[{readTimer,timerExists,tInitial,tRestrict,tRegrid,virtualTimers,regridTimer},

    timerExists[n_]:=
      Module[{tNames},tNames=Map[First,ts];
        MemberQ[tNames,n]];

    readTimer[n_]:=
      Module[{},
        If[!timerExists[n],
          Throw["Timer "<>n<>" not found"]];
        Select[ts,(#[[1]]===n)&][[1]][[2]]];

    tInitial=readTimer["Initialise::CallRegridInitialMeta"]-readTimer["Initialise::CallRegridInitialMeta::CCTK_PREREGRIDINITIAL"];
    tRestrict=readTimer["Evolve::CallRestrict"];

    tRegrid=readTimer["Evolve::CallRegrid"]-(readTimer["Evolve::CallRegrid::CCTK_PREREGRID"]+readTimer["Evolve::CallRegrid::CCTK_POSTREGRID"]);

    virtualTimers=
      {{"Virtual::Initial",tInitial},
       {"Virtual::Restrict",tRestrict},
       {"Virtual::Regrid",tRegrid}};

    Return[virtualTimers]];

isIndependentNonScheduleTimer[{n_,v_}]:=
  MemberQ[{"Initialise::CallAnalysis::OutputGH","Evolve::CallAnalysis::OutputGH"},n];


timerExists[ts_,n_]:=
  Module[{tNames},
    tNames=Map[First,ts];
    MemberQ[tNames,n]];

readTimer[ts_List,n_String]:=
  Module[{},
    If[!timerExists[n],
      Throw["Timer "<>n<>" not found"]];
    Select[ts,(#[[1]]===n)&][[1]][[2]]];

isInitialTimer[{n_,v_}]:=
  StringMatchQ[n,"Virtual::Initial"]||
  StringMatchQ[n,"TwoPunctures:"~~__]||
  StringMatchQ[n,"Initialise::CallAnalysis::OutputGH"];

evolutionTimers[timers_]:=
  Select[timers,!isInitialTimer[#]&];

(* Public functions *)

ParseTimersFile[fileName_]:=
  Module[{timers,column,parseLine,lines, ts, col, return, isColumnDesc, columnDescs, tList, maxCol = 0, timerVals},
    parseLine[l_]:=
      Module[{stream,words,return},
        stream=StringToStream[l];
        words=ReadList[stream,Word];

        If[words[[1]]=="#" && words[[2]]=="Column",
          colNo = ToExpression[words[[3]]];
          column[Evaluate[colNo]]=StringJoin[Riffle[Drop[words,3]," "]];
          maxCol=Max[colNo,maxCol]];

        Close[stream];
        ];

    If[FileType[fileName] === None,
      Throw["File " <> fileName <> " not found"]];

    lines=ReadList[fileName,String];

    isColumnDesc[l_] := StringMatchQ[l, "# Column *"];

    columnDescs = Select[lines, isColumnDesc];

    Scan[parseLine, columnDescs];
    timerVals=Map[stringToReal,StringSplit[Last[lines]]];

    If[Length[timerVals] != maxCol,
      timerVals=Map[stringToReal,StringSplit[lines[[-2]]]];
        If[Length[timerVals] != maxCol,
          Throw["Timers file " <> fileName <> " might be corrupt; there are not enough columns in the output"]]];

    tList = Table[{column[c],timerVals[[c]]},{c,3,Length[timerVals]}];

    Return[tList];

    ];

ListTimers[ts_] := Map[First, ts];

FindTimers[ts_, pattern_] := Map[First, Select[ts, StringMatchQ[#[[1]], pattern] &]];

LargestTimers[ts_,n_]:=
  Module[{sorted,topN,rest,restTime},
    sorted=Sort[ts,#1[[2]]>#2[[2]]&];
    topN=Take[sorted,n];
    rest=Drop[sorted,n];
    restTime=Apply[Plus,Map[Last,rest]];
    Return[Append[topN,{"Rest",restTime}]]];

TotalTime[ts_List] :=
  Module[{},
    timerNames = Map[First, ts];
    If[MemberQ[timerNames, "CCTK total time"],
      Throw["The TotalTime function adds together all the timers given to it.  It does not make sense to give it a list of timers which includes CCTK total time."]];
    Apply[Plus, Map[Last, ts]]];

IndependentTimers[timersFile_String]:=
  IndependentTimers[ParseTimersFile[timersFile]];

IndependentTimers[allTimers_List]:=
  Module[{scheduleTimers,nonScheduleTimers,vTimers,totalTime,scheduleTime,
          virtualTime,deficit,independentTimers,insTimers,insTime},
    scheduleTimers=Map[{StringReplace[StringDrop[#[[1]],7]," in "~~__->""],#[[2]]}&,Select[allTimers,isScheduleTimer[#[[1]]]&]];
    nonScheduleTimers=Select[allTimers,!isScheduleTimer[#[[1]]]&];
    vTimers=virtualTimers[nonScheduleTimers];
    insTimers=Select[allTimers,isIndependentNonScheduleTimer];
    totalTime=readTimer[allTimers,"CCTK total time"];
    scheduleTime=Apply[Plus,Map[Last,scheduleTimers]];
    virtualTime=Apply[Plus,Map[Last,vTimers]];
    insTime=Apply[Plus,Map[Last,insTimers]];
    deficit=totalTime-scheduleTime-virtualTime-insTime;
    independentTimers=Append[Join[scheduleTimers,vTimers,insTimers],{"Unaccounted for",deficit}];
    Return[independentTimers]];

ChartTimers[timers_List]:=
  Module[{total = TotalTime[timers]},
    Return[PieChart[Map[Last, timers], ChartStyle->60, ChartLegends -> Map[(#[[1]] <> " (" <> ToString[#[[2]]] <> " secs, " <> ToString[NumberForm[100 #[[2]]/total, 4]]<> "%)") &, timers]]]];

ChartTimers[timersFile_String, n_Integer] :=
  ChartTimers[LargestTimers[IndependentTimers[timersFile], n]];

ChartTimers[timersFile_String] :=
  ChartTimers[LargestTimers[IndependentTimers[timersFile], 20]];

TimerScaling[ts1_List,ts2_List,nFac_,tName_String]:=
  Module[{t1,t2,s},
    t1=readTimer[ts1,tName];
    t2=readTimer[ts2,tName];
    s=t2*nFac/(t1 );
    Return[s]];

TimerScaling[tss:{_List...}, nFacs_List, tName_String] :=
  Module[{t1,t2,s},
    If[Sort[nFacs] =!= nFacs,
      Throw["TimerScaling: nFacs should be ascending"]];

    t1 = readTimer[tss[[1]], tName];
    n1 = nFacs[[1]];

    Return[MapThread[{#1, #1 readTimer[#2,tName] / (t1 n1)} &, {nFacs, tss}]]];

(* Interface with NR system *)

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
