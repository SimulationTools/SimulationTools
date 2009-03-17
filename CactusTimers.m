
BeginPackage["CactusTimers`", {"NR`"}];

TimerPieChart;
IndependentTimers;
ParseTimersFile;
TimerScaling::usage = "TimerScaling[run1, run2, n1, n2, tName] returns the scaling of the given timer between runs run1 and run2, defined as (n2 t2) / (n1 t1).";

Begin["`Private`"];

stringToReal[s_]:=
  Module[{stream,x},
    stream=StringToStream[s];
    x=Read[stream,Real];
    Return[x]];

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

largestTimers[ts_,n_]:=
  Module[{sorted,topN,rest,restTime},
    sorted=Sort[ts,#1[[2]]>#2[[2]]&];
    topN=Take[sorted,n];
    rest=Drop[sorted,n];
    restTime=Apply[Plus,Map[Last,rest]];
    Return[Append[topN,{"Rest",restTime}]]];

independentTimers[timersFile_String]:=
  independentTimers[ParseTimersFile[timersFile]];

IndependentTimers[runName_String] :=
  Module[{},
    Return[independentTimers[parseTimersFile[FileInRun[runName, "AllTimers.0000.txt"]]]];
  ];

independentTimers[allTimers_List]:=
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

chartTimers[timers_List]:=
  Module[{},
    Return[PieChart[Map[Last,timers],ChartStyle->60, ChartLegends->Map[First,timers]]]];

isInitialTimer[{n_,v_}]:=
  StringMatchQ[n,"Virtual::Initial"]||
  StringMatchQ[n,"TwoPunctures:"~~__]||
  StringMatchQ[n,"Initialise::CallAnalysis::OutputGH"];

evolutionTimers[timers_]:=
  Select[timers,!isInitialTimer[#]&];

TimerScaling[run1_String, run2_String, n1_Integer, n2_Integer, tName_String] :=
  Module[{ts1, ts2, t1, t2, s},
    ts1 = IndependentTimers[run1];
    ts2 = IndependentTimers[run2];
    t1 = readTimer[ts1, tName];
    t2 = readTimer[ts2, tName];
    s = t2 * n2 / (n1 * t1);
    Return[s];
  ];

timerScaling[ts1_List,ts2_List,nFac_,tName_String]:=
  Module[{t1,t2,s},
    t1=readTimer[ts1,tName];
    t2=readTimer[ts2,tName];
    s=t2*nFac/(t1 );
    Return[s]];

TimerPieChart[runName_] :=
  TimerPieChart[runName, 20];

TimerPieChart[runName_String, n_Integer] :=
  Module[{timers},
    timers = independentTimers[ParseTimersFile[FileInRun[runName, "AllTimers.0000.txt"]]];
    Return[chartTimers[largestTimers[evolutionTimers[timers],n]]]];

End[];

EndPackage[];
