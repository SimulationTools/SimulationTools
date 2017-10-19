(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["SimulationTools`WaveformExtrapolation`", {
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  "SimulationTools`Plotting`",
  "SimulationTools`Utils`",
  "SimulationTools`Waveforms`"}];

WaveformExtrapolationAnalysis;

Begin["`Private`"];

retardedWaveforms[waveforms_, rads_, MADM_, schRads_] :=
  rads MapThread[
    ToSchwarzschildRetardedTime[#1, #2, MADM] &, {schRads, waveforms}];

retardedWaveformPlot[waveforms_, rads_] :=
 PresentationListLinePlot[Re /@ (waveforms), PlotLegend -> rads, 
  FrameLabel -> {"(t-\!\(\*SuperscriptBox[\(r\), \(*\)]\))/M", 
    "Waveform"}, PlotLabel -> "Waveform at different radii", 
  LegendPosition -> {Right, Top}];

retardedFrequencyPlot[freqs_, rads_] :=
 PresentationListLinePlot[freqs, PlotLegend -> rads, 
  LegendPosition -> {Right, Top}, 
  FrameLabel -> {"(t-\!\(\*SuperscriptBox[\(r\), \(*\)]\))/M", 
    "\!\(\*SuperscriptBox[\(\[Omega]\), \(22\)]\)"}, 
  PlotLabel -> "Frequency at different radii"];

interpolateAtCoordinate[ds_List, tCheck_] :=
 #[tCheck] & /@ 
  Interpolation /@ ds;

phaseExtrapolationFitPlot[rads : {_?NumberQ ..}, 
  phis : {_?NumberQ ..}, tCheck_?NumberQ] :=
 Module[{a, b, c, d, y},
  FindFitPlot[
   Thread[{1/rads, 
     phis}], {{a + b y, {a, b}}, {a + b y + c y^2, {a, b, c}},
    {a + b y + c y^2 + d y^3, {a, b, c, d}}}, y, 
   PlotLegend -> {1, 2, 3},
   PlotRange -> {{0, All}, {Min[phis] - 0.1, Max[phis] + 0.1}}, 
   FitFunctionRange -> {0, 1}, 
   PlotLabel -> 
    "Phase at t-\!\(\*SuperscriptBox[\(r\), \(*\)]\) = " <> 
     ToString@tCheck <> " M", 
   FrameLabel -> {"M/\!\(\*SubscriptBox[\(r\), \(Sch\)]\)", "\[Phi]"}]]

amplitudeExtrapolationFitPlot[rads : {_?NumberQ ..}, 
  amps : {_?NumberQ ..}, tCheck_?NumberQ] :=
 Module[{a, b, c, d, y}, 
  FindFitPlot[
   Thread[{1/rads, 
     amps/First[amps]}], {{a + b y, {a, b}}, {a + b y + c y^2, {a, b, 
      c}},
    {a + b y + c y^2 + d y^3, {a, b, c, d}}}, y, 
   PlotLegend -> {1, 2, 3}, PlotRange -> {{0, All}, {0.95, 1.05}}, 
   FitFunctionRange -> {0, 1}, 
   PlotLabel -> 
    "Amplitude at t-\!\(\*SuperscriptBox[\(r\), \(*\)]\) = " <> 
     ToString@tCheck <> " M", 
   FrameLabel -> {"M/\!\(\*SubscriptBox[\(r\), \(Sch\)]\)", 
     "A/\!\(\*SubscriptBox[\(A\), \(min\)]\)"}]];
                                                   

extrapolatedWaveformPlot[extrapWaveform_] :=
 PresentationListLinePlot[extrapWaveform, 
  FrameLabel -> {"(t-\!\(\*SuperscriptBox[\(r\), \(*\)]\))/M", 
    "Waveform"}, PlotLabel -> "Extrapolated waveform", 
  LegendPosition -> {Right, Top}]

extrapolatedAmplitudePlot[extrapAmps_] :=
 PresentationListLinePlot[extrapAmps, 
  FrameLabel -> {"(t-\!\(\*SuperscriptBox[\(r\), \(*\)]\))/M", "A"}, 
  PlotLabel -> "Extrapolated amplitude", 
  LegendPosition -> {Right, Top}]

extrapolatedFrequencyPlot[freq_] :=
 PresentationListLinePlot[MovingAverage[freq, 1], 
  LegendPosition -> {Right, Top}, 
  FrameLabel -> {"(t-\!\(\*SuperscriptBox[\(r\), \(*\)]\))/M", 
    "\!\(\*SuperscriptBox[\(\[Omega]\), \(22\)]\)"}, 
  PlotLabel -> "Extrapolated frequency"];



phaseExtrapolationErrorPlot[extrapPhases_] :=
 PresentationListLinePlot[
  Log10 /@ Abs /@ 
    MovingAverageOperator[16] /@ Differences[extrapPhases], 
  PlotRange -> All, 
  FrameLabel -> {"(t-\!\(\*SuperscriptBox[\(r\), \(*\)]\))/M", 
    "\!\(\*SubscriptBox[\(log\), \(10\)]\)|\[CapitalDelta]\[Phi]|"}, 
  PlotLegend -> {"\!\(\*SubscriptBox[\(\[Phi]\), \
\(1\)]\)-\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)", 
    "\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)-\!\(\*SubscriptBox[\(\
\[Phi]\), \(3\)]\)"}, LegendPosition -> {Right, Top}, 
  PlotLabel -> "Phase extrapolation error", Axes -> {None, True}];

amplitudeExtrapolationErrorPlot[extrapAmps_] := 
 PresentationListLinePlot[
  Log10 /@ Abs /@ 
    MovingAverageOperator[
      8] /@ (Differences[extrapAmps]/Drop[extrapAmps, 1]), 
  PlotRange -> All, 
  FrameLabel -> {"(t-\!\(\*SuperscriptBox[\(r\), \(*\)]\))/M", 
    "\!\(\*SubscriptBox[\(log\), \(10\)]\)|\[CapitalDelta]A/A|"}, 
  PlotLegend -> {"\!\(\*SubscriptBox[\(A\), \
\(1\)]\)-\!\(\*SubscriptBox[\(A\), \(2\)]\)", 
    "\!\(\*SubscriptBox[\(A\), \(2\)]\)-\!\(\*SubscriptBox[\(A\), \(3\
\)]\)"}, LegendPosition -> {Right, Top}, 
  PlotLabel -> "Amplitude extrapolation error", Axes -> {None, True}];
                                                    
Options[WaveformExtrapolationAnalysis] =
  {"PhaseOrder" -> 1,
   "AmplitudeOrder" -> 2};

WaveformExtrapolationAnalysis[rads_, waveforms_, schRads_, MADM_, OptionsPattern[]] :=
 Module[{scaledRetardedWaveformsAtRadii, waveformPlot, freqPlot, 
   ampCheckPlot, scaledRetardedAmpsAtRadiiCheck, phaseCheckPlot, 
   tCheck = 100, retardedPhasesAtRadiiCheck, retardedPhasesAtRadii,
   extrapWaveformPlot, ords, ord, extrapPhases, extrapAmps, 
   extrapPhase, extrapAmp, extrapWaveform, extrapAmpPlot, 
   extrapFreqPlot, ampErrPlot, phaseErrPlot, plotGrid, padGraphics},
  
  (* Waveforms *)
  
  scaledRetardedWaveformsAtRadii = 
   retardedWaveforms[waveforms, rads, MADM, schRads];
  retardedPhasesAtRadii = 
   AlignPhases[Phase /@ scaledRetardedWaveformsAtRadii, 100];
  waveformPlot = 
   retardedWaveformPlot[scaledRetardedWaveformsAtRadii, rads];
  freqPlot = 
   retardedFrequencyPlot[
    Frequency /@ scaledRetardedWaveformsAtRadii, rads];
  
  (* Single point check *)
  
  scaledRetardedAmpsAtRadiiCheck = 
   interpolateAtCoordinate[Abs /@ scaledRetardedWaveformsAtRadii, 
    tCheck];
  ampCheckPlot = 
   amplitudeExtrapolationFitPlot[rads, scaledRetardedAmpsAtRadiiCheck,
     tCheck];
  retardedPhasesAtRadiiCheck = 
   interpolateAtCoordinate[retardedPhasesAtRadii, tCheck];
  phaseCheckPlot = 
   phaseExtrapolationFitPlot[rads, retardedPhasesAtRadiiCheck, tCheck];
  
  (* Extrapolation *)
  phaseOrds = OptionValue[PhaseOrder] + {0, 1, 2};

  extrapPhases = 
   Table[RadialExtrapolation[{rads, 
      ResampleDataTables[retardedPhasesAtRadii]}, ord], {ord, phaseOrds}]; 

  ampOrds = OptionValue[AmplitudeOrder] + {0, 1, 2, 3};
  extrapAmps = 
   Table[RadialExtrapolation[{rads, 
      ResampleDataTables[Abs /@ scaledRetardedWaveformsAtRadii]}, 
     ord], {ord, ampOrds}];

  extrapPhase = extrapPhases[[1]]; 
  extrapAmp = extrapAmps[[1]]; 
  extrapWaveform = extrapAmp Exp[I extrapPhase];
  
  (* Extrapolated waveform plots *)
  
  extrapWaveformPlot = extrapolatedWaveformPlot[Re@extrapWaveform];
  extrapAmpPlot = extrapolatedAmplitudePlot[extrapAmps];
  extrapFreqPlot = extrapolatedFrequencyPlot[Frequency@extrapWaveform];
  
  (* Extrapolation errors plots *)
  
  ampErrPlot = amplitudeExtrapolationErrorPlot[extrapAmps];
  phaseErrPlot = phaseExtrapolationErrorPlot[extrapPhases];
  
  padGraphics[gs_List] :=
   If[$FrontEnd === Null,
     (* Cannot use Rasterize without a frontend *)
     gs,
     PadGraphics[gs]];

  plotGrid = 
   GraphicsGrid[
    PartitionComplete[
     padGraphics[{waveformPlot, freqPlot, ampCheckPlot, 
       phaseCheckPlot, extrapWaveformPlot, extrapAmpPlot, 
       extrapFreqPlot, ampErrPlot, phaseErrPlot}], 2], 
    ImageSize -> 800(*, 
    PlotLabel -> Style["Extrapolation analysis", 16]*)];
  
  Association["PlotGrid" -> plotGrid,
   "ExtrapolatedWaveform" -> extrapWaveform,
   "ExtrapolatedAmplitudes" -> extrapAmps,
   "AmplitudeOrders" -> ampOrds,
   "PhaseExtrapolationError" -> Differences[extrapPhases][[1]],
   "AmplitudeExtrapolationError" -> Differences[extrapAmps][[1]]]];

End[];
EndPackage[];
