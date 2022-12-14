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

BeginPackage["SimulationTools`Plotting`",
 {
  "SimulationTools`DataRegion`",
  "SimulationTools`DataRepresentations`",
  "SimulationTools`DataTable`",
  "SimulationTools`Error`",
  If[$VersionNumber >= 10, "GeneralUtilities`", Unevaluated[Sequence[]]]
 }];

(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

DynamicListLinePlot(*::usage = "DynamicListLinePlot[args] is a wrapper around ListLinePlot which adds the ability to zoom into a region of the plot by dragging a box with the mouse.  Double-click to return to the previous zoom level and right-click to show a button-bar with various options including resetting the plot range to All or Automatic, joining the points in the plot, or using a log scale."*);
PresentationListLinePlot(*::usage = "PresentationListLinePlot[args] is a wrapper around ListLinePlot which uses a plot style designed to be more attractive than the default Mathematica plot style.  A plot legend can be added by giving the option PlotLegend -> {set1, set2, ...} where seti are labels for each of the curves in the plot."*);
PresentationArrayPlot(*::usage = "PresentationArrayPlot[d] plots a 2-dimensional DataRegion using ArrayPlot.  The data range of the plot is determined automatically from the DataRegion.  The plot style is designed to be more attractive than the default Mathematica plot style."*);
PresentationArrayPlot2;

(****************************************************************)
(* Experimental                                                 *)
(****************************************************************)

PadGraphics;

(****************************************************************)
(* Deprecated                                                   *)
(****************************************************************)

MakePlotLegend;
ListLinePlotWithLegend;
ListLogLinearPlotWithLegend;
PlotLegend;
LegendPosition;
ColorRange;
ColorMap;
PresentationPlotStyles;
PresentationPlotColors;
LegendOrientation;
LegendLineSize;
PlotFit(*::usage = "PlotFit[data, model, pars, var] takes the same arguments as FindFit and returns a plot of the fitted function as well as the result of the fit."*);
PlotKey;
DynamicArrayPlot;
FitPlot(*::usage = "FitPlot[data, model, pars, {t, t1, t2}] generates a ListLinePlot of data and a fit (using FindFit) to model with parameters pars for the variable t in the range t1 to t2.  The parameters to FitPlot are analogous to those of FindFit."*);
FilterPlot(*::usage = "FilterPlot[data, out, om, {t1, t2}] generates a ListLinePlot of data and applies a discrete cosine transform filter to a portion between t1 and t2 (using FilterDCT) cutting off frequencies above om.  t1, t2 and om can be varied in the resulting plot.  When the Update button is pressed, the filtered data is stored in out."*);
PlotKeySize;
DynamicShow;
LegendBackground;
ScaledColorFunction;
ColorMapLegend;
QuickSlicePlot;
RasterizeManipulate;
FindFitPlot;

Begin["`Private`"];

point = Graphics[Point[{0, 0}]];

plotStyles = 
  {Black, Blue, Darker[Magenta], Darker[Green], Orange, Gray, 
   Darker[Red], Darker[Yellow]};

PresentationPlotColors = Flatten[ConstantArray[{Black, Blue, Darker[Magenta], Darker[Green], Orange, Gray, 
   Darker[Red], Darker[Yellow], Darker[Blue], Darker[Gray]},10],1];

PresentationPlotStyles = Flatten[ConstantArray[Map[Directive[#,AbsoluteThickness[1]]&,PresentationPlotColors],10],1];

withStyle[elem_, style_List] :=
  Append[style, elem];

withStyle[elem_, style_] :=
  {style, elem};

Options[MakePlotLegend] = {"LegendOrientation" -> Vertical, "LegendLineSize" -> 40,
                           "LegendBackground" -> None};

MakePlotLegend[labels_List, style1_List : plotStyles, labelStyle_ : Automatic,
               opts:OptionsPattern[]] :=
  Module[{l = Line[{{-1, 0}, {1, 0}}], style,
      hor = OptionValue[LegendOrientation] === Horizontal},
    If[labels === {}, Return[Grid[{}]]];
    style =
      If[Length[style1] >= Length[labels],
         style1,
         PadLeft[style1, Length[labels], style1]];
      Grid[
        If[hor,List,Identity][Flatten[MapThread[Function[{lab,sty},
                   {Graphics[{
                              withStyle[l, sty]},
                          ImageSize -> OptionValue[LegendLineSize], AspectRatio -> 1/3],
                 Style[lab, If[labelStyle === Automatic, {},labelStyle]]}],
                {labels, Take[style, Length[labels]]}],If[hor,1,0]]],
      Alignment -> {Left,Center},
      Spacings-> {{0,Automatic}~Join~Flatten[Table[{1,Automatic},{i,Length[labels]}],1]},
        Background->OptionValue[LegendBackground]]];

styleInListLinePlot[lp_] :=
  Module[{},
    Cases[lp, {styles___, _Line} :> {styles}, Infinity]];

Options[ListLinePlotWithLegend] =
  Join[Options[ListLinePlot], {"PlotLegend" -> {}, "LegendPosition" -> {Left, Top},
                               "LegendBackground" -> None}, Options[MakePlotLegend]];

ListLinePlotWithLegend[data_, opts:OptionsPattern[]] :=
  Module[{style, pos, posx, posy, offset, scale, labelStyle, f, nLines},
    style = OptionValue[PlotStyle];
    nLines = Replace[data,
      {{_DataTable..} :> Length[data],
        _DataTable :> 1,
        _ :> All}];
    pos = OptionValue[LegendPosition];
    {posx, posy} = pos;
    f = 0.05;
    offset = 10 {Switch[posx, Right, -1, Left, 1, Center, 0, _, Error["Unknown position"]], If[posy === Top, -1, 1]};
    scale = {Switch[posx, Right, 1-f, Left, f, Center, 1, _, Error["Unknown position"]], If[posy === Bottom, f, 1-f]};
    labelStyle = OptionValue[LabelStyle];
    ListLinePlot[data,
      FilterRules[{opts}, Options[ListLinePlot]],
      Epilog -> 
        Inset[
          MakePlotLegend[Take[OptionValue[PlotLegend], If[nLines=!=All,Min[Length[OptionValue[PlotLegend]],nLines],All]], Take[If[!ListQ[style], {style}, style], nLines], labelStyle,
          FilterRules[{opts}, Options[MakePlotLegend]]],
          Scaled[scale], pos]]];

Options[PresentationListLinePlot] = Options[ListLinePlotWithLegend];
PresentationListLinePlot[data_, opts:OptionsPattern[]] :=
    Module[{data2},
      data2 = If[$VersionNumber < 10 && data === {}, {Undefined[]}, data];
      ListLinePlotWithLegend[data2, opts,
        PlotStyle -> PresentationPlotStyles, LabelStyle->"Medium", Frame->True]];

(*
Options[ListLogLinearPlotWithLegend] = 
  Join[Options[ListLogLinearPlot], 
       {PlotLegend -> {}, LegendPosition -> {Right, Bottom}}];

ListLogLinearPlotWithLegend[args___, opts:OptionsPattern[]] :=
  Module[{style, legend, pos, posx, posy, offset, scale, labelStyle},
    style = styleInListLinePlot[
      ListLogLinearPlot[args, FilterRules[{opts}, Options[ListLogLinearPlot]]]];
    labelStyle = If[OptionValue[LabelStyle] === {}, Automatic, OptionValue[LabelStyle]];
    legend = MakePlotLegend[OptionValue[PlotLegend], If[Length[First[{args}]] === 1, {style}, style], labelStyle];
    pos = OptionValue[LegendPosition];
    {posx, posy} = pos;
    offset = 10 {If[posx === Right, -1, 1], If[posy === Top, -1, 1]};
    scale = {If[posx === Right, 1, 0], If[posy === Bottom, 0, 1]};
    ListLogLinearPlot[args,
      FilterRules[{opts}, Options[ListLinePlot]],
      Epilog -> 
        Inset[legend, Offset[offset, Scaled[scale]], pos]]];
*)

(*
ProcPlot[data_, ns_, opts___] :=
 Module[{},
  ListLogLinearPlot[data, opts, PlotStyle -> styles, 
   PlotMarkers -> {point}, Joined -> True,
   Ticks -> {Table[2^i, {i, 0, Log[2, Last[ns]]}], Automatic}, 
   LabelStyle -> Medium]];

ProcPlot2[data : {{_, _} ...}, opts___] :=
 ProcPlot2[{data}, opts];

ProcPlot2[ds : {{{_, _} ...} ...}, opts___] :=
 Module[{nss, n1, n2, ticks},
  nss = Flatten[Map[Map[First, #] &, ds]];
  n1 = Min[nss];
  n2 = Max[nss];
  ticks = Table[2^i, {i, 0, Log[2, n2]}];
  ListLogLinearPlot[ds, opts,
   ImageSize -> 400,
   PlotStyle -> styles, PlotRange -> {{n1/2., n2*2}, Automatic}, 
   PlotMarkers -> {point}, Joined -> True,
   Ticks -> {ticks, Automatic},
   LabelStyle -> Medium]]

ProcPlot2[ds : {_DataTable ...}, opts___] :=
 ProcPlot2[Map[ToList, ds], opts];

ProcPlot2[d_DataTable, opts___] :=
 ProcPlot2[{d}, opts];

*)

coordsToRanges[{p1x_, p1y_}, {p2x_, p2y_}] :=
 {{Min[p1x, p2x], 
   Max[p1x, p2x]}, {Min[p1y, p2y], Max[p1y, p2y]}};

DynamicListLinePlot[data_, opts___] :=
 DynamicModule[{dragging, p1, p2, plotRange, history, inDouble, 
   doubleStart, logScale, joined, controls, delta, mp, 
   doubleTime = 0.5},
  dragging = False;
  p1 = Null;
  p2 = Null;
  plotRange = 
   If[(PlotRange /. {opts}) === PlotRange, Automatic, 
    PlotRange /. {opts}];
  history = {};
  inDouble = False;
  doubleStart = AbsoluteTime[];
  logScale = False;
  controls = False;
  joined = If[(Joined /. {opts}) === Joined, True, Joined /. {opts}];
  Column[{
    Dynamic[If[controls,
      Row[{Button["All", plotRange = All, ImageSize -> 100],
        Button["Automatic", plotRange = Automatic, ImageSize -> 100],
        Labeled[Checkbox[Dynamic[logScale]], Text["Log scale"], Right,
          Alignment -> Center],
        Labeled[Checkbox[Dynamic[joined]], Text["Joined"], Right, 
         Alignment -> Center]}], ""]],
    EventHandler[
     Dynamic[Show[
       If[logScale, ListLogPlot, ListPlot][data, 
        PlotRange -> plotRange, Joined -> joined, opts], 
       If[dragging, Graphics[{Opacity[0.1], Rectangle[p1, p2]}], {}], 
       ImageSize -> {350, Automatic}]],
     {{"MouseDown", 2} :> (controls = ! controls),
      "MouseClicked" :>
       (delta = AbsoluteTime[] - doubleStart;
        If[! inDouble || delta > doubleTime,
         inDouble = True; doubleStart = AbsoluteTime[],
         inDouble = False;
         If[delta < doubleTime,
          If[Length[history] >= 1,
           plotRange = Last[history];
           history = Drop[history, -1]]]]),
      "MouseDragged" :>
       (If[! dragging,
         dragging = True;
         p1 = MousePosition["Graphics"]];
        p2 = MousePosition["Graphics"]),
      "MouseUp" :>
       (If[dragging,
         dragging = False;
         history = Append[history, plotRange];
         plotRange = coordsToRanges[p1, p2]])},
     PassEventsDown -> False],
    
    Dynamic[
     If[controls, mp = MousePosition["Graphics"]; 
      If[mp =!= None, 
       Text["(" <> ToString[mp[[1]]] <> ", " <> ToString[mp[[2]]] <> 
         ")"], ""], ""]]
    
    }]];

(* Should make this code in common with the above *)
DynamicArrayPlot[data_, opts___] :=
 DynamicModule[{dragging, p1, p2, plotRange, history, inDouble,
   doubleStart, logScale, controls, delta, mp,
   doubleTime = 0.5},
  dragging = False;
  p1 = Null;
  p2 = Null;
  plotRange =
   If[(PlotRange /. {opts}) === PlotRange, Automatic,
    PlotRange /. {opts}];
  history = {};
  inDouble = False;
  doubleStart = AbsoluteTime[];
  logScale = False;
  controls = False;
  Column[{
    Dynamic[If[controls,
      Row[{Button["All", plotRange = All, ImageSize -> 100],
        Button["Automatic", plotRange = Automatic, ImageSize -> 100],
        Labeled[Checkbox[Dynamic[logScale]], Text["Log scale"], Right,
          Alignment -> Center]}], ""]],
    EventHandler[
     Dynamic[Show[
       ArrayPlot[If[logScale, Log10, Identity][data],
        PlotRange -> plotRange, opts],
       If[dragging, Graphics[{Opacity[0.1], Rectangle[p1, p2]}], {}],
       ImageSize -> {350, Automatic}]],
     {{"MouseDown", 2} :> (controls = ! controls),
      "MouseClicked" :>
       (delta = AbsoluteTime[] - doubleStart;
        If[! inDouble || delta > doubleTime,
         inDouble = True; doubleStart = AbsoluteTime[],
         inDouble = False;
         If[delta < doubleTime,
          If[Length[history] >= 1,
           plotRange = Last[history];
           history = Drop[history, -1]]]]),
      "MouseDragged" :>
       (If[! dragging,
         dragging = True;
         p1 = MousePosition["Graphics"]];
        p2 = MousePosition["Graphics"]),
      "MouseUp" :>
       (If[dragging,
         dragging = False;
         history = Append[history, plotRange];
         plotRange = coordsToRanges[p1, p2]])},
     PassEventsDown -> False],

    Dynamic[
     If[controls, mp = MousePosition["Graphics"];
      If[mp =!= None,
       Text["(" <> ToString[mp[[1]]] <> ", " <> ToString[mp[[2]]] <>
         ")"], ""], ""]]

    }]];



DynamicShow[graphics_, opts___] :=
 DynamicModule[{dragging, p1, p2, plotRange, history, inDouble,
   doubleStart, logScale, joined, controls, delta, mp,
   doubleTime = 0.5},
  dragging = False;
  p1 = Null;
  p2 = Null;
  plotRange =
   If[(PlotRange /. {opts}) === PlotRange, Automatic,
    PlotRange /. {opts}];
  history = {};
  inDouble = False;
  doubleStart = AbsoluteTime[];
  logScale = False;
  controls = False;
  joined = If[(Joined /. {opts}) === Joined, True, Joined /. {opts}];
  Column[{
    Dynamic[If[controls,
      Row[{Button["All", plotRange = All, ImageSize -> 100],
        Button["Automatic", plotRange = Automatic, ImageSize -> 100],
        Labeled[Checkbox[Dynamic[logScale]], Text["Log scale"], Right,
          Alignment -> Center],
        Labeled[Checkbox[Dynamic[joined]], Text["Joined"], Right,
         Alignment -> Center]}], Row[{}]]],
    EventHandler[
     Dynamic[Show[
       graphics,
       Graphics[If[dragging,{Opacity[0.1], Rectangle[p1, p2]},{}]],
       ImageSize -> {350, Automatic}, PlotRange -> plotRange, opts]],
     {{"MouseDown", 2} :> (controls = ! controls),
      "MouseClicked" :>
       (delta = AbsoluteTime[] - doubleStart;
        If[! inDouble || delta > doubleTime,
         inDouble = True; doubleStart = AbsoluteTime[],
         inDouble = False;
         If[delta < doubleTime,
          If[Length[history] >= 1,
           plotRange = Last[history];
           history = Drop[history, -1]]]]),
      "MouseDragged" :>
       (If[! dragging,
         dragging = True;
         p1 = MousePosition["Graphics"]];
        p2 = MousePosition["Graphics"]),
      "MouseUp" :>
       (If[dragging,
         dragging = False;
         history = Append[history, plotRange];
         plotRange = coordsToRanges[p1, p2]])},
     PassEventsDown -> False],

    Dynamic[
     If[controls, mp = MousePosition["Graphics"];
      If[mp =!= None,
       Text["(" <> ToString[mp[[1]]] <> ", " <> ToString[mp[[2]]] <>
         ")"], ""], ""]]

    }]];

PlotFit[data_, model_, pars_, var_, args___] :=
 Module[{fit, fittedModel},
  fit = FindFit[data, model, pars, var, MaxIterations -> 10000];
  fittedModel = model /. fit;
  {Show[ListPlot[data, PlotRange -> All],
    Plot[fittedModel, {h, 0, Max[First /@ data]}, PlotRange -> {{0,0.6},{-50,-40}}],
    PlotLabel -> First[pars] /. fit, args, ImageSize -> 400,PlotRange->{{0,0.6},{-50,-40}}], fit}];

FitPlot[data_DataTable, model_, pars_, {t_, t1p_, t2p_}, opts___] :=
 DynamicModule[
  {tMax = DataTableRange[data][[2]],
   dMin = Min[DepVar[data]],
   dMax = Max[DepVar[data]]},
  Manipulate[
   Module[
    {fit = Quiet[Check[
        FindFit[ToList@DataTableInterval[data, {t1, t2}], model,
         pars, t],
        Map[If[ListQ[#], #[[1]], #] -> 0 &, pars], {FindFit::cvmit, 
         FindFit::sszero}], {FindFit::cvmit, FindFit::sszero}]},
    Show[
     DataTableListLinePlot[data, opts],
     Plot[Evaluate[model /. fit], {t, 0, tMax}, PlotStyle -> Red],
     Graphics[{{Dashed, Line[{{t1, dMin}, {t1, dMax}}], 
        Line[{{t2, dMin}, {t2, dMax}}]}}],
     PlotLabel -> Column[fit]]],
   {{t1, t1p}, 0, tMax}, {{t2, t2p}, 0, tMax},
   SaveDefinitions -> True]];

key[mapName_String, {min_, max_}, plotkeysize_,opts___] :=
  ArrayPlot[Table[{c, c}, {c, max, min, -(max - min)/100}],
   opts,
   ColorFunctionScaling -> False,
   ColorFunction ->
    ScaledColorFunction[mapName, 1 {min, max}],
   DataRange -> {{0, 1}, {min, max}}, AspectRatio -> 8, ImageSize-> plotkeysize,
   FrameTicks -> {Table[
      If[Abs[c] < 10^-15, 0, N@c], {c, min, max, (max - min)/10}],
     False, False, False}];

Options[PresentationArrayPlot] = {
  "ColorRange" -> Automatic,
  "ColorMap" -> "TemperatureMap",
  "PlotKeySize" -> {Automatic,400},
  "PlotKey" -> True} ~Join~ Options[ArrayPlot];

DocumentationBuilder`OptionDescriptions["PresentationArrayPlot"] = {
  "ColorRange" -> "{a,b} to indicate the data values corresponding to the endpoints of the color range, or Automatic to determine this from the available data",
  "ColorMap" -> "the named color scheme (used with ColorData) to use for the plot",
  "PlotKeySize" -> "{w,h} to indicate the width and height of the plot key",
  "PlotKey" -> "whether to display a plot key"};

minNone[d_DataRegion] :=
  Min[Flatten[DeleteCases[ToListOfData[d], None, Infinity]]];

maxNone[d_DataRegion] :=
  Max[Flatten[DeleteCases[ToListOfData[d], None, Infinity]]];

PresentationArrayPlot[data_DataRegion, opts:OptionsPattern[]] :=
  Module[{range, keyPlot, plotkeysize, plot},
    range = OptionValue[ColorRange];
	plotkeysize = OptionValue[PlotKeySize];

    If[range === Automatic,
       range = {minNone[data], maxNone[data]};
       If[range[[1]] === range[[2]],
          range[[2]] = range[[1]]+0.1]];

    keyPlot = key[OptionValue[ColorMap],range,plotkeysize];

    plot = ArrayPlot[data, Sequence@@FilterRules[{opts},Options[ArrayPlot]],
             ImageSize->{400,400}, ColorFunctionScaling -> False,
             ColorFunction -> ScaledColorFunction[OptionValue[ColorMap], range],
             FrameTicks -> True, LabelStyle -> Medium];
    If[OptionValue[PlotKey],
      Row[{plot,keyPlot}],
      plot]];

SetAttributes[FilterPlot, HoldAll];

FilterPlot[data_, data2p_, omCutOff0_, {t10_, t20_}, opts___] :=
 DynamicModule[{tMin, tMax, omMax, data2},
  {tMin, tMax} = DataTableRange[data];
  omMax = Length[data]*2 Pi/(tMax - tMin)/4;
  (* The very high frequencies are never useful, so we go to omMax/4 *)
  data2p = {};
  Manipulate[
   Module[{nModes, T, om0},
    T = t2 - t1 + 100;
    om0 = 2 Pi/T;
    nModes = Floor[omCutOff/om0];
    data2 = FilterDCT[data, nModes, {t1 - 50, t2 + 50}, {t1, t2}];
    If[data2p === {}, data2p = data2];
    Show[DataTableListLinePlot[{data, data2}, 
      PlotStyle -> {LightGray, Darker[Blue]}, opts], 
     Graphics[{{Dashed, Line[{{t1, -1000}, {t1, 1000}}], 
        Line[{{t2, -1000}, {t2, 1000}}]}}]]], {{omCutOff, omCutOff0}, 
    0, omMax}, {{t1, t10}, 0, tMax}, {{t2, t20}, 0, tMax},
   Button["Update " <> ToString[Unevaluated[data2p]], 
    data2p = data2],
   SaveDefinitions -> True, ContinuousAction -> False]];


(* Deprecated functions *)

ScaledColorFunction[name_, {min_, max_}] :=
 (ColorData[name][(# - min)/(max - min)] &);

ColorMapLegend[colorFn_, {min_, max_, dx_: Automatic}] :=
 ArrayPlot[
  Table[{c, c}, {c, min, max, 
     (max - min)/100.}], 
  DataRange -> {{0, 0.1}, {min, max}}, ColorFunction -> colorFn, 
  FrameTicks -> {{None, 
     Table[x, {x, min, max, If[dx === Automatic, (max - min)/10., dx]}]}, {None, None}}, ImageSize->{60,300}];

QuickSlicePlot[v_DataRegion, {min_, max_}, colorMap_: "TemperatureMap", opts___] :=
 Module[{cf},
  cf = ScaledColorFunction[colorMap, {min, max}];
  GraphicsGrid[{{
     ArrayPlot[v, FrameTicks -> True, FrameLabel -> {"y", "x"},
       ColorFunction -> cf, ImageSize->300,opts], ColorMapLegend[cf, {min, max}]}}]
];


(* PresentationArrayPlot2 (EXPERIMENTAL) *)

(* This is work-in-progress for replacing PresentationArrayPlot.  It's
   main advantage is that it returns a Graphics object which can be
   combined with others using Show.  It does this by making the colour
   bar an inset of the plot rather than an external object which needs
   to be combined using Row, which is not a Graphics, and cannot be
   used in Show. *)

Options[key2] = Options[DensityPlot];

key2[{min_, max_}, opts : OptionsPattern[]] :=
 DensityPlot[y, {x, 0, 1}, {y, min, max}, Frame -> True, 
  PlotRangePadding -> 0, FrameTicks -> {{All, None}, {None, None}}, 
  AspectRatio -> Full, opts]

Options[PresentationArrayPlot2] =
 Join[
  {"ValueRange" -> Automatic, 
   "ColorScheme" -> "LightTemperatureMap", "PlotLegend" -> True, 
   "LegendPosition" -> {Right, Top}}, Options[ArrayPlot]];

PresentationArrayPlot2[data_, opts : OptionsPattern[]] :=
 Module[{range, plot, legend, plotOpts},
  range = Switch[OptionValue[ValueRange],
    Automatic, {minNone[data], maxNone[data]},
    {_, _}, OptionValue[ValueRange],
    _, Error[
     "Unrecognised value " <> ToString[OptionValue[ValueRange]] <> 
      "for option ValueRange to PresentationArrayPlot"]];

  If[MatchQ[range, {a_,a_}], range[[2]] = range[[1]]+0.1];

  plotOpts = {LabelStyle -> Medium, 
    ColorFunction -> ColorData[{OptionValue[ColorScheme], range}], 
    ColorFunctionScaling -> False};
  plot = ArrayPlot[data, FilterRules[{opts}, Options[ArrayPlot]], 
    FrameTicks -> {{All, None}, {All, None}}, 
    PlotRangePadding -> None, plotOpts];
  legend = If[OptionValue[PlotLegend],
    Epilog -> 
     Inset[key2[range, FilterRules[{opts}, Options[key2]], plotOpts], 
      Offset[{-10, -10}, Scaled[{1, 1}]], ImageScaled[{1, 1}], 
      Scaled[{0.3, 0.5}]],
    Graphics[]];
  Show[plot, legend]];


SetAttributes[RasterizeManipulate, HoldFirst]

RasterizeManipulate[expr_, {var_, start_, end_, inc_, opts___}] :=
 Module[{frames},
  Monitor[
   frames = 
    Table[Module[{var = i}, Compress@Rasterize[expr]], {i, start, end,
       inc}], ProgressIndicator[(i - start)/(end - start)]];
  Manipulate[
   Uncompress[frames[[1 + Round[(var - start)/inc]]]], {var, start, 
    end, inc, opts}, SaveDefinitions -> True]];

(* See http://mathematica.stackexchange.com/questions/8645/how-do-i-make-framed-plots-the-same-size/8660#8660 *)

graphicsPadding[g_] :=
 BorderDimensions[
  Image[Rasterize@Show[g/.(PlotLabel->_)->Sequence[], LabelStyle -> White, Background -> White]]];

graphicsPadding[gs_List] :=
 MapThread[Max, Map[graphicsPadding, gs], 2];

PadGraphics[gs_List, depth_:1] :=
 With[{padding = 4 {{1,1},{1,1}} + graphicsPadding[Flatten[gs,depth]]},
  Map[Show[#, ImagePadding -> padding] &, gs, {depth}]];


(****************************************************************)
(* FindFitPlot                                                  *)
(****************************************************************)

Options[FindFitPlot] = 
 Join[Options[PresentationListLinePlot], 
  Options[Plot], {"FitFunctionRange" -> Automatic, "FitRange" -> Automatic}];

FindFitPlot[data_, model_, params_List, var_, 
  opts : OptionsPattern[]] :=
  FindFitPlot[data, {{model, params}}, var, opts];

FindFitPlot[datap_, modelParamsList : {{_, _List} ..}, var_, 
  opts : OptionsPattern[]] :=
  Module[{fits, fitFns, ffRange, fitRange, data},
    data = If[Head[datap] === List, ToDataTable[datap], datap];
    ffRange =
    Replace[OptionValue[FitFunctionRange],
      {Automatic :> Sort@Replace[Head[data],
        {DataTable :> CoordinateRange[data],
          List :> data[[{1, -1}, 1]],
          h_ :> 
          Error["findFitPlot: Unrecognised data type: " <> 
            ToString[h]]}],
        r_ :> r}];

    fitRange = Replace[OptionValue[FitRange],
      {Automatic :> ffRange,
        {a_,b_} :> {a,b}}];
    fits = FindFit[ToList@Slab[data,Span@@fitRange], Sequence @@ #, var] & /@ modelParamsList;
    fitFns = MapThread[ReplaceAll, {modelParamsList[[All, 1]], fits}];
    
    Show[PresentationListLinePlot[data, 
      FilterRules[{opts}, Options[PresentationListLinePlot]], 
      Joined -> False(*, PlotMarkers -> Automatic*)],
      Plot[fitFns, {var, ffRange[[1]], ffRange[[2]]}, 
        Evaluate[FilterRules[{opts}, Options[Plot]]], 
        PlotStyle -> PresentationPlotStyles]]];

End[];

EndPackage[];
