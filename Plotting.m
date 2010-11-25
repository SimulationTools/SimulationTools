
BeginPackage["Plotting`", {"DataRegion`"}];

MakePlotLegend;
ListLinePlotWithLegend;
ListLogLinearPlotWithLegend;
PlotLegend;
LegendPosition;
DynamicListLinePlot;
PresentationListLinePlot;
PresentationArrayPlot;
ColorRange;
PresentationPlotStyles;
PresentationPlotColors;
LegendOrientation;
LegendLineSize;
PlotFit::usage = "PlotFit[data, model, pars, var] takes the same arguments as FindFit and returns a plot of the fitted function as well as the result of the fit."
PlotKey;

Begin["`Private`"];

point = Graphics[Point[{0, 0}]];

plotStyles = 
  {Black, Blue, Darker[Magenta], Darker[Green], Orange, Gray, 
   Darker[Red], Darker[Yellow]};

PresentationPlotColors = {Black, Blue, Darker[Magenta], Darker[Green], Orange, Gray, 
   Darker[Red], Darker[Yellow]};

PresentationPlotStyles = Map[Directive[#,AbsoluteThickness[1]]&,PresentationPlotColors];

withStyle[elem_, style_List] :=
  Append[style, elem];

withStyle[elem_, style_] :=
  {style, elem};

Options[MakePlotLegend] = {LegendOrientation -> Vertical, LegendLineSize -> 40};

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
                 Style[Text[lab], If[labelStyle === Automatic, {},labelStyle]]}],
                {labels, Take[style, Length[labels]]}],If[hor,1,0]]],
      Alignment -> {Left,Center},
      Spacings-> {{0,Automatic}~Join~Flatten[Table[{1,Automatic},{i,Length[labels]}],1]}]];

styleInListLinePlot[lp_] :=
  Module[{styles},
    Cases[lp, {styles___, _Line} :> {styles}, Infinity]];

Options[ListLinePlotWithLegend] =
  Join[Options[ListLinePlot], {PlotLegend -> {}, LegendPosition -> {Left, Top}}, Options[MakePlotLegend]];

ListLinePlotWithLegend[args___, opts:OptionsPattern[]] :=
  Module[{style,pos,posx,posy,offset,scale,labelStyle,f, single},
    dims = Dimensions@First[{args}];
    single = (Length[dims] == 2 || (Length[dims] === 3 && dims[[1]] === 1));
(*    style = styleInListLinePlot[
      ListLinePlot[args, FilterRules[{opts}, Options[ListLinePlot]]]]; *)

    If[OptionValue[PlotStyle] === Automatic,
      style = PresentationPlotStyles,
      style = OptionValue[PlotStyle]];
    If[single, style = First[style]];
    pos = OptionValue[LegendPosition];
    {posx, posy} = pos;
    f = 0.05;
    offset = 10 {Switch[posx, Right, -1, Left, 1, Center, 0, _, Throw["Unknown position"]], If[posy === Top, -1, 1]};
    scale = {Switch[posx, Right, 1-f, Left, f, Center, 1, _, Throw["Unknown position"]], If[posy === Bottom, f, 1-f]};

    labelStyle = OptionValue[LabelStyle];
    ListLinePlot[args, PlotStyle -> style,
      FilterRules[{opts}, Options[ListLinePlot]],
      Epilog -> 
        Inset[
          MakePlotLegend[OptionValue[PlotLegend], If[single, {style}, style],labelStyle,
          FilterRules[{opts}, Options[MakePlotLegend]]],
          Scaled[scale], pos]]];

PresentationListLinePlot[args___, opts:OptionsPattern[]] :=
  ListLinePlotWithLegend[args,opts,LabelStyle->Medium,Frame->True];

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

PlotFit[data_, model_, pars_, var_, args___] :=
 Module[{fit, fittedModel},
  fit = FindFit[data, model, pars, var, MaxIterations -> 10000];
  fittedModel = model /. fit;
  {Show[ListPlot[data, PlotRange -> All],
    Plot[fittedModel, {h, 0, Max[First /@ data]}, PlotRange -> {{0,0.6},{-50,-40}}],
    PlotLabel -> First[pars] /. fit, args, ImageSize -> 400,PlotRange->{{0,0.6},{-50,-40}}], fit}];


key[mapName_String, {min_, max_}, opts___] :=
  ArrayPlot[Table[{c, c}, {c, max, min, -(max - min)/100}],
   opts,
   ColorFunctionScaling -> False,
   ColorFunction ->
    ScaledColorFunction[mapName, 1 {min, max}],
   DataRange -> {{0, 1}, {min, max}}, AspectRatio -> 8, ImageSize->{Automatic,400},
   FrameTicks -> {Table[
      If[Abs[c] < 10^-15, 0, N@c], {c, min, max, (max - min)/10}],
     False, False, False}];

Options[PresentationArrayPlot] =
{ColorRange -> Automatic, ColorMap -> "TemperatureMap", PlotKey -> True} ~Join~ Options[ArrayPlot];

PresentationArrayPlot[data_DataRegion, opts:OptionsPattern[]] :=
  Module[{range},
    range = OptionValue[ColorRange];
    If[range === Automatic, range = {Min[data], Max[data]}];

    keyPlot = key[OptionValue[ColorMap],range];

    plot = DataRegionArrayPlot[data, Sequence@@FilterRules[{opts},Options[ArrayPlot]],
             ImageSize->{400,400}, ColorFunctionScaling -> False,
             ColorFunction -> ScaledColorFunction[OptionValue[ColorMap], range],
             FrameTicks -> True, LabelStyle -> Medium];
    If[OptionValue[PlotKey],
      Row[{plot,keyPlot}],
      plot]];

End[];

EndPackage[];
