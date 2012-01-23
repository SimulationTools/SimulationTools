{
  "More Information" ->
   {
    "In addition to the options supported by ListLinePlot, ListLinePlot adds the PlotLegend and LegendPosition options.",
    "PlotLegend must be given as a list, not a single value.",
    "The list of curves to plot must be given in {{x,y},...} form rather than {{y1, y2...}, ...}, or the plot styles might not be consistent."
   },
   "Basic Examples" ->
    {
     "data1 = Table[{x, Sin[2 Pi x - Pi]}, {x, 0, 1, 0.01}];",
     "data2 = Table[{x, Sin[2 Pi x - Pi/2]}, {x, 0, 1, 0.01}];",
     "data3 = Table[{x, Sin[2 Pi x - Pi/1.5]}, {x, 0, 1, 0.01}];",
     "ListLinePlotWithLegend[{data1, data2, data3}, PlotLegend -> {1, 2, 3}, PlotStyle -> {Red, Green, Blue}]"
    }
    (* FIXME: Add 
    Options->PlotLegend
      Set labels for use in the plot legend.
      ListLinePlotWithLegend[{data1, data2, data3}, PlotLegend -> {1, 2, 3}]
    Options->LegendPosition
      Set the location of the legend in the form {xpos, ypos} where xpos can be Left or Right and ypos can be Top or Bottom.
      ListLinePlotWithLegend[{data1, data2, data3}, PlotLegend -> {1, 2, 3}, LegendPosition -> {Left, Bottom}]
    *)
}