{
  "Basic Examples" -> {
    "ReadPsi4[\"bbh\",2,2,100]",
    "ToList[ReadPsi4[\"bbh\",2,2,100]] // Short",
    "ListLinePlot[Re[ReadPsi4[\"bbh\",2,2,100]], PlotRange -> All]",
    "ListLinePlot[Map[Re[ReadPsi4[\"bbh\",2,2,#]] &, ReadPsi4Radii[\"bbh\"]],
  PlotRange -> All]"
    },
  "See Also" -> {
    "ReadPsi4Radii", "ReadPsi4Modes", "ReadWaveformFile"
   },
  "More About" -> {"SimulationTools"}
}
