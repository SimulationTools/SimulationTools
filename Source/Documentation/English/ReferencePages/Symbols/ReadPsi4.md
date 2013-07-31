{
  "More Information" ->
   {
     "The Psi4 data can come from the Multipole or Ylm_Decomp thorns, or a simulation in the Numerical Relativity Data Format",
     "The alias chosen for Psi4 in Multipole can be chosen by setting $MultipolePsi4Variable (the default is \"psi4\")"
   },

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
  "Tutorials" -> {"SimulationTools"}
}
