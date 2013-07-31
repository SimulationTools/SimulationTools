{
  "More Information" ->
   {
    "Computation of a continuous phase from a complex timeseries (e.g. using the Phase function) is complicated by the fact that some sort of continuity condition must be imposed at the values +- \[Pi].  This continuity condition, which determines the multiples of 2 \[Pi] to add, can be unpredictable in the presence of noisy data.  Given several phases which can be taken to be within 2 \[Pi] of each other at t, this function can be used to correct them by multiples of 2 \[Pi] to ensure that this is the case."
   },

  "Basic Examples" -> {
    "phase1 = Shifted[Phase[ReadPsi4[\"bbh\",2,2,80]], -80]",
    "phase2 = Shifted[Phase[ReadPsi4[\"bbh\",2,2,100]], -100]",
    "{phase1a, phase2a} = AlignedPhases[{phase1, phase2}, 100]",
    "ListLinePlot[{phase1a, phase2a}]"
    },
  "See Also" -> {
    "Phase", "ReadPsi4"
   },
  "Tutorials" -> {"SimulationTools", "GridFunctions"}
}
