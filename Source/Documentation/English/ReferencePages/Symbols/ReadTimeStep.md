{
  "More Information" ->
   {
     "This information is read from the parameter file, and currently supports the CoordBase, Time and Coordinates (Llama multipatch) thorns.",
     "The time step is that on refinement level given as the optional argument RefinementLevel, which defaults to 0 (coarse level).",
     "The parameter Carpet::time_refinement_factors, if set, is taken into account."
   },

  "Basic Examples" -> {
    "ReadTimeStep[\"bbh\"]",
    "ReadTimeStep[\"bbh\", RefinementLevel -> 3]"
    },
  "See Also" -> {
    "ReadMaxRefinementLevels", "ReadGridSpacings"
   },
  "Tutorials" -> {"SimulationTools"}
}
