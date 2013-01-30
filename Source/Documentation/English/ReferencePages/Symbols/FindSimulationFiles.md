{
  "More Information" ->
   {
     "Simulations are often split into multiple segments due to limitations on the run time of a single job on an HPC system.  This function searches for files across all the segments and returns full pathnames to each one that is found.",
     "A simulation directory can be a single output directory, in which case at most one filename will be returned.",
     "Multiple segments in the format supported by SimFactory are supported."
   },

  "Basic Examples" -> {
    "FindSimulationFiles[\"bbh\",\"*.h5\"]"
   },
  "More About" -> {"SimulationTools"}
}
