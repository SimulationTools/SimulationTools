{
 "Summary" -> "produce a quick overview of a simulation",
 "URL" -> "http://bitbucket.org/ianhinder/nrmma",
 "Tutorials" -> {"NumericalRelativity"},
 "See Also" -> {
   "ReadCores",
   "ReadCPUHours",
   "ReadMemory",
   "ReadRunSpeed",
   "ReadSwap",
   "ReadWalltime"
   },
  "More About" -> {"nrmma"},
  "More Information" -> {
    "If not specified, rad defaults to the first available extraction radius.",
    "A list of run names may be given, in which case data from all the runs will be displayed on the same plots.",
    "SimView produces plots of the waveforms, trajectories, run speed, memory usage, simulation chunks, start times and end times.",
    "SimView looks for simulations in RunDirectory.",
    "SimView requires that the Multipole, PunctureTracker and SystemStatistics thorns to have been activated in the simulation."
    },
  "Basic Examples" -> {
    "SimView[\"bbh\"]"
  }
}
