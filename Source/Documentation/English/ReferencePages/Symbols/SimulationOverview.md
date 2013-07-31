{
 "Summary" -> "produce a quick overview of a simulation",
 "URL" -> "http://simulationtools.org",
 "Tutorials" -> {"SimulationInformation"},
 "See Also" -> {
   "ReadSimulationCoreCount",
   "ReadSimulationCost",
   "ReadSimulationMemoryUsage",
   "ReadSimulationRunTime",
   "ReadSimulationSpeed",
   "ReadSimulationSwapUsage"
   },
  "More Information" -> {
    "A list of run names may be given, in which case data from all the runs will be displayed on the same plots.",
    "SimulationOverview produces plots and tables of useful information which show an overview of the simulation.  Different components of SimulationTools can provide plots for SimulationOverview to use.  For example, it can show waveforms and trajectories of binary simulations, and run speed, memory usage, simulation chunks, start times and end times of each segment, for all simulations, if this data is available in the simulation output.",
    "SimulationOverview looks for simulations in $SimulationPath.",
    "SimulationOverview data can be obtained from the output of the Multipole, PunctureTracker and SystemStatistics thorns."
    },
  "Basic Examples" -> {
    "SimulationOverview[\"bbh\"]"
  }
}
    