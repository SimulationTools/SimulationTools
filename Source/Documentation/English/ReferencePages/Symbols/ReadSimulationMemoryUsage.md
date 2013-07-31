{
  "More Information" ->
   {
    "This function requires output from the SystemStatistics thorn:

IOScalar::outScalar_every      = 256
IOScalar::outScalar_reductions = \"maximum\"
IOScalar::outScalar_vars       = \"SystemStatistics::process_memory_mb\"",
    "The memory usage is the maximum across all processes in the simulation",
    "The memory usage is the 'resident set size' measured in bytes"
   },

  "Basic Examples" -> {
    "mem = ReadSimulationMemoryUsage[\"bbh\"]",
    "ListLinePlot[mem]"
    },
  "See Also" -> {
    "ReadSimulationCoreCount", "ReadSimulationRunTime"
   },
  "Tutorials" -> {"SimulationTools"}
}
