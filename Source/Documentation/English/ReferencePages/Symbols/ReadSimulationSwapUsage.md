{
  "More Information" ->
   {
    "This function requires output from the SystemStatistics thorn:

IOScalar::outScalar_every = 256
IOScalar::outScalar_reductions = \"maximum\"
IOScalar::outScalar_vars = \"SystemStatistics::process_memory_mb\"",
    "The swap memory usage is the maximum across all processes in the simulation",
    "The memory is measured in bytes"
   },

  "Basic Examples" -> {
    "ReadSimulationSwapUsage[\"bbh\"]"
    },
  "See Also" -> {
    "ReadSimulationCoreCount", "ReadSimulationRunTime", "ReadSimulationMemoryUsage"
   },
  "Tutorials" -> {"SimulationTools"}
}
