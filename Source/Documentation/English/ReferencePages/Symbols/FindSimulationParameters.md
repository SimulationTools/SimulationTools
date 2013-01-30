{
  "More Information" ->
   {
     "The parameter file must be available in the simulation output directory, and only the parameters found there will be returned.",
     "The pattern is the same as is used in Mathematica's StringMatchQ; it can be a simple wildcarded string, a StringExpression, or a RegularExpression.",
     "Parameter names are returned as strings in the form \"<thorn>::<param>\"."
   },

  "Basic Examples" -> {
    "FindSimulationParameters[\"bbh\",\"CoordBase::*\"]"
    },
  "See Also" -> {
    "ReadSimulationParameter"
   },
  "More About" -> {"SimulationTools"}
}
