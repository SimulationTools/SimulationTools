{
  "More Information" -> {
    "Parameters are always returned as strings."
  },
  "Basic Examples" -> {
    "dx = LookupParameter[\"bbh\", \"CoordBase::dx\"]",
    TextComment["Parameters are always returned as strings"],
    "FullForm[dx]",
    TextComment["To convert to numbers use ToExpression"],
    "ToExpression[dx]",
    TextComment["Lookup all parameters for the Carpet thorn:"],
    "carpetParams = FindParameters[\"bbh\", \"Carpet::*\"]",
    "Map[# -> LookupParameter[\"bbh\", #] &, carpetParams] // TableForm"
   },
  "See Also" -> {"FindParameters"},
  "More About" -> {"SimulationTools"}
}
