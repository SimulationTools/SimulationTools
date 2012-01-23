{
  "More Information" -> {
    "The data is returned as a list of the form {{t1, d1}, {t2, d2}, ..., {tn, dn}} where the ti are the times and the di are DataTables representing the 1D data at those times.",
    "The functions ASCIITimeOfIndex and ASCIIDataOfIndex can be used for working with the output format."
  },
  "Basic Examples" ->
   {
    TextComment["The resulting structure (simulated here for the purpose of documentation) is of the form"],
    "data = Table[{t, MakeDataTable[Table[{x, Sin[2 Pi (x - t)]}, {x, 0, 1, 0.1}]]}, {t, 0, 1, 0.1}]"
   },
  "See Also" -> {
    "ASCIIDataOfIndex",
    "ASCIITimeOfIndex"
   },
  "More About" -> {"nrmma"}
}
