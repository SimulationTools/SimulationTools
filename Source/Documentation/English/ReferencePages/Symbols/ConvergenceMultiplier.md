{
  "Basic Examples" -> 
   {
    "exact = MakeDataTable@Table[{x, 1 + 0.1 Sin[2 Pi x]}, {x, 0, 1, 0.01}];\n"<>
    "addErr[tb_DataTable, h_, p_] := MakeDataTable@Map[{#[[1]], #[[2]] + 0.03 Cos[2 Pi #[[1]]] h^p + 0.03 Sin[2 Pi #[[1]]] h^(p + 1)} &, ToList[tb]];\n"<>
    "{h1, h2, h3} = {0.96, 0.80, 0.64};\n"<>
    "order = 4;\n"<>
    "{f[h1], f[h2], f[h3]} = addErr[exact, #, order] & /@ {h1, h2, h3};\n"<>
    "cr4 = ConvergenceMultiplier[{h1, h2, h3}, order];\n"<>
    "ListLinePlot[{(f[h1] - f[h2])/cr4, f[h2] - f[h3]}]"
   },
  "See Also" -> {"ConvergenceRate"},
  "Tutorials" -> {"SimulationTools"}
}
