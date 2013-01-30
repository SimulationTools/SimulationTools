{
  "More Information" ->
   {
    "The Fixed Frequency Integration method is defined in Reisswig and Pollney, Class.Quant.Grav. 28 (2011) 195015, http://arxiv.org/abs/arXiv:1006.1632",
    "The cutoff frequency om0 can be chosen as half the initial physical gravitational wave frequency.  For more information, see Reisswig and Pollney 2011",
   },
  "Basic Examples" -> {
    "psi4 = ReadPsi4[\"bbh\", 2, 2, 100]",
    "h = Psi4ToStrain[psi4, 0.07]",
    "ListLinePlot[Re[h]]"
    },
  "See Also" -> {
    "ReadPsi4"
   },
  "More About" -> {"SimulationTools"}
}
