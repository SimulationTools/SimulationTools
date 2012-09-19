$SimulationPath = {$NRMMATestSimulationDirectory};

(****************************************************************)
(* NumCycles                                                    *)
(****************************************************************)

Module[
 {wf1 = Psi4ToStrain[Shifted[30 ReadPsi4[$NRMMATestSimulation, 2, 2, 30], -TortoiseCoordinate[30, ReadADMMass[$NRMMATestSimulation]]], 0.1],
  wf2 = Psi4ToStrain[Shifted[100 ReadPsi4[$NRMMATestSimulation, 2, 2, 100], -TortoiseCoordinate[100, ReadADMMass[$NRMMATestSimulation]]], 0.1],
  sn  = ToDataTable[Table[{f, 125/100 + f/5 - f^2}, {f, -1, 1, 0.01}]],
  withinRoundoff
 },

withinRoundoff[a_, b_] := 2 Abs[(a-b)/(a+b)] < 10^-15;

Test[
    WaveformMatch[{wf1, wf1}, sn]
    ,
    1.
    ,
    EquivalenceFunction -> withinRoundoff,
    TestID->"WaveformMatch with the same waveform"
]

Test[
    WaveformMatch[{wf1, wf2}, sn]
    ,
    0.9901663128717905
    ,
    EquivalenceFunction -> withinRoundoff,
    TestID->"WaveformMatch with different waveforms"
]
]