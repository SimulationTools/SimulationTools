Get[FileNameJoin[{$SimulationToolsInstallationDirectory,"Tests","Common.m"}]];

$SimulationPath = {$SimulationToolsTestSimulationDirectory};

(****************************************************************)
(* NumCycles                                                    *)
(****************************************************************)

Module[
 {wf1 = Psi4ToStrain[Shifted[30 ReadPsi4[$SimulationToolsTestSimulation, 2, 2, 30], -TortoiseCoordinate[30, ReadADMMass[$SimulationToolsTestSimulation]]], 0.1],
  wf2 = Psi4ToStrain[Shifted[100 ReadPsi4[$SimulationToolsTestSimulation, 2, 2, 100], -TortoiseCoordinate[100, ReadADMMass[$SimulationToolsTestSimulation]]], 0.1],
  sn  = ToDataTable[Table[{f, 125/100 + f/5 - f^2}, {f, -1, 1, 0.01}]]
 },

VerificationTest[
    WaveformMatch[{wf1, wf1}, sn]
    ,
    1.
    ,
    SameTest -> withinRoundoff,
    TestID->"WaveformMatch with the same waveform"
];

VerificationTest[
    WaveformMatch[{wf1, wf2}, sn]
    ,
    0.9901663128717905
    ,
    SameTest -> withinRoundoff,
    TestID->"WaveformMatch with different waveforms"
];
]