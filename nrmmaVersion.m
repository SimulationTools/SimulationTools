(* ::Package:: *)

(* Copyright 2010-2011 Barry Wardell and Ian Hinder

   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU Lesser General Public License as published by the Free
   Software Foundation; either version 2.1 of the License, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
   PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License along
   with this library; if not, write to the Free Software Foundation, Inc.,
   59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*)

BeginPackage["nrmmaVersion`"];

nrmmaVersion::usage = "nrmmaVersion[] returns version information for the nrmma package."

Begin["`Private`"];

nrmmaVersion[] :=
 Module[{path, buildid, gitrev},
  path = FileNameDrop[FindFile["nrmma`"], -2];

  buildid = Catch@ReadList[FileNameJoin[{path, "BUILD_ID"}], "String"];
  If[SameQ[buildid, $Failed],
    buildid = "git";
  ,
    buildid = First[buildid];
  ];

  gitrev = Catch@ReadList[FileNameJoin[{path, "GIT_REVISION"}],"String"];
  If[SameQ[gitrev, $Failed],
    gitrev = First@ReadList["!git --git-dir "<>FileNameJoin[{path, ".git"}]<>" rev-parse HEAD", String];
  ,
    gitrev = First[gitrev];
  ];

  Grid[{{"Installed in:", path},
       {"Build ID: ", buildid},
       {"Git revision: ", gitrev}},
    Alignment -> Left]
]

End[];
EndPackage[];

