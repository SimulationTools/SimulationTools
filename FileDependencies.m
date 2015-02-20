BeginPackage["SimulationTools`FileDependencies`"];

FileDependencies::usage = "FileDependencies[expr] returns {val,deps} where val is the result of evaluating expr, and deps is a list of filenames used during the evaluation";
DeclareFileDependency::usage = "DeclareFileDependency[f] asserts that the file f is being accessed.  f can be a filename or a list of filenames.";

Begin["`Private`"];

(****************************************************************)
(* Internal *)
(****************************************************************)

prefixQ[l1_List, l2_List] :=
 SameQ[l1, Take[l2, Min[Length[l1], Length[l2]]]]

inDirectoryQ[file_String, dir_String] :=
  prefixQ[FileNameSplit[dir], FileNameSplit[file]];

putDependency[dep_] :=
  If[!inDirectoryQ[dep, $TemporaryDirectory],
    Sow[dep, Dependency]];

SetAttributes[withReadListDeps, HoldFirst];
withReadListDeps[x_] :=
Internal`InheritedBlock[{ReadList},
  Unprotect[ReadList];
  Module[{flag},
    ReadList[file_, args___] /; ! TrueQ[flag] :=
    Block[{flag = True},
      (* Print["Accessing file ", file]; *)
      putDependency[file];
      ReadList[file, args]]];
  Protect[ReadList];

  x];

(****************************************************************)
(* FileDependencies *)
(****************************************************************)

DeclareFileDependency[f_String] :=
  putDependency[f];

DeclareFileDependency[l_List] :=
  Scan[putDependency, l];

SetAttributes[FileDependencies, HoldFirst];
FileDependencies[x_] :=
  Replace[Reap[withReadListDeps[x],Dependency],
    {{result_, deps_} :> {result, Sow[Union[Flatten[deps]], Dependency]},
      _ :> (Print["Error in FileDependencies"]; Abort[])}];

End[];

EndPackage[];
