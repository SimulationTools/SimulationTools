BeginPackage["SimulationTools`FileMemo`", {"SimulationTools`FileDependencies`"}];

WithFileMemo::usage = "WithFileMemo[f[args] := body] defines a function f with arguments args and body body such that the result of the function is stored on disk the first time it is called.  This is used to cache data that is expensive to read or compute.  The data is stored in $FileMemoDirectory under a filename which is a hash of f[args].";

$FileMemoDirectory = "cache";

Begin["`Private`"];

(****************************************************************)
(* Internal *)
(****************************************************************)

hash[x_] :=
  IntegerString[Hash[x,"SHA1"],16];

SetAttributes[cacheFile, HoldFirst];
cacheFile[pat_] :=
  FileNameJoin[{$FileMemoDirectory, ToString[hash[Hold[pat]]]<>".m"}];

SetAttributes[depsFile, HoldFirst];
depsFile[pat_] :=
  FileNameJoin[{$FileMemoDirectory, ToString[hash[Hold[pat]]]<>".deps.m"}];

SetAttributes[writeCache, HoldFirst];
writeCache[pat_, value_, deps_List : {}] :=
  ( If[!DirectoryQ[$FileMemoDirectory], CreateDirectory[$FileMemoDirectory]];
    Put[{Hold[pat], value}, cacheFile[pat]];
    Put[{Hold[pat], deps}, depsFile[pat]];
    value);

SetAttributes[readCache, HoldFirst];
readCache[pat_] :=
  Replace[
    DeclareFileDependency[readCacheDeps[pat]];
    Get[cacheFile[pat]],
    {{Hold[pat], value_} :> value,
      _ :> (Print["Error reading cache file ", cacheFile[pat], " for ",
        ToString[HoldForm[pat],InputForm]]; Abort[])}];

SetAttributes[readCacheDeps, HoldFirst];
readCacheDeps[pat_] :=
  Replace[Get[depsFile[pat]],
    {{Hold[pat], deps_} :> deps,
      _ :> (Print["Error reading cache file dependencies ", depsFile[pat], " for ",
        ToString[HoldForm[pat],InputForm]]; Abort[])}];

SetAttributes[cachedQ, HoldFirst];
cachedQ[pat_, staticDeps_List] :=
  FileExistsQ[cacheFile[pat]] &&
  And@@With[
    {cacheTime=AbsoluteTime[FileDate[cacheFile[pat]]]},
    (* Cache is only valid if all the dependencies exist and are older
       than the cache *)
    Map[(FileExistsQ[#] && AbsoluteTime[FileDate[#]] < cacheTime) &,
      Join[staticDeps,readCacheDeps[pat]]]];

(****************************************************************)
(* WithFileMemo *)
(****************************************************************)

SetAttributes[WithFileMemo, HoldFirst];

WithFileMemo[name_[args___] := body_, staticDeps_List : {}] :=
  pat : name[args] :=
  ( DeclareFileDependency[staticDeps];
    If[cachedQ[pat, staticDeps], readCache[pat],
    Internal`InheritedBlock[{ReadList},
      writeCache[pat, Sequence@@FileDependencies[body]]]] );

End[];

EndPackage[];
