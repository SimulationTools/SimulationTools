(* Copyright (C) 2010 Ian Hinder and Barry Wardell *)

BeginPackage["Memo`"];

DefineMemoFunction::usage = "DefineMemoFunction[f[args], body] defines a function f with arguments args and body body such that the result of the function is remembered after the first time it is called.  This is used to cache data that is expensive to read or compute.  See ClearAllMemos for flushing this cache.";
ClearAllMemos::usage = "ClearAllMemos[] wipes the cache of functions that have been memoised using DefineMemoFunction.";

Begin["`Private`"];

If[!ValueQ[$cache], $cache = {}];

SetAttributes[DefineMemoFunction, HoldAll];
DefineMemoFunction[name_[args___], body_] :=
  Module[
    {pat},
    pat : name[args] := (AppendTo[$cache, Hold[pat]]; pat = body)];

ClearAllMemos[] :=
  ($cache /. Hold -> Unset; $cache = {};)

End[];

EndPackage[];
