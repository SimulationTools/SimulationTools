
(*  Copyright 2012 Ian Hinder
*)

BeginPackage["Piraha`", {"JLink`"}];

Parse::usage = "Parse[grammarfile, pattern, inputfile] parses a file named inputfile using a grammar stored in a file named grammarfile using pattern as the root pattern.  The parse tree is returned as Symbolic XML.";
CleanParseTree;

EnsureJava;

Begin["`Private`"];

javaInstalled = False;

EnsureJava[] :=
  If[javaInstalled,
     Null,
     (* else *)
     InstallJava[];
     AddToClassPath[FileNameJoin[{nrmmaDir,"PirahaPeg","piraha.jar"}]];
     javaInstalled = True];

(* The JRE does not share a current directory with the Mathematica
   kernel, so relative paths have to be converted to absolute paths.
   It is not possible to change the JRE working directory. *)

absPath[s_String] :=
  If[StringTake[s,1] === $PathnameSeparator, s, FileNameJoin[{Directory[],s}]];

nrmmaDir = FileNameDrop[FindFile["nrmma`"],-2];

DefFn[
  Parse[grammarFileName_String, pattern_String, inputFileName_String] :=
  Module[
    {gf,g,m,c,sw,dout,xmlString,xml},

    EnsureJava[];

    If[FileExistsQ[grammarFileName],
       gf = grammarFileName,
       If[FileExistsQ[FileNameJoin[{nrmmaDir, "Grammars", grammarFileName}]],
          gf=FileNameJoin[{nrmmaDir, "Grammars", grammarFileName}],
          Error[StringForm["Cannot find grammar '`1`'", grammarFileName]]]];

    g = JavaNew["edu.lsu.cct.piraha.Grammar"];
    g@compileFile[JavaNew["java.io.File", gf]];

    c = Grammar`readContents[JavaNew["java.io.File", absPath@inputFileName]];

    m = g@matcher[pattern, c];

    If[!m@match[0], Error["Failed to parse input file: "<>inputFileName<>" " <> m@near[]@toString[]]];

    sw = JavaNew["java.io.StringWriter"];
    dout = JavaNew["edu.lsu.cct.piraha.DebugOutput", JavaNew["java.io.PrintWriter", sw]];
    m@dumpMatchesXML[dout];
    dout@flush[];
    xmlString = sw@toString[];
    xml = ImportString[xmlString, "XML"];
    Scan[ReleaseJavaObject, {g, m, sw, dout}];
    xml]];

(* Structure functions *)

DefFn[
  StructGet[expr_, path_List] :=
  Module[
    {results, pat,x},
    pat = Fold[#2[___, #1, ___] &, x:Last[path], Reverse@Drop[path,-1]];
    results = Cases[{expr}, pat :> x];

    If[Length[results] === 0, 
       Error[ToString@StringForm["Cannot find `1` in expression `2`", path, expr]]];

    results[[1]]]];

DefFn[
  StructMatchQ[expr_, path_List] :=
  Module[{result},
         result=MatchQ[expr, Fold[#2[___, #1, ___] &, Last[path], Reverse@Drop[path,-1]]];
         result]];

CleanParseTree[t_] :=
  ((t //. {("startIndex" -> _) :> Sequence[], ("endIndex" -> _) :> Sequence[]} )//.
   {(XMLObject["Document"][_,data_,___]) :> data,  XMLElement[s_,_,c_] :> s@@c});

End[];

EndPackage[];
