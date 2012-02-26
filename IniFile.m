
BeginPackage["IniFile`", {"RunFiles`", "DataTable`", "Memo`", "Error`"}];

ReadIniFile;
IniVariable;

Begin["`Private`"];

DefineMemoFunction[ReadIniFile[file_],
  Flatten[StringCases[
    ReadList[file, String],
    key__ ~~ "=" ~~ value___ :> {StringTrim@key -> StringTrim@value}]]];

IniVariable[file_, key_] :=
  Module[{map = ReadIniFile[file]},
    If[!MemberQ[First/@map, key], Error["Key not found"],
    Return[key /. map]]];

End[];

EndPackage[];
