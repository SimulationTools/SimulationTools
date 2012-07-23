(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["MessageCatcher`",
 {
  "Error`"
 }];

WithCaughtMessages::usage = "WithCaughtMessages[expr] evaluates expr, converting any generated messages into exceptions";

Begin["`Private`"];

$allowedMessages = {};

(****************************************************************)
(* messageHandler *)
(****************************************************************)

messageHandler[x_] :=
  If[Last[x] && !MemberQ[$allowedMessages,x[[1,1]]],
     x /. HoldPattern[_[Message[id_, args___], _]] :> ErrorMessage[id, args]; Abort[]];

(****************************************************************)
(* WithCaughtMessages *)
(****************************************************************)

SetAttributes[WithCaughtMessages, HoldFirst];
WithCaughtMessages[expr_] :=
  Internal`HandlerBlock[{"Message", messageHandler},
                        (expr)];

End[];

EndPackage[];
