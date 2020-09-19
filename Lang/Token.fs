module Fsgo.Lang.Token

type Loc = { Line: int; Col: int }

[<RequireQualifiedAccess>]
module Loc =
  let empty = { Line = 0; Col = 0 }
  
  let nextLine loc = { Line = loc.Line + 1; Col = 0 }
  
  let nextCol loc = { loc with Col = loc.Col + 1 }

type RawToken =
  | Identifier of string

type Token = RawToken * Loc
