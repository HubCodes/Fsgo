module Fsgo.Program

open Fsgo.FrontEnd.Lexer

[<EntryPoint>]
let main _ =
  printfn "%A" <| lex "identifier 1"
  0
