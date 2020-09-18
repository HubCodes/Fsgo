module Fsgo.FrontEnd.Lexer

open System
open System.IO
open System.Text

open Fsgo.Lang.Token

let private isIdentifierStart ch = Char.IsLetter (char ch)

let private isDigit ch = Char.IsDigit (char ch)

let private lexIdentifier (code: StreamReader) =
  let sb = StringBuilder ()
  let rec makeToken (acc: StringBuilder) (code: StreamReader) =
    match code.Peek () with
    | ch when isIdentifierStart ch || isDigit ch ->
      makeToken (acc.Append (code.Read ())) code
    | _ -> acc.ToString ()
  makeToken (sb.Append (code.Read ())) code |> Identifier

let private doLex (code: StreamReader) =
  let startChar = code.Peek ()
  match startChar with
  | ch when isIdentifierStart ch -> lexIdentifier code
  | _ -> Identifier ""

let lex (code: string) =
  use codeReader = new StreamReader (code)
  Ok ()
