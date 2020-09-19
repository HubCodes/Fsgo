module Fsgo.FrontEnd.Lexer

open System

open FSharpPlus
open FSharpPlus.Data
open Fsgo.Lang.Token
open Fsgo.FrontEnd.StringAcc

let peek = State (fun stream -> Seq.head stream, stream)

let read = State (fun stream -> Seq.head stream, Seq.tail stream)

let private isIdentifierStart ch = Char.IsLetter ch

let private isDigit ch = Char.IsDigit ch

let private lexIdentifier =
  let stringAcc = makeStringAcc ()
  let rec makeToken acc =
    monad {
      match! peek with
      | ch when isIdentifierStart ch || isDigit ch ->
        let! ch = read
        return! makeToken (acc >>= append ch)
      | _ -> toString acc |> Identifier
    }
  makeToken stringAcc

let private doLex =
  monad {
    let! startChar = peek
    match startChar with
    | ch when isIdentifierStart ch -> return! lexIdentifier
    | _ -> Identifier ""
  }

let lex (code: string) =
  State.run doLex (String.toSeq code) |> fst |> Ok
