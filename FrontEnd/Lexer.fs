module Fsgo.FrontEnd.Lexer

open System
open FSharpPlus
open FSharpPlus.Data
open Fsgo.Lang.Token
open Fsgo.FrontEnd.StringAcc

let private peek =
  State (fun (stream, loc) -> Seq.head stream, (stream, loc))

let private read =
  let updateState (stream, loc) =
    let head = Seq.head stream
    match head with
    | '\n' -> head, (Seq.tail stream, Loc.nextLine loc)
    | _ -> head, (Seq.tail stream, Loc.nextCol loc)
  State updateState

let private tokenFromState rawToken =
  let readLocFromState (stream, loc) =
    let result = Token (rawToken, loc)
    result, (stream, loc)
  State readLocFromState

let private isIdentifierStart ch = Char.IsLetter ch || ch = '_'

let private isDigit = Char.IsDigit

let private isWhitespace = Char.IsWhiteSpace

let lexIdentifier =
  let stringAcc = makeStringAcc ()
  let rec makeToken acc =
    monad {
      match! peek with
      | ch when isIdentifierStart ch || isDigit ch ->
        let! ch = read
        return! makeToken (acc >>= append ch)
      | _ ->
        let tokenStr = toString acc
        return! tokenFromState (Identifier tokenStr)
    }
  makeToken stringAcc

let lexWhitespace =
  let rec makeToken () =
    monad {
      match! peek with
      | ch when isWhitespace ch ->
        let! _ = read
        return! makeToken ()
      | _ ->
        return! tokenFromState Whitespace
    }
  makeToken ()

let doLex =
  monad {
    let! startChar = peek
    match startChar with
    | ch when isIdentifierStart ch -> return! lexIdentifier
    | ch when isWhitespace ch -> return! lexWhitespace
    // | ch when isEndOfFile ch -> return! lexEndOfFile
    | _ -> Token (End, Loc.empty)
  }

let lex (code: string) =
  let rec repeatLex acc state =
    let token, nextState = State.run doLex state
    match token with
    | (End, _) -> acc
    | _ -> repeatLex (token :: acc) nextState
  repeatLex [] (String.toSeq code, { Line = 0; Col = 0 }) |> Ok
