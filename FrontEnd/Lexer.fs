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

let private tokenFromState tokenString tokenConstructor =
  let readLocFromState (stream, loc) =
    let result = Token (tokenConstructor tokenString, loc)
    result, (stream, loc)
  State readLocFromState

let private isIdentifierStart ch = Char.IsLetter ch || ch = '_'

let private isDigit ch = Char.IsDigit ch

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
        return! tokenFromState tokenStr Identifier
    }
  makeToken stringAcc

let doLex =
  monad {
    let! startChar = peek
    match startChar with
    | ch when isIdentifierStart ch -> return! lexIdentifier
    // | ch when isEndOfFile ch -> return! lexEndOfFile
    // TODO: 공백 처리하기
    | _ -> Token (Identifier "", Loc.empty)
  }

let lex (code: string) =
  let rec repeatLex acc state =
    let token, nextState = State.run doLex state
    match token with
    | (End, _) -> acc
    | _ -> repeatLex (token :: acc) nextState
  repeatLex [] (String.toSeq code, { Line = 0; Col = 0 }) |> Ok
