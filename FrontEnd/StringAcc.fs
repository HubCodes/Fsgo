module Fsgo.FrontEnd.StringAcc

open System.Text
open FSharpPlus.Data

let makeStringAcc () = State (fun _ -> StringBuilder (), ())

let append (ch: char) (sb: StringBuilder) = State (fun _ -> sb.Append ch, ())

let toString stringAcc =
  let sb: StringBuilder = State.eval stringAcc ()
  sb.ToString ()
