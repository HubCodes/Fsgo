module FrontEnd.Test.TestLexer

open FSharpPlus.Data
open Fsgo.FrontEnd.Lexer
open Fsgo.Lang.Token
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestLexer () =

  [<TestMethod>]
  member this.LexWhitespace () =
    let input = makeState " "
    let ((token, _), _) = State.run lexWhitespace input
    Assert.AreEqual (Whitespace, token)
