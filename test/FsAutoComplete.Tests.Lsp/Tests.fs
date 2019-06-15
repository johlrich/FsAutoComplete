﻿module FsAutoComplete.Tests.Lsp

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers

///Test for initialization of the server
let initTests () =
  test "InitTest" {
    let (server, event) = createServer()

    let p : InitializeParams =
      { ProcessId = Some 1
        RootPath = Some __SOURCE_DIRECTORY__
        RootUri = None
        InitializationOptions = Some (Server.serialize defaultConfigDto)
        Capabilities = Some clientCaps
        trace = None}

    let result = server.Initialize p |> Async.RunSynchronously
    match result with
    | Result.Ok res ->
      Expect.equal res.Capabilities.CodeActionProvider (Some true) "Code Action Provider"
      Expect.equal res.Capabilities.CodeLensProvider (Some {CodeLensOptions.ResolveProvider = Some true}) "Code Lens Provider"
      Expect.equal res.Capabilities.DefinitionProvider (Some true) "Definition Provider"
      Expect.equal res.Capabilities.DocumentFormattingProvider None "Document Formatting Provider"
      Expect.equal res.Capabilities.DocumentHighlightProvider (Some true) "Document Highligthing Provider"
      Expect.equal res.Capabilities.DocumentLinkProvider None "Document Link Provider"
      Expect.equal res.Capabilities.DocumentOnTypeFormattingProvider None "Document OnType Formatting Provider"
      Expect.equal res.Capabilities.DocumentRangeFormattingProvider None "Document Range Formatting Provider"
      Expect.equal res.Capabilities.DocumentSymbolProvider (Some true) "Document Symbol Provider"
      Expect.equal res.Capabilities.ExecuteCommandProvider None "Execute Command Provider"
      Expect.equal res.Capabilities.Experimental None "Experimental"
      Expect.equal res.Capabilities.HoverProvider (Some true) "Hover Provider"
      Expect.equal res.Capabilities.ImplementationProvider (Some true) "Implementation Provider"
      Expect.equal res.Capabilities.ReferencesProvider (Some true) "References Provider"
      Expect.equal res.Capabilities.RenameProvider (Some true) "Rename Provider"
      Expect.equal res.Capabilities.SignatureHelpProvider (Some {SignatureHelpOptions.TriggerCharacters = Some [| "("; ","|]} ) "Signature Help Provider"
      let td =
        { TextDocumentSyncOptions.Default with
            OpenClose = Some true
            Change = Some TextDocumentSyncKind.Full
            Save = Some { IncludeText = Some true }
        }
      Expect.equal res.Capabilities.TextDocumentSync (Some td) "Text Document Provider"
      Expect.equal res.Capabilities.TypeDefinitionProvider (Some true) "Type Definition Provider"
      Expect.equal res.Capabilities.WorkspaceSymbolProvider (Some true) "Workspace Symbol Provider"
    | Result.Error e ->
      failwith "Initialization failed"
  }


///Tests for basic operations like hover, getting document symbols or code lens on simple file
let basicTests () =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "BasicTest")
  serverTest path defaultConfigDto (fun (server, event) ->
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "Basic Tests" [
      testList "Hover Tests" [

        test "Hover Tests - simple symbol" {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 0; Character = 4}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val t : int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.t*"
                    MarkedString.String "*Assembly: Script*"|]

            Expect.equal res.Contents expected "Hover test - simple symbol"
        }

        test "Hover Tests - let keyword" {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 0; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "let"}
                    MarkedString.String "Used to associate, or bind, a name to a value or function."|]

            Expect.equal res.Contents expected "Hover test - let keyword"
        }

        test "Hover Tests - out of position" {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 1; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            failtest "Expected failure"
        }
      ]
      testList "Document Symbol Tests" [
        test "Document Symbol" {
          let p : DocumentSymbolParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentDocumentSymbol p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 2 "Document Symbol has all symbols"
        }
      ]
      testList "Code Lens Tests" [
        test "Get Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 1 "Get Code Lens has all locations"
        }

        test "Resolve Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let cl = res.[0]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            match res with
            | Result.Error e -> failtest "Request failed"
            | Result.Ok cl ->
              Expect.equal cl.Command.Value.Title "int" "Code Lens contains signature"
        }
      ]


    ])

///Tests for getting and resolving code(line) lenses with enabled reference code lenses
let codeLensTest () =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensTest")
  serverTest path {defaultConfigDto with EnableReferenceCodeLens = Some true} (fun (server, event) ->
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "Code Lens Tests" [
      test "Get Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 18 "Get Code Lens has all locations"
        }
      test "Resolve Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[0]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            let cl = result.[9]
            let res2 = server.CodeLensResolve cl |> Async.RunSynchronously
            match res, res2 with
            | Result.Ok cl, Result.Ok cl2 ->
              //TODO
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "string -> unit" "Code Lens contains signature"

            | _ -> failtest "Request failed"
        }

      test "Resolve Code Lens 2" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[3]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            let cl = result.[12]
            let res2 = server.CodeLensResolve cl |> Async.RunSynchronously
            match res, res2 with
            | Result.Ok cl, Result.Ok cl2 ->
              //TODO
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "unit -> (int64 -> System.DateTime)" "Code Lens contains signature"

            | _ -> failtest "Request failed"
        }
    ]
  )

///Tests for getting document symbols
let documentSymbolTest () =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DocumentSymbolTest")
  serverTest path defaultConfigDto (fun (server, event) ->
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "Document Symbols Tests" [
      test "Get Document Symbols" {
        let p : DocumentSymbolParams = { TextDocument = { Uri = filePathToUri path}}
        let res = server.TextDocumentDocumentSymbol p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Length 15 "Document Symbol has all symbols"
          Expect.exists res (fun n -> n.Name = "MyDateTime" && n.Kind = SymbolKind.Class) "Document symbol contains given symbol"
        }
    ]
  )

type ResponseWrapper<'a> = 
  {
    Kind: string
    Data: 'a
  }

type DocumentationDescription =
    { XmlKey : string
      Constructors : string list
      Fields : string list
      Functions : string list
      Interfaces: string list
      Attributes: string list
      Types: string list
      Signature : string
      Comment : string
      Footer : string }

let documentionTest () =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DocumentationTest")
  serverTest path defaultConfigDto (fun (server, event) ->
    let path = Path.Combine(path, "Effects.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "Document Tests" [
      test "Get Document" {
        let p : TextDocumentPositionParams  = { TextDocument = { Uri = filePathToUri path} ; Position = { Line=62 ; Character = 12 }}
        let res = server.FSharpDocumentation p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok notification ->

          let resp =  Newtonsoft.Json.Linq.JToken.Parse(notification.Content).ToObject<ResponseWrapper<DocumentationDescription[][]>>().Data
          let doc = resp |> Array.head |> Array.head

          Expect.stringContains doc.Signature "State.IState" "Missing inferred constraint"
          Expect.stringContains doc.Signature "Logger.ILogger" "Missing inferred constraint"
          Expect.stringContains doc.Signature "DateTime.IDateTime" "Missing inferred constraint"
        }
    ]
  )


///Tests for getting autocomplete
let autocompleteTest () =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteTest")
  serverTest path defaultConfigDto (fun (server, event) ->
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "Autocomplete Tests" [
      test "Get Autocomplete module members" {
        let p : CompletionParams = { TextDocument = { Uri = filePathToUri path}
                                     Position = { Line = 8; Character = 2}
                                     Context = None }
        let res = server.TextDocumentCompletion p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 2 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "func") "Autocomplete contains given symbol"
          Expect.exists res.Items (fun n -> n.Label = "sample func") "Autocomplete contains given symbol"
        }

      test "Get Autocomplete namespace" {
        let p : CompletionParams = { TextDocument = { Uri = filePathToUri path}
                                     Position = { Line = 10; Character = 2}
                                     Context = None }
        let res = server.TextDocumentCompletion p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "System") "Autocomplete contains given symbol"

        }

      test "Get Autocomplete namespace members" {
        let p : CompletionParams = { TextDocument = { Uri = filePathToUri path}
                                     Position = { Line = 12; Character = 7}
                                     Context = None }
        let res = server.TextDocumentCompletion p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "DateTime") "Autocomplete contains given symbol"

        }

      test "Get Autocomplete module doublebackticked members" {
        let p : CompletionParams = { TextDocument = { Uri = filePathToUri path}
                                     Position = { Line = 14; Character = 18}
                                     Context = None }
        let res = server.TextDocumentCompletion p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "z") "Autocomplete contains given symbol"

        }

    ]
  )

///Rename tests
let renameTest () =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest")
  serverTest path defaultConfigDto (fun (server, event) ->
    do waitForWorkspaceFinishedParsing event
    let pathTest = Path.Combine(path, "Test.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument pathTest}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let path = Path.Combine(path, "Program.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "Rename Tests" [
      test "Rename from usage" {
        let p : RenameParams = { TextDocument = { Uri = filePathToUri path}
                                 Position = { Line = 7; Character = 12}
                                 NewName = "y" }
        let res = server.TextDocumentRename p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res.DocumentChanges with
          | None -> failtest "No changes"
          | Some result ->
            Expect.equal result.Length 2 "Rename has all changes"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Program.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 7; Character = 12 }; End = {Line = 7; Character = 13 } }) ) "Rename contains changes in Program.fs"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Test.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 5 } }) ) "Rename contains changes in Test.fs"
            ()
      }

      test "Rename from definition" {
        let p : RenameParams = { TextDocument = { Uri = filePathToUri pathTest}
                                 Position = { Line = 2; Character = 4}
                                 NewName = "y" }
        let res = server.TextDocumentRename p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res.DocumentChanges with
          | None -> failtest "No changes"
          | Some result ->
            // TODO
            // Expect.equal result.Length 2 "Rename has all changes"
            // Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Program.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 7; Character = 12 }; End = {Line = 7; Character = 13 } }) ) "Rename contains changes in Program.fs"
            // Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Test.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 5 } }) ) "Rename contains changes in Test.fs"
            ()
      }

    ]
  )


///GoTo tests
let gotoTest () =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")
  serverTest path defaultConfigDto (fun (server, event) ->
    do waitForWorkspaceFinishedParsing event
    let definitionPath = Path.Combine(path, "Definition.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument definitionPath }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let externalPath = Path.Combine(path, "External.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument externalPath }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let path = Path.Combine(path, "Library.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "GoTo Tests" [
      testAsync "Go-to-definition on external symbol (System.Net.HttpWebRequest)" {
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = filePathToUri externalPath }
          Position = { Line = 4; Character = 30 }
        }

        let! res = server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some (GotoResult.Multiple _)) -> failtest "Should only get one location"
        | Result.Ok (Some (GotoResult.Single r)) when r.Uri.EndsWith("startup") ->
          failtest "Should not generate the startup dummy file"
        | Result.Ok (Some (GotoResult.Single r)) ->
          Expect.stringEnds r.Uri ".cs" "should have generated a C# code file"
          Expect.stringContains r.Uri "System.Net.HttpWebRequest" "The generated file should be for the HttpWebRequest type"
          () // should
      }

      testAsync "Go-to-definition on external namespace (System.Net) should error when going to a namespace " {
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = filePathToUri externalPath }
          Position = { Line = 2; Character = 15 }
        }

        let! res = server.TextDocumentDefinition p
        match res with
        | Result.Error e ->
          Expect.equal "Could not find declaration" e.Message "Should report failure for navigating to a namespace"
        | Result.Ok r -> failtestf "Declaration request should not work on a namespace, instead we got %A" r
      }

      test "Go-to-definition" {
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = filePathToUri path}
            Position = { Line = 2; Character = 29}}
        let res = server.TextDocumentDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 16 }} "Result should have correct range"
      }

      test "Go-to-definition on custom type binding" {
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = filePathToUri path}
            Position = { Line = 4; Character = 24}}
        let res = server.TextDocumentDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 6; Character = 4 }; End = {Line = 6; Character = 19 }} "Result should have correct range"
      }

      test "Go-to-type-definition" {
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = filePathToUri path}
            Position = { Line = 4; Character = 24}}
        let res = server.TextDocumentTypeDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 4; Character = 5 }; End = {Line = 4; Character = 6 }} "Result should have correct range"
      }

      test "Go-to-implementation-on-interface-definition" {
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = filePathToUri definitionPath}
            Position = { Line = 8; Character = 11}}
        let res = server.TextDocumentImplementation p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtest "Request failed"
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Single res -> failtest "Should be multiple GotoResult"
          | GotoResult.Multiple res ->
            // TODO???
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 7; Character = 8 }; End = {Line = 7; Character = 30 }}) "First result should be in Library.fs"
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 13; Character = 14 }; End = {Line = 13; Character = 36 }}) "Second result should be in Library.fs"
            ()
        }
    ]
  )

///Global list of tests
let tests =
   [
    initTests
    basicTests
    codeLensTest
    documentSymbolTest
    autocompleteTest
    renameTest
    gotoTest
    documentionTest
  ]
