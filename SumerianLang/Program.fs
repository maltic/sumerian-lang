

[<EntryPoint>]
let main argv = 
    Seq.iter (printfn "%A") 
        ( (fun _ -> 
                let codes, defs = 
                    System.Console.ReadLine() |> Parse.parse |> Compile.compile
                VM.runVM defs (codes@[VM.Force])) |> Seq.initInfinite )
    0 // return an integer exit code
