

[<EntryPoint>]
let main argv = 
    Seq.iter (printfn "%A") 
        ( (fun _ -> System.Console.ReadLine() |> Token.tokenize |> Interpret.evaluate ) |> Seq.initInfinite )
    0 // return an integer exit code
