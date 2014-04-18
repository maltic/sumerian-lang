

[<EntryPoint>]
let main argv = 
    Seq.iter (printfn "%A") ( (fun _ -> Token.tokenize (System.Console.ReadLine())) |> Seq.initInfinite )
    0 // return an integer exit code
