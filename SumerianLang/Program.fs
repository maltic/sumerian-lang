

[<EntryPoint>]
let main argv = 
    Seq.iter (printfn "%A") 
        ( (fun _ -> System.Console.ReadLine() |> Parse.parse |> Compile.compile ) |> Seq.initInfinite )
    0 // return an integer exit code
