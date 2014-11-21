module Compile

let mergeMaps m m' = 
    Map.fold (fun state key value -> 
        if Map.containsKey key state |> not then Map.add key value state else state) m m'


/// Lift all definitons from the parse term stream into a map
let liftDefinitions = 
    let rec lifer path defs filtered = function
        | Parse.Definition(name, body) :: tail ->
            if Map.containsKey (name::path) defs then
                failwith "Redefinition!"
            else
                let interiorDefs, liftedBody = lifer (name::path) Map.empty [] body
                lifer 
                    path 
                    (mergeMaps (Map.add (name::path) liftedBody defs) interiorDefs)
                    filtered 
                    tail
        | [] -> defs, List.rev filtered
        | head :: tail -> lifer path defs (head::filtered) tail
    lifer [] Map.empty []


let rec toOpCode path defs = function
    | Parse.Id(id) -> VM.Call(Map.find (id::path) defs)
    | Parse.Bool(b) -> VM.Bool(b)
    | Parse.Int(i) -> VM.Int(i)
    | Parse.Float(f) -> VM.Float(f)
    | Parse.Block(b) -> VM.Block(List.map (toOpCode path defs) b)
    | Parse.Definition(_, _) -> failwith "Invalid definition"

let compileLayer path defs = List.map (toOpCode path defs)

let compile stream =
    let defs, rem = liftDefinitions stream
    let pathToCode = 
        Seq.zip (seq { for k, _ in Map.toSeq defs -> k }) (Seq.initInfinite id)
        |> Map.ofSeq
    let codeToValue = 
        Seq.zip (Seq.initInfinite id) (seq { for k, v in Map.toSeq defs -> compileLayer k pathToCode v })
        |> Map.ofSeq
    compileLayer [] pathToCode rem, codeToValue
