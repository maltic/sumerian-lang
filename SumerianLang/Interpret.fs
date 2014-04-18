module Interpret

open AST

let sameType a b = 
    match (a, b) with
    | Int(_), Int(_) -> true
    | Float(_), Float(_) -> true
    | Char(_), Char(_) -> true
    | String(_), String(_) -> true
    | Bool(_), Bool(_) -> true
    | Block(_), Block(_) -> true
    | _ -> false

type Scope(parent:Scope option) = 
    let map = System.Collections.Generic.Dictionary<string, AST.Node>()
    member this.Find(k:string) = 
        if map.ContainsKey(k) then Some(map.[k])
        elif Option.isSome parent then parent.Value.Find(k)
        else None
    member this.Add(k, v) = map.Add(k, v)


// predefined functions
let sAdd = function
    | Int(av) :: Int(bv) :: t -> Int(av + bv) :: t
    | Float(av) :: Float(bv) :: t -> Float(av + bv) :: t
    | String(av) :: String(bv) :: t -> String(bv + av) :: t
    | Block(Bracket.Paren, av) :: Block(Bracket.Paren, bv) :: t -> Block(Bracket.Paren, bv @ av) :: t
    | s -> Error("Invalid arguments to function '+'") :: s

let sMult = function
    | Int(av) :: Int(bv) :: t -> Int(av * bv) :: t
    | Float(av) :: Float(bv) :: t -> Float(av * bv) :: t
    | s -> Error("Invalid arguments to function '*'") :: s

let sDiv = function
    | Int(av) :: Int(bv) :: t -> Int(bv / av) :: t
    | Float(av) :: Float(bv) :: t -> Float(bv / av) :: t
    | s -> Error("Invalid arguments to function '/'") :: s

let sSub = function
    | Int(av) :: Int(bv) :: t -> Int(bv - av) :: t
    | Float(av) :: Float(bv) :: t -> Float(bv - av) :: t
    | s -> Error("Invalid arguments to function '-'") :: s

let sMod = function
    | Int(av) :: Int(bv) :: t -> Int(bv % av) :: t
    | s -> Error("Invalid arguments to function '%'") :: s

let sDef stack (env:Scope) = 
    match stack with
    | v :: Id(n) :: t ->
        env.Add(n, v)
        t
    | s -> Error("Invalid arguments for function 'def'") :: s

let sSwap = function
    | a :: b :: t -> b :: a :: t
    | s -> Error("Invalid arguments for function 'swap'") :: s

let sScrunch stack = [Block(Bracket.Paren, stack)]

let sExplode = function
    | Block(Bracket.Paren, block) :: t -> block @ t
    | s -> Error("Invalid arguments for function 'explode'") :: s

let sCons = function
    | v :: Block(Bracket.Paren, block) :: t -> Block(Bracket.Paren, v :: block) :: t
    | s -> Error("Invalid arguments for function 'cons'") :: s

let sDecons = function
    | Block(Bracket.Paren, h :: t) :: rest -> h :: Block(Bracket.Paren, t) :: rest
    | s -> Error("Invalid arguments for function 'decons'") :: s

let sEquals = function
    | a :: b :: t -> Bool(a = b) :: t
    | s -> Error("Invalid arguments for function '='") :: s



let evaluate stream = 

    /// Attempts to call a built-in function given the current stack
    let rec keyword stack env = function
        | "+" -> Some(sAdd stack)
        | "*" -> Some(sMult stack)
        | "/" -> Some(sDiv stack)
        | "-" -> Some(sSub stack)
        | "%" -> Some(sMod stack)
        | "def" -> Some(sDef stack env)
        | "swap" -> Some(sSwap stack)
        | "scrunch" -> Some(sScrunch stack)
        | "explode" -> Some(sExplode stack)
        | "if" -> Some(sIf stack env)
        | "cons" -> Some(sCons stack)
        | "decons" -> Some(sDecons stack)
        | "eval" -> Some(sEval stack env)
        | "=" -> Some(sEquals stack)
        | _ -> None
    // predefined functions that need to call helper
    and sIf stack env = 
        match stack with
        | Block(Bracket.Paren, elseb) :: Block(Bracket.Paren, thenb) :: Bool(cond) :: t ->
            helper t env ((if cond then thenb else elseb) @ t)
        | s -> Error("Invalid arguments for function 'if'") :: s

    and sEval stack env = 
        match stack with
        | Block(Bracket.Paren, strm) :: rest -> helper rest (Scope(Some(env))) strm
        | s -> Error("Invalid arguments for function 'evaluate'") :: s

    and helper stack (env:Scope) = function
        | Id(id) :: t ->
            match keyword stack env id with
            | Some(Error(estr) :: _) -> failwith estr
            | Some(s) -> helper s env t
            | None ->
                match env.Find(id) with
                | Some(v) -> helper (v::stack) env t
                | None -> helper (Id(id) :: stack) env t
        | h :: t -> helper (h::stack) env t
        | [] -> stack


    helper [] (Scope(None)) stream