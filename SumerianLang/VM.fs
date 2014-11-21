module VM

type OpCode = 
    | Int of int
    | Float of float
    | Bool of bool
    | Block of OpCode list
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulus
    | XOR
    | OR
    | AND
    | And
    | Or
    | Not
    | Eq
    | Lt
    | Gt
    | Cons
    | Decons
    | ToInt
    | ToFloat
    | Call of int
    | Force


// predefined functions
module BuiltinFunctions = 
    let add = function
        | Int(av) :: Int(bv) :: t -> Int(av + bv) :: t
        | Float(av) :: Float(bv) :: t -> Float(av + bv) :: t
        | Block(av) :: Block(bv) :: t -> Block(bv @ av) :: t
        | _ -> failwith "Invalid arguments to function '+'"
    let mult = function
        | Int(av) :: Int(bv) :: t -> Int(av * bv) :: t
        | Float(av) :: Float(bv) :: t -> Float(av * bv) :: t
        | _ -> failwith "Invalid arguments to function '*'"
    let div = function
        | Int(av) :: Int(bv) :: t -> Int(bv / av) :: t
        | Float(av) :: Float(bv) :: t -> Float(bv / av) :: t
        | _ -> failwith "Invalid arguments to function '/'"
    let sub = function
        | Int(av) :: Int(bv) :: t -> Int(bv - av) :: t
        | Float(av) :: Float(bv) :: t -> Float(bv - av) :: t
        | _ -> failwith "Invalid arguments to function '-'"
    let modulus = function
        | Int(av) :: Int(bv) :: t -> Int(bv % av) :: t
        | _ -> failwith "Invalid arguments to function '%'"
    let cons = function
        | v :: Block(block) :: t -> Block(v :: block) :: t
        | _ -> failwith "Invalid arguments for function 'cons'"
    let decons = function
        | Block(h :: t) :: rest -> h :: Block(t) :: rest
        | _ -> failwith "Invalid arguments for function 'decons'"
    let eq = function
        | a :: b :: t -> Bool(a = b) :: t
        | _ -> failwith "Invalid arguments for function '='"
    let not = function
        | Bool(v) :: t -> Bool(not v) :: t
        | _ -> failwith "Invalid arguments for function 'not'"
    let lt = function
        | Int(av) :: Int(bv) :: t -> Bool(bv < av) :: t
        | Float(av) :: Float(bv) :: t -> Bool(bv < av) :: t
        | _ -> failwith "Invalid arguments for function '<'"
    let gt = function
        | Int(av) :: Int(bv) :: t -> Bool(bv > av) :: t
        | Float(av) :: Float(bv) :: t -> Bool(bv > av) :: t
        | _ -> failwith "Invalid arguments for function '>'"
    let logicalAnd = function
        | Bool(av) :: Bool(bv) :: t -> Bool(bv && av) :: t
        | _ -> failwith "Invalid arguments for function 'and'"
    let logicalOr = function
        | Bool(av) :: Bool(bv) :: t -> Bool(bv || av) :: t
        | _ -> failwith "Invalid arguments for function 'or'"
    let xor = function
        | Int(av) :: Int(bv) :: t -> Int(bv ^^^ av) :: t
        | _ -> failwith "Invalid arguments for function 'xor'"
    let bitwiseAnd = function
        | Int(av) :: Int(bv) :: t -> Int(bv &&& av) :: t
        | _ -> failwith "Invalid arguments for function 'bitwise and'"
    let bitwiseOr = function
        | Int(av) :: Int(bv) :: t -> Int(bv ||| av) :: t
        | _ -> failwith "Invalid arguments for function 'bitwise or'"
    let toInt = function
        | Float(v) :: t -> Int(int v) :: t
        | _ -> failwith "Invalid arguments for function 'to int'"
    let toFloat = function
        | Int(v) :: t -> Float(float v) :: t
        | _ -> failwith "Invalid arguments for function 'to float'"


let rec applyOpCode (definitions:OpCode list[]) = function
    | Add :: stack -> BuiltinFunctions.add stack
    | Subtract :: stack -> BuiltinFunctions.sub stack
    | Multiply :: stack -> BuiltinFunctions.mult stack
    | Divide :: stack -> BuiltinFunctions.div stack
    | Modulus :: stack -> BuiltinFunctions.modulus stack
    | XOR :: stack -> BuiltinFunctions.xor stack
    | OR :: stack -> BuiltinFunctions.bitwiseOr stack
    | AND :: stack -> BuiltinFunctions.bitwiseAnd stack
    | And :: stack -> BuiltinFunctions.logicalAnd stack
    | Or :: stack -> BuiltinFunctions.logicalOr stack
    | Not :: stack -> BuiltinFunctions.not stack
    | Lt :: stack -> BuiltinFunctions.lt stack
    | Gt :: stack -> BuiltinFunctions.gt stack
    | Cons :: stack -> BuiltinFunctions.cons stack
    | Decons :: stack -> BuiltinFunctions.decons stack
    | ToInt :: stack -> BuiltinFunctions.toInt stack
    | ToFloat :: stack -> BuiltinFunctions.toFloat stack
    | Call(id) :: stack -> applyOpCode definitions (List.rev definitions.[id] @ stack)
    | v -> failwith (sprintf "Impossible: %A" v) //good to check my assumptiopns

let vmExecutionStep definitons stack = function
    | Force -> applyOpCode definitons stack // evaluate thunk at top of stack
    | func -> func :: stack // lazy, everything is a function/thunk

let runVM definitions = List.fold (fun state elem -> vmExecutionStep definitions state elem) []
    