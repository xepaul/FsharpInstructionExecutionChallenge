namespace Lib


// https://gist.github.com/jwosty/5338fce8a6691bbd9f6f
[<Struct>]
type State<'s, 'a> = State of ('s -> struct ('a * 's))

module State =
    let inline runState state x = let (State (f)) = x in f state

    let inline evalState state x =
        let struct (a, b) = let (State (f)) = x in f state //|> fst
        a

    let getState = State(fun s -> s, s)
    let inline modifyState f = State(fun s -> (), f s)
    let inline put newState = State(fun _ -> (), newState)

    let inline map f s =
        State
            (fun (state: 's) ->
                let struct (x, state) = runState state s
                f x, state)

    let inline withModify f s =
        State
            (fun (state: 's) ->
                let struct (x, state) = runState state s
                x, f x state)

    let inline bind (f: 'a -> State<'s, 'b>) (x: State<'s, 'a>) =
        State
            (fun state ->
                let struct ((result: 'a), state) = runState state x
                runState state (f result))

    let inline pure' a = State(fun s -> a, s)
    let inline return' a = State(fun s -> a, s)

/// The state monad passes around an explicit internal state that can be
/// updated along the way. It enables the appearance of mutability in a purely
/// functional context by hiding away the state when used with its proper operators
/// (in StateBuilder()). In other words, you implicitly pass around an implicit
/// state that gets transformed along its journey through pipelined code.
type StateBuilder() =
    member this.Zero() = State(fun s -> (), s)
    member this.Return x = State(fun s -> x, s)
    member inline this.ReturnFrom(x: State<'s, 'a>) = x

    member this.Bind(x, f) : State<'s, 'b> =
        State
            (fun state ->
                let struct ((result: 'a), state) = State.runState state x
                State.runState state (f result))

    member this.Combine(x1: State<'s, 'a>, x2: State<'s, 'b>) =
        State
            (fun state ->
                let struct (result, state) = State.runState state x1
                State.runState state x2)

    member this.Delay f : State<'s, 'a> = f ()

    member this.For(seq, (f: 'a -> State<'s, 'b>)) =
        seq
        |> Seq.map f
        |> Seq.reduceBack (fun x1 x2 -> this.Combine(x1, x2))

    member this.While(f, x) =
        if f () then
            this.Combine(x, this.While(f, x))
        else
            this.Zero()

[<AutoOpen>]
[<RequireQualifiedAccess>]
module List =
    // traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    // https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:traverse
    // Map each element of a structure to an action, evaluate these actions from left to right, and collect the results.
    let rec traverseResult f list =
        // define the bind operator for result
        let (>>=) x f = Result.bind f x

        match list with
        | [] -> Ok []
        | head :: tail ->
            f head
            >>= (fun h -> traverseResult f tail >>= (fun t -> Ok <| h :: t))

    let state = new StateBuilder()

    //foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
    let inline foldMState (f: 'b -> 'a -> State<'s, 'b>) (seed: 'b) (l: 'a list) : State<'s, 'b> =
        let rec r s l : State<'s, 'b> =
            state {
                match l with
                | [] -> return s
                | x :: xs ->
                    let! answer = f s x
                    let! rest = r answer xs
                    return rest
            }

        r seed l

    /// foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
    /// Map each element of the structure to a monoid, and combine the results.
    let inline foldMapState ( append:'b -> 'b -> 'b) (empty: 'b)  (f:  'a -> 'b) (l: 'a list) : 'b =
        List.fold (fun acc v -> append (f v) acc) empty l

    /// foldMapM :: (Monad m, Monoid w, Foldable t) => (a -> m w) -> t a -> m w
    /// Extend foldMap to allow side effects.
    let inline foldMapMState ( append:'b -> 'b -> 'b) (empty: 'b)  (f:  'a -> State<'s, 'b>) (l: 'a list) : State<'s, 'b> =
            foldMState
                        (fun acc addr ->
                            f addr
                            |> State.map (fun v -> append acc v))
                        empty
                        l

    let reduceState (f: 'a -> 'a -> 'a) (l: State<'s, 'a> list) : State<'s, 'a> =
        state {

            let! seed' = l.[0]
            let! aa = foldMState (fun acc addr -> addr |> State.map (fun a -> f a acc)) seed' (l |> List.skip 1)
            return aa
        }

    let inline product (source: List< ^a >) : ^a when ^T: (static member (*) : ^T * ^T -> ^T) =
        source |> List.reduce (*)

    let inline mSumState (l: State< ^s, ^a > list) : State< ^s, ^a > when ^a: (static member (*) : ^a * ^a -> ^a) =
        l |> reduceState (*)

module AST =
    open System
    type Address = UInt32
    type Value = Int32

    [<RequireQualifiedAccess>]
    type Instruction =
        | Add of Address list
        | Value of Value
        | Mult of Address list

    type Program = Map<Address, Instruction>
    type ExecutableProgram = ExecutableProgram of startAddress: Address * program: Program // should have a private constructor to ensure only correct by construction

module Parser =
    open AST
    open FParsec

    let parseInstructionLabels : Parser<uint32 list, unit> = puint32 |> sepBy1 <| skipChar ' '

    let endOfProgramLine = skipNewline <|> eof
    let parseAddress : Parser<uint32, unit> = (spaces >>. puint32 .>> skipString ": ")

    let parseAdd =
        pipe3
            (pstring "Add" .>> skipString " ")
            parseInstructionLabels
            endOfProgramLine
            (fun _ y _ -> Instruction.Add y)

    let parseMult =
        pipe3
            (pstring "Mult" .>> skipString " ")
            parseInstructionLabels
            endOfProgramLine
            (fun _ y _ -> Instruction.Mult y)

    let parseValue : Parser<(Instruction), unit> =
        pipe3 (pstring "Value" .>> skipString " ") pint32 endOfProgramLine (fun _ y _ -> Instruction.Value y) // should jsut be one value

    let parseInstruction =
        pipe2 parseAddress (parseAdd <|> parseMult <|> parseValue) (fun x y -> x, y)

    let parseInstructionFileMany = many1 parseInstruction

    let parseInput p str =
        match run p str with
        | Success (result, _, _) -> Result.Ok result
        | Failure (errorMsg, _, _) -> Result.Error <| sprintf "Failure: %s" errorMsg

    let parseInstructionMemoryReferences (instructions: (Address * Instruction) list) =
        let program =
            instructions
            |> List.map (fun (addr, i) -> addr, i)
            |> Map.ofList

        let parseIntructionForValidity = // validate the labels of Add's and Mult's are all known address
            let checkInstructionLabels labels ((addr, _) as addrAndInstruction) =
                if (labels
                    |> List.forall (fun label -> Map.containsKey label program)) then
                    Result.Ok addrAndInstruction
                else
                    Result.Error
                    <| sprintf
                        "bad instruction at addr one of the labels points to a no existant memory location %A"
                        addr

            function
            | (_, Instruction.Mult labels) as addrAndInstruction -> checkInstructionLabels labels addrAndInstruction
            | (_, Instruction.Add labels) as addrAndInstruction -> checkInstructionLabels labels addrAndInstruction
            | (_, Instruction.Value _) as addrAndInstruction -> Result.Ok addrAndInstruction

        match instructions with //validate the memory address for instructions validate they all exist (handle empty list, already used many1 but his doesn't return a nonEmpty List )
        | [] -> Result.Error "no instructions"
        | (x :: _) as instructions ->
            let isAnyDuplicateMemoryDeclarations =
                instructions
                |> Seq.map fst
                |> Seq.countBy id
                |> Seq.tryFind (fun x -> (snd x) > 1)

            match isAnyDuplicateMemoryDeclarations with
            | Some (addr, _) ->
                Result.Error
                <| sprintf "duplicate definition of addr %i" addr
            | None ->
                instructions
                |> List.traverseResult parseIntructionForValidity
                |> Result.map (fun program -> ExecutableProgram(fst x, Program program))

    let parsefile =
        parseInput parseInstructionFileMany
        >> Result.bind parseInstructionMemoryReferences

module ProgramExecution =
    open AST
    open System
    open State

    [<RequireQualifiedAccess>]
    //[<Struct>]
    type private MemoryCacheItem =
        | EvaluatedInstruction of eval:Int32
        | UncomputedInstruction of inst:Instruction

    type private RunningProgramCache = Map<Address, MemoryCacheItem>
    type private ValueCache = Map<Address, Value>

    let state = new StateBuilder()

    let executeProgramWithStateMonad (ExecutableProgram (startAddress, program)) =
        let rec evalInstruction addr =
            state {
                let evalLabelsInstruction f s =
                    List.foldMapMState f s evalInstruction 
                match! getState |> map (Map.tryFind addr) with
                | Some value -> return value
                | None ->
                    match Map.find addr program with // this is a partial function but this should be impossible to fail as all the instructions have been parsed, should be correct by construction (constructor should be private to the parser)
                    | Instruction.Value value -> return value
                    | Instruction.Add labels -> return! labels |> evalLabelsInstruction (+) 0
                    | Instruction.Mult labels -> return! labels |> evalLabelsInstruction (*) 1
            }
            |> withModify (Map.add addr) 

        evalState Map.empty (evalInstruction startAddress)

    let executeProgram (ExecutableProgram (startAddress, program)) =
        let rec executeInstruction (addr: Address) (cache: RunningProgramCache) =
            let evaluateInstructionAndUpdateCache cache addr f reducerSeed =
                let second f (a, b) = a, f b //bimap.second

                let saveValueInCache addr (cache, result) =
                    Map.add addr (MemoryCacheItem.EvaluatedInstruction result) cache, result

                List.fold
                    (fun (cache, valueAcc) addr ->
                        executeInstruction addr cache
                        |> second (f valueAcc))
                    (cache, reducerSeed) // reduce while updating instruction evaluation cache
                >> saveValueInCache addr

            match Map.find addr cache with // this is a partial function but this should be impossible to fail as all the instructions have been validated, should be correct by construction (constructor should be private to the parser)
            | MemoryCacheItem.EvaluatedInstruction value
            | MemoryCacheItem.UncomputedInstruction (Instruction.Value value) -> cache, value // don't bother caching the result of a value instruction
            | MemoryCacheItem.UncomputedInstruction (Instruction.Add labels) ->
                evaluateInstructionAndUpdateCache cache addr (+) 0 labels
            | MemoryCacheItem.UncomputedInstruction (Instruction.Mult labels) ->
                evaluateInstructionAndUpdateCache cache addr (*) 1 labels

        program
        |> Map.map (fun _ i -> MemoryCacheItem.UncomputedInstruction i) // introduce a instruction result cache for speed
        |> executeInstruction startAddress
        |> snd


    let executeProgramFast2 (ExecutableProgram (startAddress, program)) =
        let rec executeInstruction (addr: Address) (cache: Map<Address, Value>) =
            let evaluateInstructionAndUpdateCache cache addr f reducerSeed =
                let second f (a, b) = a, f b //bimap.second
                let saveValueInCache addr (cache, result) = Map.add addr result cache, result

                List.fold
                    (fun (cache, valueAcc) addr ->
                        executeInstruction addr cache
                        |> second (f valueAcc))
                    (cache, reducerSeed) // reduce while updating instruction evaluation cache
                >> saveValueInCache addr

            match Map.tryFind addr cache with
            | Some value -> cache, value
            | None ->
                match Map.find addr program with // this is a partial function but this should be impossible to fail as all the instructions have been validated, should be correct by construction (constructor should be private to the parser)
                | (Instruction.Value value) -> cache, value // don't bother caching the result of a value instruction
                | (Instruction.Add labels) -> evaluateInstructionAndUpdateCache cache addr (+) 0 labels
                | (Instruction.Mult labels) -> evaluateInstructionAndUpdateCache cache addr (*) 1 labels

        executeInstruction startAddress Map.empty |> snd

    let executeProgramSlow (ExecutableProgram (startAddress, program)) =
        let rec executeInstruction addr =
            let evaluateInstructionValue f =
                Seq.map executeInstruction >> Seq.reduce f

            match Map.find addr program with // this is a partial function but this should be impossible to fail as all the instructions have been validated, should be correct by construction (constructor should be private to the parser)
            | Instruction.Value value -> value
            | Instruction.Add labels -> labels |> evaluateInstructionValue (+)
            | Instruction.Mult labels -> labels |> evaluateInstructionValue (*)

        executeInstruction startAddress



module ProgramRunner =
    open Parser
    open ProgramExecution
    open State


    let inputFile =
        "./input.txt"

    let inputExampleFile =
        "./inputExample.txt"

    let loadfile f = System.IO.File.ReadAllText(f)

    let executeProgramText = parsefile >> Result.map executeProgram

    let executeProgramTextSlow =
        parsefile >> Result.map executeProgramSlow

    let executeProgramWithStateMonad =
        parsefile >> Result.map executeProgramWithStateMonad

    let executeProgramTextFast2 =
        parsefile >> Result.map executeProgramFast2
