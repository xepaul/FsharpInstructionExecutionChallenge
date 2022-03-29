module Tests

open Xunit
open Lib.AST
open Lib.Parser
open Lib.ProgramRunner
open Lib

open Lib.State

[<Fact>]
let ``Test Traverse`` () =
    let x = [1;2;3;4] |> List.traverseResult Ok
    let expected =  Ok [1;2;3;4]
    let isCorrect = x = expected
    Assert.True(isCorrect)
    ()

// [<Fact>]
// let ``Test state1`` =
//     let state = new StateBuilder()
//     let numbers = [ 1; 2; 3; 4 ]

//     let addAmount2 number acc : State<int, int> =
//         state {
//             let! amountToAdd = getState
//             do! modifyState ((+) 0)
//             return (2)
//         }

//     let x =
//         State.eval 3 (numbers |> List.foldMState addAmount2 0)

    


[<Fact>]
let ``Test Traverse2`` () =
    let state = new StateBuilder()
    let numbers = [1;2;3;4] 
    let addAmount2 number acc : State<int,int> = state {
        let! amountToAdd = State.getState
        do! State.modifyState ((+)0)
        return (number + acc)
    }
    let x = State.evalState 1 (numbers |> List.foldMState addAmount2 0)
    let expected =  10
    let isCorrect = x = expected
    Assert.True(isCorrect)
    ()

[<Fact>]
let ``Test Traverse fail`` () =
    let x = [Ok 1;Ok 2; Ok 3;  Error "bad"] |> List.traverseResult id
    let expected =  Error "bad"
    let isCorrect = x = expected
    Assert.True(isCorrect)
    ()

[<Fact>]
let ``Test Add Instruction Parsing`` () =
    let a1 : Address =4u; 
    let a2 : Address =4u; 
    let a3 : Address =1u; 
    let expected = Instruction.Add [ a1; a2;a3  ]
    let result = parseInput parseInstruction "2: Add 4 4 1"     
    let isCorrect = Ok (2u,expected) =   result
    Assert.True(isCorrect)
[<Fact>]
let ``Test MultI nstruction Parsing`` () =
    let a1 : Address =4u; 
    let a2 : Address =4u; 
    let a3 : Address =1u; 
    let expected = Instruction.Mult [ a1; a2;a3  ]
    let result = parseInput parseInstruction "5: Mult 4 4 1"     
    let isCorrect = Ok (5u,expected) =   result
    Assert.True(isCorrect)

[<Fact>]
let ``Test Value Instruction Parsing`` () =
    let expected = Instruction.Value 4
    let result = parseInput parseInstruction "5: Value 4"     
    let isCorrect = Result.Ok (5u,expected) =   result
    Assert.True(isCorrect)

[<Fact>]
let ``Test Value Instruction Parsing Failure`` () =
    let result = parseInput parseInstruction "5: Values 4"     
    let isCorrect = match result with Ok _ -> false | Result.Error _ -> true
    Assert.True(isCorrect)

[<Fact>]
let ``Test short Program1`` () = //         
    let expected = 4;
    let result = "5: Value 4"  |> executeProgramText
     
    Assert.Equal(Result.Ok expected,  result)

[<Fact>]
let ``Test short Program2`` () = //         
    let expected = 4;
    let result = "5: Add 6\n6: Value 4"  |> executeProgramText
     
    Assert.Equal(Result.Ok expected,  result)

[<Fact>]
let ``Test short Program3`` () = //         
    let expected = 11;
    let result = "5: Add 6 7\n6: Value 4\n7: Value 7"  |> executeProgramText
     
    Assert.Equal(Result.Ok expected,  result)
[<Fact>]
let ``Test Example Input File Slow`` () =
    let expected = 4;
    let result = inputExampleFile |> loadfile |> executeProgramTextSlow 
     
    let isCorrect = Result.Ok expected =   result
    Assert.Equal(Result.Ok expected,  result)

[<Fact>]
let ``Test Example Input File Fast`` () =
    let expected = 4;
    let result = inputExampleFile |> loadfile |> executeProgramText
     
    let isCorrect = Result.Ok expected =   result
    Assert.True(isCorrect)

[<Fact>]
let ``Test Input File Fast`` () =
    let expected = 348086909;
    let result = inputFile |> loadfile |> executeProgramText
     
    let isCorrect = Result.Ok expected =   result
    Assert.True(isCorrect)

[<Fact>]
let ``Test Input File Fast Using State Monad2`` () =
    let expected = 348086909;
    let result = inputFile |> loadfile |> executeProgramWithStateMonad
     
    let isCorrect = Result.Ok expected =   result
    Assert.True(isCorrect)

// [<Fact>]
let ``TestInputFileSlow (this is very slow ...)`` () = // 
    let expected = 348086909;
    let result = inputFile |> loadfile |> executeProgramTextSlow 
     
    let isCorrect = Result.Ok expected =   result
    Assert.True(isCorrect)