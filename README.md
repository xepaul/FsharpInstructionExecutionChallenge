
![CI](https://github.com/xepaul/FsharpInstructionExecutionChallenge/actions/workflows/dotnet.yml/badge.svg)

Your challenge is to write F#/C# (F# preferred) code that can correctly evaluate a list of instructions:

    - Every instruction has a label specified by number followed by a colon e.g. "4:"

    - There are 3 types of instruction:

      1) Value x         - Returns the value x
      2) Add <labels>    - Evaluates the instruction at each label in <labels> and adds the result together
      3) Mult <labels>   - Evaluates the instruction at each label in <labels> and multiplies the results together

      <labels> is a list of one or more numbers that refer to the label of other instructions in the input.

    - The overall result for a set of instruction is the result of evaluating the first instruction.

For example, given the input:

    0: Add 4 4 1
    1: Mult 6 2
    2: Value -3
    3: Add 6 1 2
    4: Value 5
    6: Value 2

The first instruction is "Add 4 4 1", and the overall result is 4, which is produced as follows:

            Add 4 4 1
      ->    Add (5) (5) (Mult 6 2)
      ->    Add (5) (5) (Mult (2) (-3))
      ->    Add (5) (5) (-6)
      ->    Add (10) (-6)
      ->    (4)

Challenge:
    What is the overall result of evaluating the instructions in the input.txt file?

Further guidance:
    - As a rough guide, we expect you to spend around 1 hour on this exercise.
    - Make sure to document any assumptions that you make.
    - Provide your answer, along with the code you wrote to get the result.

    


Commands
dotnet run 


Commands for running tests
dotnet test -v n
dotnet test -v quiet --nologo -l:"console;verbosity=normal"   -- only test output

Fsi Commands
dotnet fsi
#r "nuget: FParsec, 1.1.1";; 
#load "./Program.fs";; 
open Lib.ProgramExecution;;       
#time;;
open Lib.ProgramRunner;;

inputExampleFile |> loadfile |> executeProgramText
inputFile |> loadfile |> executeProgramText

"5: Add 6 7\n6: Value 4\n6: Value 7"  |> executeProgramText;;

inputExampleFile |> loadfile |> executeProgramTextSlowState