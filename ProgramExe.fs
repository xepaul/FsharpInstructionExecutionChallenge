open Lib.ProgramRunner

    [<EntryPoint>]
    let main argv =
        let x =inputFile |> loadfile |> executeProgramText
        let result = 
            match  x with 
            | Ok v -> sprintf "%A" v
            | Error e -> sprintf "failed %s" e
        printfn "%s" result
        0