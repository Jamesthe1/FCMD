namespace FCMD

open System

module Command =
    type FuncDef =
        { Function: string[] -> unit; Description: string }
        with
        static member empty = { Function = fun _ -> ()
                                Description = ""
                              }
    type FuncCommands = Map<string, FuncDef>

    let internal splitInput (input: string) =
        let quotes = String.filter (fun c -> c = '\"' ) input
                     |> String.length
        if quotes % 2 = 1 then
            failwith "Uneven quotes in command"

        input.Split '\"'
        |> Array.mapi (fun i s -> if i%2 = 1 then [|s|] else s.Split " ")
        |> Array.concat
        |> Array.filter (fun s -> s <> "")

    let internal matchWithKeyword input (commands: FuncCommands) =
        try
            let args = splitInput input

            if commands.ContainsKey (args.[0]) then
                commands.[args.[0]].Function args.[1..]
            else
                printfn "Error: %s is not a valid command" args.[0]
        with
        | :? Exception as e -> printfn "Error: %s" e.Message

    let internal listCommands (commands: FuncCommands) =
        for c in commands do
            printfn "-\t%s: %s" c.Key c.Value.Description

    let internal mergeMaps: (FuncCommands -> FuncCommands -> FuncCommands) =
        Map.fold (fun s k v -> Map.add k v s)

    /// <summary>
    /// Fails when no arguments are given. Useful for functions that require arguments.
    /// </summary>
    /// <param name="aList">The list of arguments</param>
    let failOnNoArg (aList: string[]) =
        if aList.Length = 0 then
            failwith "No arguments given"

    /// <summary>Starts a terminal session.</summary>
    /// <param name="getPrefix">A function that returns a string that will be printed before each input</param>
    /// <param name="getCommands">A function that returns a map of commands with strings as keys, void functions taking string arrays as values</param>
    let inputLoop getPrefix (getCommands: unit -> FuncCommands) =
        printfn "Type 'help' for a list of available commands"
        let mutable loop = true;
        let baseCommands = FuncCommands [ ("exit", { Function = fun _ -> loop <- false
                                                     Description = "Exits the terminal"
                                                   }
                                          )
                                        ]
        // Currying will only run inner functions once, which is undesired
        let combineWithCommands mapB = mergeMaps (getCommands()) mapB

        while loop do
            printf "%s" (getPrefix ())
            match Console.ReadLine () with
            | "help" -> Map.add "help" { FuncDef.empty with Description = "Shows this message" } baseCommands
                        |> combineWithCommands
                        |> listCommands
            | input -> combineWithCommands baseCommands
                       |> matchWithKeyword input