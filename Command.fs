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
            failwith "Error: Uneven quotes in command"

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
        | :? Exception as e -> printfn "%s" e.Message

    let internal listCommands (commands: FuncCommands) =
        for c in commands do
            printfn "%s: %s" c.Key c.Value.Description

    /// <summary>Starts a terminal session.</summary>
    /// <param name="prefix">Prefix before input</param>
    /// <param name="commands">A map of commands with strings as keys, void functions taking string arrays as values</param>
    let inputLoop prefix (commands: inref<FuncCommands>) =
        let mutable loop = true;
        let allCommands = FuncCommands [ ("exit", { Function = fun _ -> loop <- false
                                                    Description = "Exits the terminal"
                                                  }
                                         )
                                       ]
                          |> Map.fold (fun s k v -> Map.add k v s) commands

        while loop do
            printf "%s" prefix
            match Console.ReadLine () with
            | "help" -> Map.add "help" { FuncDef.empty with Description = "Shows this message" } allCommands
                        |> listCommands
            | input -> matchWithKeyword input allCommands