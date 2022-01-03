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

    let internal matchWithKeyword (input: string) (commands: FuncCommands) =
        let args = input.Split " "
        if commands.ContainsKey (args.[0]) then
            commands.[args.[0]].Function args.[1..]
        else
            printfn "Error: %s is not a valid command" args.[0]

    let internal listCommands (commands: FuncCommands) =
        for c in commands do
            printfn "%s: %s" c.Key c.Value.Description

    /// <summary>Starts a terminal session.</summary>
    /// <param name="prefix">Prefix before input</param>
    /// <param name="commands">A map of commands with strings as keys, void functions taking string arrays as values</param>
    let inputLoop prefix (commands: inref<FuncCommands>) =
        let mutable loop = true;
        while loop do
            printf "%s" prefix
            match Console.ReadLine () with
            | "exit" -> loop <- false
            | "help" -> Map.add "exit" { FuncDef.empty with Description = "Exits the terminal" } commands
                        |> Map.add "help" { FuncDef.empty with Description = "Shows this message" }
                        |> listCommands
            | input -> matchWithKeyword input commands