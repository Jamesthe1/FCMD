namespace FCMD

open System

module Command =
    type FuncCommands = Map<string, string[] -> unit>

    let internal matchWithKeyword (input: string) (commands: FuncCommands) =
        let args = input.Split " "
        if commands.ContainsKey (input) then
            commands.[args.[0]] args.[1..]
        else
            printfn "Error: %s is not a valid command" args.[0]

    /// <summary>Starts a terminal session.</summary>
    /// <param name="prefix">Prefix to input</param>
    /// <param name="commands">A map of commands with strings as keys, void functions taking string arrays as values</param>
    let inputLoop prefix (commands: inref<FuncCommands>) =
        let mutable loop = true;
        while loop do
            printf "%s" prefix
            match Console.ReadLine () with
            | "exit" -> loop <- false
            | input -> matchWithKeyword input commands