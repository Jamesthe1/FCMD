namespace FCMD

open System
open System.Text.RegularExpressions
open Printf

open FCMD.Types

module Command =
    let internal splitInput (input: string) =
        let quotes = String.filter (fun c -> c = '\"' ) input
                     |> String.length
        if quotes % 2 = 1 then
            failwith "Uneven quotes in command"

        input.Split '\"'
        |> Array.mapi (fun i s -> if i%2 = 1 then [|s|] else s.Split " ")
        |> Array.concat
        |> Array.filter (fun s -> s <> "")

    let internal printColor (printFunc: TextWriterFormat<string -> unit> -> string -> unit) color text =
        Console.ForegroundColor <- color
        printFunc "%s" text
        Console.ResetColor()

    let internal printfColor =
        printColor printf

    let internal printfnColor =
        printColor printfn

    let internal matchWithKeyword input (commands: FuncCommands) =
        try
            if input.Equals "" then
                failwith "No input provided"

            let args = splitInput input

            if commands.ContainsKey args.[0] then
                commands.[args.[0]].Function args.[1..]
            else
                failwith $"{args.[0]} is not a valid command"
        with
        | :? Exception as e -> printfnColor ConsoleColor.Red $"Error: {e.Message}"

    let internal listCommands (commands: FuncCommands) =
        Map.iter (fun k v -> printf "==\t"
                             printfColor ConsoleColor.Green k
                             printfn ": %s" v.Description
                             Array.iter (fun (name, desc, required) -> printf "-\t\t<"
                                                                       printfColor ConsoleColor.DarkGreen name
                                                                       printf ">: "
                                                                       if required then
                                                                           printfColor ConsoleColor.Yellow "REQUIRED. "
                                                                       printfn "%s" desc
                                        ) v.ArgumentDescriptions
                             printf "\n"
                 ) commands
                

    let internal mergeMaps: (FuncCommands -> FuncCommands -> FuncCommands) =
        Map.fold (fun s k v -> Map.add k v s)

    let internal regexMatches str expression =
        Regex.IsMatch(str, expression, RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

    let internal regexesMatchAny str expressions =
        Array.fold (fun s e -> s || regexMatches str e) false expressions

    /// <summary>
    /// Fails when no arguments are given. Useful for functions that require arguments.
    /// </summary>
    /// <param name="aList">The list of arguments</param>
    let failOnNoArg (aList: string[]) =
        if aList.Length = 0 then
            failwith "No arguments given"

    /// <summary>
    /// Fails when the number of arguments are less than the desired count.
    /// </summary>
    /// <param name="aList">The list of arguments</param>
    /// <param name="count">The number of desired arguments</param>
    let failOnFewerArgs (aList: string[]) count =
        if aList.Length < count then
            failwith "Not enough arguments given"

    /// <summary>Starts a terminal session.</summary>
    /// <param name="getPrefix">A function that returns a string that will be printed before each input</param>
    /// <param name="getCommands">A function that returns a map of commands with strings as keys, void functions taking string arrays as values</param>
    let inputLoop getPrefix (getCommands: unit -> FuncCommands) =
        printfn "Type 'help' for a list of available commands"

        // Currying will only run inner functions once, which is undesired
        let combineWithCommands mapB = mergeMaps (getCommands()) mapB

        let mutable loop = true;
        let mutable baseCommands = FuncCommands [ ("exit", { Function = fun _ -> loop <- false
                                                             Description = "Exits the terminal"
                                                             ArgumentDescriptions = Array.empty
                                                           }
                                                  )
                                                ]
        baseCommands <- Map.add "help" { Function = fun args -> combineWithCommands baseCommands
                                                                |> if args.Length > 0 then
                                                                       Map.filter (fun k _ -> regexesMatchAny k args)
                                                                   else
                                                                       fun c -> c
                                                                |> listCommands
                                         Description = "Shows this message"
                                         ArgumentDescriptions = [| "filters", "Filters commands by name with regexes", false |]
                                       } baseCommands

        while loop do
            printf "%s" (getPrefix ())
            match Console.ReadLine () with
            | input -> combineWithCommands baseCommands
                       |> matchWithKeyword input