namespace FCMD

module Types =
    type FuncDef =
        { Function: string[] -> unit
          Description: string
          ArgumentDescriptions: (string * string * bool)[] }
        with
        static member empty = { Function = fun _ -> ()
                                Description = ""
                                ArgumentDescriptions = Array.empty
                              }

    type FuncCommands = Map<string, FuncDef>