## FuncDef
- Function: string[] -> unit
  - The function that is called when the command is executed
- Description: string
  - A brief description of the function

## FuncCommands
A Map with key of type string, value of type FuncDef

## inputLoop (prefix: string) (commands: inref<FuncCommands>)
Starts a terminal session
- prefix: Text that is printed behind the user's input
- commands: A map of commands that the user can use
