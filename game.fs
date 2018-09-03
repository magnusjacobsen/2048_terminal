open System
open System.Windows.Forms
open System.Web.UI.WebControls

type state = S of int array list

type action = Up | Right | Down | Left

let rng = Random ()

let getEmptyBoard () = S [
                          [|0;0;0;0|];
                          [|0;0;0;0|];
                          [|0;0;0;0|];
                          [|0;0;0;0|] ]

let getEmpties board = 
  let rec aux1 row es = function
  | [] -> es
  | (x:int array)::xs ->
      let mutable lst = []
      for i in 0 .. x.Length - 1 do
        if x.[i] = 0 then lst <- (int i,row)::lst
      aux1 (row + 1) (lst @ es) xs
  aux1 0 [] board

let getRandomEmptyPos (lst : 'a list) = lst.[rng.Next (lst.Length)]

let randomSpawn (S s) =
  let num = rng.Next (2) + 1
  let x,y = (getRandomEmptyPos << getEmpties) s
  s.[y].[x] <- (num + num)
  S s

let welcomeText () =
  printfn "**********************************************"
  printfn "** Welcome to the amazing 2048 console game **"
  printfn "**********************************************"
  printfn "You can press the arrow keys, to do a legal move. If the action is not legal, nothing will happen"
  printfn "You can exit the game anytime, by pressing 'x'\n\n"

let start s = 
  Console.ForegroundColor <- ConsoleColor.Black
  Console.BackgroundColor <- ConsoleColor.White
  Console.Clear ()
  welcomeText ()
  (randomSpawn << randomSpawn) s

let rec pad num =
  if num = 0 then "    "    
  else if num < 10 then "  " + string num + " "
  else if num < 100 then " " + string num + " "
  else if num < 1000 then " " + string num
  else string num

let setColor v = 
  match v with
  | 2 -> Console.BackgroundColor <- ConsoleColor.Gray
  | 4 -> Console.BackgroundColor <- ConsoleColor.DarkGray
         Console.ForegroundColor <- ConsoleColor.White
  | 8 -> Console.BackgroundColor <- ConsoleColor.Yellow
  | 16 -> Console.BackgroundColor <- ConsoleColor.DarkYellow
  | 32 -> Console.BackgroundColor <- ConsoleColor.Blue
          Console.ForegroundColor <- ConsoleColor.White
  | 64 -> Console.BackgroundColor <- ConsoleColor.DarkBlue
          Console.ForegroundColor <- ConsoleColor.White
  | 128 -> Console.BackgroundColor <- ConsoleColor.Cyan
  | 256 -> Console.BackgroundColor <- ConsoleColor.DarkCyan
           Console.ForegroundColor <- ConsoleColor.White
  | 512 -> Console.BackgroundColor <- ConsoleColor.Magenta
  | 1024 -> Console.BackgroundColor <- ConsoleColor.DarkMagenta
            Console.ForegroundColor <- ConsoleColor.White
  | 2048 -> Console.BackgroundColor <- ConsoleColor.Green  
            Console.ForegroundColor <- ConsoleColor.White
  | 4096 -> Console.BackgroundColor <- ConsoleColor.DarkGreen
            Console.ForegroundColor <- ConsoleColor.White
  | x when x > 4096 -> Console.BackgroundColor <- ConsoleColor.Red
                       Console.ForegroundColor <- ConsoleColor.White
  | _ -> ()

let resetColor () = 
  Console.ForegroundColor <- ConsoleColor.Black
  Console.BackgroundColor <- ConsoleColor.White

let rec printState (S s) =
  match s with
  | [] -> ()
  | x::xs ->
      printfn "     ____   ____   ____   ____"
      printf "    "
      Array.iter (fun e -> 
        printf "%s" "|"
        setColor e
        printf "%s" (pad e)
        resetColor ()
        printf "| "
        ) x
      printfn ""
      printfn "     ‾‾‾‾   ‾‾‾‾   ‾‾‾‾   ‾‾‾‾"
      printState (S xs)

// moves and merges, returns the resulting state
let result (S s) action =
  let mutable (S b) = getEmptyBoard ()
  match action with
  | Up    ->
    for x in 0 .. 3 do
      let mutable lastPos = 0
      let mutable former = s.[lastPos].[x]
      b.[lastPos].[x] <- former
      for y in 1 .. 3 do
        let current = s.[y].[x]
        if current <> 0 then
          if former = current then
            b.[lastPos].[x] <- current + current
            former <- -1
          else
            if former = 0 then
              b.[lastPos].[x] <- current
            else
              b.[lastPos + 1].[x] <- current
              lastPos <- lastPos + 1
            former <- current            
  | Right ->
    for y in 0 .. 3 do
      let mutable lastPos = 3
      let mutable former = s.[y].[lastPos]
      b.[y].[lastPos] <- former
      for x in 2 .. -1 .. 0 do
        let current = s.[y].[x]
        if current <> 0 then
          if former = current then
            b.[y].[lastPos] <- current + current
            former <- -1
          else
            if former = 0 then
              b.[y].[lastPos] <- current
            else
              b.[y].[lastPos - 1] <- current
              lastPos <- lastPos - 1
            former <- current    
  | Down  ->
    for x in 0 .. 3 do
      let mutable lastPos = 3
      let mutable former = s.[lastPos].[x]
      b.[lastPos].[x] <- former
      for y in 2 .. -1 .. 0 do
        let current = s.[y].[x]
        if current <> 0 then
          if former = current then
            b.[lastPos].[x] <- current + current
            former <- -1
          else
            if former = 0 then
              b.[lastPos].[x] <- current
            else
              b.[lastPos - 1].[x] <- current
              lastPos <- lastPos - 1
            former <- current    
  | Left  -> 
    for y in 0 .. 3 do
      let mutable lastPos = 0
      let mutable former = s.[y].[lastPos]
      b.[y].[lastPos] <- former
      for x in 1 .. 3 do
        let current = s.[y].[x]
        if current <> 0 then
          if former = current then
            b.[y].[lastPos] <- current + current
            former <- -1
          else
            if former = 0 then
              b.[y].[lastPos] <- current
            else
              b.[y].[lastPos + 1] <- current
              lastPos <- lastPos + 1
            former <- current    
  S b // returns the resulting board state

let move (S s) action =
  let (S res) = result (S s) action
  let res' = if s = res then S res
             else randomSpawn (S res)
  res'

let rec terminalTest state =
  let left = result state Left
  let right = result state Right
  let up = result state Up
  let down = result state Down
  left = state && right = state && up = state && down = state

let countScore (S s) =
  let mutable score = 0
  let rec aux = function
  | [] -> score
  | x::xs -> Array.iter (fun e -> score <- score + e) x
             aux xs
  aux s           

let printStatus clear state =
  let up = if terminalTest state then 14 else 13
  if clear then Console.SetCursorPosition(Console.CursorLeft - 1, Console.CursorTop - up);
  printState state
  printfn "                                                                           "
  if terminalTest state then 
    printfn "%s" ("Game over! You did really well. Final score: " + (string (countScore state)))

let play () = 
  let mutable b = (start << getEmptyBoard) ()
  let mutable isRunning = true
  printStatus false b

  while isRunning do

    match Console.ReadKey().Key.GetHashCode() with
    | 38 -> b <- move b Up // UP
    | 37 -> b <- move b Left // LEFT
    | 39 -> b <- move b Right // RIGHT
    | 40 -> b <- move b Down // DOWN
    | 88 -> Console.SetCursorPosition(Console.CursorLeft - 1, Console.CursorTop);
            printfn "Goodbye. Quitter!" // x
            isRunning <- false
    | _  -> ()
    if isRunning then printStatus true b


[<EntryPoint>]
let main argv =
  play ()
  0

