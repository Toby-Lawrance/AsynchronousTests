// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Threading
open System.Diagnostics

let log thing =
    printfn "%A" thing
    thing

let sumNumbers list =
    match list with
    | [] -> 0
    | l -> List.reduce (fun x y -> x + y) l

let wrappedSumming list =
    async {
        return sumNumbers list
    }

let timeThing name thing arg =
    let stopWatch = Stopwatch.StartNew()
    let value = thing arg
    stopWatch.Stop()
    printfn "%s took: %fms" name stopWatch.Elapsed.TotalMilliseconds
    value

let doListThingInParallel thing arg =
    let wrappedThing argument =
        async {
            return thing argument
        }
    arg
    |> List.splitInto 8
    |> List.map wrappedThing
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.toList
    |> thing

let doListThingInSerial thing arg =
    thing arg
let generateLongIntList howMany =
    [0..howMany]
    
let generateLongIntArray howMany =
    [|0..howMany|]

let timeFunction name act arg =
    let stopWatch = Stopwatch.StartNew()
    let value = act arg
    value
    stopWatch.Stop()
    printfn "%s: %f" name stopWatch.Elapsed.TotalMilliseconds
    value
    

[<EntryPoint>]
let main argv =
    let firstN = 1000
    
    let normal = (fun n -> generateLongIntList n
                           |> List.splitInto 8)
    
    let arrayed = (fun n -> generateLongIntList n
                           |> List.toArray
                           |> Array.splitInto 8
                           |> Array.map (fun x -> Array.toList x)
                           |> Array.toList)
    
    let fromArray = (fun n -> generateLongIntArray n
                              |> Array.splitInto 8
                              |> Array.map (fun x -> Array.toList x)
                              |> Array.toList)
    
    let genList = generateLongIntList firstN
    
    timeThing "Normal" normal firstN |> ignore
    timeThing "Array" arrayed firstN |> ignore
    timeThing "FromArray" fromArray firstN |> ignore
    
    timeThing (sprintf "Serial %i number sum" firstN) (sumNumbers) genList |> ignore
    timeThing (sprintf "Parallel %i number sum" firstN) (fun x -> doListThingInParallel sumNumbers x) genList |> ignore
    GC.Collect()
    
    let ready = generateLongIntList firstN
                |> List.splitInto 8
    let preSplit =  (fun x -> x
                              |> List.map wrappedSumming
                              |> Async.Parallel
                              |> Async.RunSynchronously
                              |> Array.toList
                              |> sumNumbers
                     )
             
    timeThing (sprintf "Pre-split parallelised %i number sum" firstN) preSplit ready
    |> ignore
    GC.Collect()
    0
    