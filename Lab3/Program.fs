module Program

open Linear 
open Newton
open Point
open Stream
open System

type Algoritm = 
    | Linear
    | Newton

type CommandLineOptions = {
    Algoritms: Algoritm list
    Step: decimal
    WindowSize: int
}

type PointWithAlgo = {
    Point: Point
    Algorithm: Algoritm
}

let parseAlgorithm arg = 
    match arg with
    | "--linear" -> Some Linear
    | "--newton" -> Some Newton  
    | _ -> None

let parseArguments (argv: string list) =
    let rec parse args currentOptions=
        match args with
        | [] -> currentOptions
        | "--step" :: stepValue :: rest ->
            printf "%A" stepValue
            match Decimal.TryParse stepValue with
            | true, step -> parse rest { currentOptions with Step = step }
            | false, _ -> 
                printfn "invalid step %s" stepValue
                parse rest currentOptions
        | "-n" :: nValue :: rest ->
            match Int32.TryParse nValue with
            | true, n -> parse rest { currentOptions with WindowSize = n }
            | false, _ ->
                printfn "invalid window size %s" nValue
                parse rest currentOptions
        | arg :: rest ->
            match parseAlgorithm arg with
            | Some alg -> 
                parse rest { currentOptions with Algoritms = alg :: currentOptions.Algoritms }
            | None ->
                printfn "unknown argument %s" arg
                parse rest currentOptions

    let defaultOptions = {
        Algoritms = []
        Step = 0.1m
        WindowSize = 2
    }
    
    parse  argv  defaultOptions

let algorithmFunc func = 
    match func with
    | Linear -> createLinearFunc
    | Newton -> createNewtonFunc


let runAlg (algs: Algoritm list) (step: decimal) (points: seq<Point>) (n: int) : seq<PointWithAlgo[]> =
    let rec runAllAlgs (algs: Algoritm[]) x (window: Point[]) i acc = 
        if i >= algs.Length then
            acc
        else 
            let f = algorithmFunc algs.[i] window
            runAllAlgs algs x window (i + 1) (Array.append acc [| {Point = {X = x; Y = f x}; Algorithm = algs.[i]} |])
        
    let windows = points |> slidingWindows n |> Seq.filter(fun window -> List.length window = n) |> Seq.cache
    let start = (windows |> Seq.head |> List.head).X 
    let seqPoints = genPoints step start |> Seq.cache

    let rec processWindows (remainingWindows: seq<Point list>) (lastX: decimal) (acc: seq<PointWithAlgo[]>) =
        match remainingWindows |> Seq.tryHead with
        | None -> acc
        | Some window ->
            let last = (List.last window).X
            let windowArray = List.toArray window
            let algsArray = List.toArray algs
            
            let pointsInWindow = 
                seqPoints
                |> Seq.takeWhile (fun x -> x <= last)
                |> Seq.skipWhile (fun x -> x <= lastX)
                

            let windowResults = 
                pointsInWindow
                |> Seq.map (fun x -> 
                    runAllAlgs algsArray x windowArray 0 [||] 
                ) 

            processWindows 
                (remainingWindows |> Seq.tail) 
                (pointsInWindow |> Seq.last) 
                (Seq.append acc windowResults)
                
    
    processWindows windows (start - step) Seq.empty

   

let runAllAlgorithms (options: CommandLineOptions) (points: seq<Point>) : seq<PointWithAlgo> =
    runAlg options.Algoritms options.Step points options.WindowSize |> Seq.collect id

[<EntryPoint>]
let main argv = 
    let options = parseArguments (List.ofArray  argv)
    
    if List.isEmpty options.Algoritms then
        printfn "Write correct algoritms: --linear --newton"
        1
    else
        let points = stdinPoints()
        
        let results = runAllAlgorithms options points
        results |> Seq.iter (fun el -> printfn "%A: %f %f" el.Algorithm el.Point.X el.Point.Y)
        0