open System
open System.IO

let IntArrayEquals (arr1: int[]) (arr2: int[]) : int = 
    Array.compareWith (fun elem1 elem2 ->
        if(elem1 = elem2) then 0
        else 1) arr1 arr2
        

let stringToIntArray (strng:string) : int[] =
    strng.Split [|','|] |> Array.map int

let opcode1 (regAOffset: int) (regBOffset: int) (offset:int) (prgrm: int[]) : int[] =
    let mutable internalPrgm = prgrm
    let regA = internalPrgm.[regAOffset]
    let regB = internalPrgm.[regBOffset]
    printfn "ADD %d %d save in %d" regA regB offset
    Array.set internalPrgm offset (regA + regB)
    internalPrgm
    
let opcode2 (regAOffset: int) (regBOffset: int) (offset:int) (prgrm: int[]) : int[] =
    let mutable internalPrgm = prgrm
    let regA = internalPrgm.[regAOffset]
    let regB = internalPrgm.[regBOffset]
    printfn "MUL %d %d save in %d" regA regB offset
    Array.set internalPrgm offset (regA * regB)
    internalPrgm

let Run (prgrm:int[]): (int * int[]) =
    let mutable internalPrgm = prgrm
    let mutable prgrmcntr = 0
    let mutable prgrmEnd  = false
    let mutable regAOffset = 0
    let mutable regBOffset = 0
    let mutable outputOffset =0
    let mutable state =0

    while not prgrmEnd do
        printfn "%A" internalPrgm
        if  prgrmcntr+4 < internalPrgm.Length then
            regAOffset <- internalPrgm.[prgrmcntr+1]
            regBOffset <- internalPrgm.[prgrmcntr+2] 
            outputOffset <- internalPrgm.[prgrmcntr+3] 
        let opcode = internalPrgm.[prgrmcntr]
        if opcode = 1 then
            internalPrgm <- opcode1 regAOffset regBOffset outputOffset internalPrgm
            state <- internalPrgm.[outputOffset]
        elif (opcode = 2) then
            internalPrgm <- opcode2 regAOffset regBOffset outputOffset internalPrgm
            state <- internalPrgm.[outputOffset]
        elif (opcode = 99) then
            prgrmEnd <- true
            
        else
            failwith "Unknown opcode!"
        prgrmcntr <- prgrmcntr + 4

    (state, internalPrgm)

let RunAssert prgrm become =
    let (finalState, result) = Run prgrm
    if (IntArrayEquals become result) <> 0 then failwith "Nop!"
    printfn "Final State: %A" finalState
    
let FindInputs (output:int) (prgramlines:int[]): (int * int) =
    let mutable noun =0 
    let mutable verb =0
    let mutable state =0
    let mutable prgrm:int[] =[||]
    let mutable prgmInput = prgramlines

    for i in 0 .. 99 do
        for j in 0 .. 99 do
            prgmInput <- Array.copy prgramlines
            Array.set prgmInput 1 i
            Array.set prgmInput 2 j
            printfn "noun: %d, verb: %d" i j
            let (state, prgrm) = Run prgmInput
//            (state, prgrm)
            if state = output then
                noun <- i
                verb <- j
    let result = (noun,verb)
    result


[<EntryPoint>]
let main argv =
    let programlines = ((File.ReadAllLines(@"TextFile1.txt") |> String.concat "" ).Split [|','|]) |> Array.map int
    let prgrm1 =        stringToIntArray "1,9,10,3,2,3,11,0,99,30,40,50"
    let prgrm1Become =  stringToIntArray "3500,9,10,70,2,3,11,0,99,30,40,50"
    let prgrm2 =        stringToIntArray "1,0,0,0,99"
    let prgrm2Become =  stringToIntArray "2,0,0,0,99"
    let prgrm3 =        stringToIntArray "2,3,0,3,99"
    let prgrm3Become =  stringToIntArray "2,3,0,6,99"
    let prgrm4  =       stringToIntArray "2,4,4,5,99,0"
    let prgrm4Become =  stringToIntArray "2,4,4,5,99,9801"
    let prgrm5 =        stringToIntArray "1,1,1,4,99,5,6,0,99"
    let prgrm5Become =  stringToIntArray "30,1,1,4,2,5,6,0,99"
    
    printfn "Checking Computer"
    RunAssert prgrm1 prgrm1Become
    printfn "Program 1 ok"
    RunAssert prgrm2 prgrm2Become
    printfn "Program 2 ok"
    RunAssert prgrm3 prgrm3Become
    printfn "Program 3 ok"
    RunAssert prgrm4 prgrm4Become
    printfn "Program 4 ok"
    RunAssert prgrm5 prgrm5Become
    printfn "Program 5 ok"

    let mutable prgmInput = Array.copy programlines
    Array.set prgmInput 1 12
    Array.set prgmInput 2 2
    let (state, prgrm) = Run prgmInput
    printfn "Answer: %d" prgrm.[0]

    let (noun, verb) = FindInputs 19690720 programlines
    printfn "noun: %d, verb: %d, Answer: %d" noun verb (100 * noun + verb)
    0 // return an integer exit code
    //1028301
