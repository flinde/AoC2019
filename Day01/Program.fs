// Learn more about F# at http://fsharp.org

open System
open System.IO


let fuelRequired mass : float =
    Math.Floor(mass/3.0) - 2.0

let sumOfFuelRequired listOfMasses : float =
    let listOfFuel = listOfMasses |> Array.map fuelRequired
    Array.sum listOfFuel

let rec fuelOfFuel fuel : float =
    let requierdFuel = fuelRequired fuel
    if requierdFuel > 0.0 then
        requierdFuel + fuelOfFuel requierdFuel
    else
        0.0

let sumOfFuelOfFuel listOfMasses : float =
    let listOfFuel = listOfMasses |> Array.map fuelRequired
    let sumOfFuelOfFfuelFromMasses = Array.sum listOfFuel
    let sumOfFuelOfFuel = Array.sum (listOfFuel |> Array.map fuelOfFuel)
    sumOfFuelOfFfuelFromMasses + sumOfFuelOfFuel

        

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines(@"TextFile1.txt")
    let listMasses = lines |> Array.map float
    let totalFuel = sumOfFuelRequired listMasses
    let totalFuelInt = int totalFuel
    printfn "Sum of Fuel Required: %i" totalFuelInt
    let totalFuelOfFuel = sumOfFuelOfFuel listMasses
    let totalFuelOfFuelInt = int totalFuelOfFuel
    printfn "Sum of Fuel Required: %i" totalFuelOfFuelInt
    
    0 // return an integer exit code



