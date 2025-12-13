open System
open System.Collections.Generic
open System.IO
open FParsec

[<StructuredFormatDisplay("{Id}")>]
type DeviceId = { Id: string } with
    static let cache = new Dictionary<string, DeviceId>()
    static member Create id = 
        match cache.TryGetValue id with
        | true, deviceId -> deviceId
        | false, _ ->
            let deviceId = { Id = id }
            cache[id] <- deviceId
            deviceId
    static member out = DeviceId.Create "out"
    static member you = DeviceId.Create "you"

[<StructuredFormatDisplay("{Id}: {Outputs}")>]
type Device = { Id: DeviceId; Outputs: DeviceId list } with
    static member Create id outputs = { Id = id; Outputs = outputs }

let lowercase n = manyMinMaxSatisfy n n Char.IsLower 

let deviceId = spaces >>. lowercase 3 |>> DeviceId.Create

let device = pipe2 (deviceId .>> pchar ':') (many1 deviceId) Device.Create .>> eof

let parseDevice s =
    match run device s with
    | Success (device,_,_) -> device
    | Failure (error,_,_) -> failwith error

let readDevices input = 
    let deviceMap = new Dictionary<DeviceId, Device>()
    let addDevice d = deviceMap[d.Id] <- d

    File.ReadLines(input)
    |> Seq.filter (not << String.IsNullOrWhiteSpace)
    |> Seq.map parseDevice
    |> Seq.iter addDevice 

    deviceMap

let rec countOutPaths (deviceMap: Dictionary<_,_>) deviceId =
    if deviceId = DeviceId.out then 1L
    else
        deviceMap[deviceId].Outputs
        |> List.sumBy (countOutPaths deviceMap)

let result1 input = 
    let deviceMap = readDevices input
    countOutPaths deviceMap DeviceId.you


printfn "Part 1 Example: %A" (result1 "example.txt")
printfn "Part 1 Result: %A" (result1 "input.txt")
 