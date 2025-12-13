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
    static member svr = DeviceId.Create "svr"
    static member dac = DeviceId.Create "dac"
    static member fft = DeviceId.Create "fft"

[<StructuredFormatDisplay("{Id}: {Outputs}")>]
type Device = { Id: DeviceId; Outputs: DeviceId list } with
    static member Create id outputs = { Id = id; Outputs = outputs }
    static member out = Device.Create DeviceId.out []

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
    addDevice Device.out

    File.ReadLines(input)
    |> Seq.filter (not << String.IsNullOrWhiteSpace)
    |> Seq.map parseDevice
    |> Seq.iter addDevice 

    deviceMap

let rec countOutPaths (deviceMap: Dictionary<_,_>) targetDeviceId deviceId  =
    if deviceId = targetDeviceId then 1L
    else
        deviceMap[deviceId].Outputs
        |> List.sumBy (countOutPaths deviceMap targetDeviceId)

let result1 input = 
    let deviceMap = readDevices input
    countOutPaths deviceMap DeviceId.out DeviceId.you


printfn "Part 1 Example: %A" (result1 "example.txt")
printfn "Part 1 Result: %A" (result1 "input.txt")


let countOutPaths2 (deviceMap : Dictionary<_,_>) toDeviceId fromDeviceId = 
    let countCache= new Dictionary<_,_>()

    let rec countPaths toDeviceId fromDeviceId   =
        if fromDeviceId = toDeviceId then 1L
        else
            if countCache.ContainsKey(fromDeviceId) then
                countCache[fromDeviceId]
            else
                let count =
                    deviceMap[fromDeviceId].Outputs
                    |> List.sumBy (countPaths toDeviceId)
                countCache[fromDeviceId] <- count
                count

    countPaths toDeviceId fromDeviceId


let result2 input = 
    let deviceMap = readDevices input
    let svrToFft =DeviceId.svr |> countOutPaths2 deviceMap DeviceId.fft
    let fftToDac =DeviceId.fft |> countOutPaths2 deviceMap DeviceId.dac
    let dacToOut =DeviceId.dac |> countOutPaths2 deviceMap DeviceId.out
    svrToFft * fftToDac * dacToOut


printfn "Part 2 Example: %A" (result2 "example2.txt")
printfn "Part 2 Result: %A" (result2 "input.txt")
