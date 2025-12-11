// Formatting functions for Day 10 (copy into Program.fs if needed)

let formatLights n lights =
    seq {
        yield "["
        for i in 0 .. n do
            if (lights &&& (1 <<< i)) <> 0 then
                yield "#"
            else
                yield "."
        yield "]"
    } |> String.Concat

let formatButton button =
    seq {
        yield "("
        yield String.Join(",",
            seq {
                for i: int32 in 0 .. 31 do
                    if (button &&& (1 <<< i)) <> 0 then
                        yield i.ToString()
            }
        )
        yield ")"
    } |> String.Concat

let formatButtons buttons =
    buttons
    |> List.map formatButton
    |> String.concat " "

