module GradeSuggestions exposing (autoPartition, mean, variance)


mean : List Float -> Float
mean markList =
    List.foldl (+) 0.0 markList
        |> (\n -> n / toFloat (List.length markList))


variance : List Float -> Float
variance markList =
    let
        mu =
            mean markList
    in
    List.foldl (+) 0.0 (List.map (\n -> n ^ 2) markList)
        |> (\n -> n / toFloat (List.length markList))
        |> (\n -> n - mu * mu)


autoPartition : Int -> List Float -> List Float
autoPartition nPartitions markList =
    let
        mu =
            mean markList

        sigma =
            sqrt <| variance markList
    in
    if nPartitions < 2 then
        []

    else if remainderBy 2 nPartitions == 0 then
        List.range 0 (nPartitions - 1)
            |> List.map (\n -> toFloat n - (toFloat nPartitions - 1) / 2)
            |> List.map (\n -> n * (sigma / 2))
            |> List.map (\n -> n + mu)
            |> List.map (\n -> n * 2)
            |> List.map (\n -> round n)
            |> List.map (\n -> toFloat n / 2)
            |> List.map
                (\n ->
                    if n > 100 then
                        100

                    else if n < 0 then
                        0

                    else
                        n
                )

    else
        List.range -((nPartitions - 1) // 2) ((nPartitions - 1) // 2)
            |> List.map (\n -> toFloat n * (sigma / 2))
            |> List.map (\n -> n + mu)
            |> List.map (\n -> n * 2)
            |> List.map (\n -> round n)
            |> List.map (\n -> toFloat n / 2)
            |> List.map
                (\n ->
                    if n > 100 then
                        100

                    else if n < 0 then
                        0

                    else
                        n
                )
