-- IITB Grading Assistant
-- Copyright (C) 2019  Kumar Appaiah
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
-- Author: Kumar Appaiah <a.kumar@alumni.iitm.ac.in>


module GradeBarChart exposing (view)

import Html exposing (Html)
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, strokeDasharray, transform, viewBox)
import TypedSvg.Attributes.InEm exposing (dx, dy, fontSize)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Fill(..), Transform(..))


w : Float
w =
    450


h : Float
h =
    100


drawBar : Float -> Float -> Float -> String -> Svg msg
drawBar barWidth barHeight barY barLabel =
    g
        [ class [ "bar" ] ]
        [ rect [ width barWidth, height barHeight, y barY ] []
        , text_
            [ x <| barWidth + 5
            , y <| barY + 4.5
            , dy 0.35
            , fontSize 0.7
            ]
            [ text barLabel ]
        ]


view : List ( String, Int ) -> Html msg
view model =
    let
        maxNMaybe =
            model |> List.map Tuple.second |> List.maximum

        maxN =
            case maxNMaybe of
                Nothing ->
                    0

                Just x ->
                    toFloat x

        widthScaling =
            w / maxN * 0.8
    in
    if maxN < 1 then
        svg [] []

    else
        svg [ class [ "chart" ], viewBox 0 0 w h ]
            (model
                |> List.indexedMap Tuple.pair
                |> List.map
                    (\n ->
                        let
                            index =
                                Tuple.first n

                            grade =
                                n |> Tuple.second |> Tuple.first

                            occurrences =
                                n |> Tuple.second |> Tuple.second
                        in
                        drawBar (widthScaling * toFloat occurrences) 10 (toFloat index * 11 + 5) (grade ++ ": " ++ String.fromInt occurrences)
                    )
            )
