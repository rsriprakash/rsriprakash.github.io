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


module GradeHistogram exposing (view)

import Axis
import Color
import Histogram exposing (Bin, HistogramGenerator, Threshold, custom)
import Html exposing (Html)
import Random exposing (Generator, Seed)
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, line, rect, svg, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, strokeDasharray, transform, viewBox)
import TypedSvg.Attributes.InEm exposing (dx, dy, fontSize)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Fill(..), Transform(..))


separations : Int -> Threshold a Float
separations nBins fn list domain =
    List.map toFloat (List.range 1 (nBins - 1))
        |> List.map (\n -> n / (toFloat nBins / 100.0))


histogram : List Float -> Int -> List (Bin Float Float)
histogram model nBins =
    Histogram.custom (separations nBins) (\n -> n)
        |> Histogram.withDomain ( 0, 100 )
        |> Histogram.compute model


w : Float
w =
    450


h : Float
h =
    150


padding : Float
padding =
    30


xScale : ContinuousScale Float
xScale =
    Scale.linear ( 0, w - 2 * padding ) ( 0, 100 )


yScaleFromBins : List (Bin Float Float) -> ContinuousScale Float
yScaleFromBins bins =
    List.map .length bins
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> Tuple.pair 0
        |> Scale.linear ( h - 2 * padding, 0 )


xAxis : List Float -> Svg msg
xAxis model =
    Axis.bottom [] xScale


yAxis : List (Bin Float Float) -> Svg msg
yAxis bins =
    Axis.left [ Axis.tickCount 5 ] (yScaleFromBins bins)


column : ContinuousScale Float -> Bin Float Float -> Svg msg
column yScale { length, x0, x1 } =
    rect
        [ x <| Scale.convert xScale x0
        , y <| Scale.convert yScale (toFloat length)
        , width <| Scale.convert xScale x1 - Scale.convert xScale x0
        , height <| h - Scale.convert yScale (toFloat length) - 2 * padding
        , fill <| Fill <| Color.rgb255 46 118 149
        ]
        []


boundaryLine x =
    line
        [ x1 <| Scale.convert xScale x
        , y1 0.0
        , x2 <| Scale.convert xScale x
        , y2 (h - 2 * padding)
        , strokeWidth 1
        , stroke <| Color.rgb255 0 0 0
        , strokeDasharray "5 5"
        ]
        []


gradeText : ( Float, String ) -> Svg msg
gradeText labelStr =
    let
        xval =
            Tuple.first labelStr

        gradeStr =
            Tuple.second labelStr
    in
    text_
        [ x <| Scale.convert xScale xval
        , y 0.0
        , dx -0.1
        , dy -0.3
        , fontSize 0.75
        ]
        [ text gradeStr ]


scaleFactor =
    1.0


view : List Float -> List ( Float, String ) -> Int -> Html msg
view model boundaries nBins =
    let
        bins =
            histogram model nBins
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Scale scaleFactor scaleFactor, Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Scale scaleFactor scaleFactor, Translate (padding - 1) padding ] ]
            [ yAxis bins ]
        , g [ transform [ Scale scaleFactor scaleFactor, Translate padding padding ], class [ "series" ] ] <|
            List.map (column (yScaleFromBins bins)) bins
        , g [ transform [ Scale scaleFactor scaleFactor, Translate padding padding ], class [ "series" ] ] <|
            List.map boundaryLine (List.map Tuple.first boundaries)
        , g [ transform [ Scale scaleFactor scaleFactor, Translate padding padding ], class [ "series" ] ] <|
            List.map gradeText boundaries
        ]
