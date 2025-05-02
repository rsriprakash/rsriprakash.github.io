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


module RandomStudentData exposing (generateRandomData)

import Random exposing (Generator, Seed)


generator : Generator Float
generator =
    Random.float 0 100


seed : Seed
seed =
    Random.initialSeed 227852860


generateRandomData : Int -> String -> String
generateRandomData num separator =
    List.map2 (++)
        (List.range 1 num
            |> List.map
                (\n ->
                    [ "rollno" ++ String.fromInt n
                    , "Student " ++ String.fromInt n
                    ]
                )
        )
        (List.map (\n -> [ String.fromFloat n ]) data)
        |> List.map (String.join separator)
        |> String.join "\n"


data : List Float
data =
    [ 69.57
    , 45.48
    , 78.81
    , 52.14
    , 54.91
    , 35.77
    , 48.87
    , 66.6
    , 59.84
    , 68.13
    , 96.44
    , 56.66
    , 60.41
    , 58.46
    , 39.64
    , 61.39
    , 74.18
    , 45.42
    , 44.63
    , 30.37
    , 70.55
    , 61.22
    , 85.79
    , 64.72
    , 74.23
    , 35.09
    , 60.23
    , 39.57
    , 45.65
    , 42.62
    , 43.5
    , 44.71
    , 64.04
    , 40.4
    , 7.95
    , 48.46
    , 35.34
    , 34.87
    , 67.89
    , 52.11
    , 60.75
    , 46.28
    , 35.99
    , 65.5
    , 63.29
    , 78.04
    , 37.8
    , 46.19
    , 63.38
    , 37.05
    , 55.88
    , 61.08
    , 73.02
    , 60.8
    , 70.02
    , 58.15
    , 42.55
    , 67.71
    , 64.43
    , 74.97
    , 30.32
    , 42.02
    , 66.75
    , 55.3
    , 52.1
    , 75.37
    , 44.34
    , 36.04
    , 60.54
    , 56.24
    , 52.56
    , 39.22
    , 63.88
    , 64.7
    , 61.19
    , 33.28
    , 61.46
    , 62.19
    , 43.53
    , 48.94
    , 51.74
    , 28.22
    , 62.83
    , 61.22
    , 89.76
    , 25.49
    , 58.21
    , 59.08
    , 29.83
    , 55.4
    , 72.89
    , 31.56
    , 24.16
    , 52.51
    , 75.33
    , 26.23
    , 62.39
    , 25.53
    , 64.9
    , 45.64
    , 44.9
    , 34.93
    , 25.73
    , 53.73
    , 65.08
    , 60.69
    , 7.13
    , 70.85
    , 47.27
    , 56.67
    , 68.92
    , 35.83
    , 33.83
    , 69.3
    , 47.22
    , 68.76
    , 50.19
    , 42.77
    , 31.41
    , 42.19
    , 46.9
    , 27.8
    , 63.51
    , 57.79
    , 63.47
    , 49.36
    , 61.18
    , 64.75
    , 42.09
    , 42.02
    , 63.91
    , 53.01
    , 15.83
    , 54.02
    , 42.09
    , 47.37
    , 61.62
    , 54.95
    , 47.12
    , 65.38
    , 70.36
    , 58.13
    , 58.27
    , 51.34
    , 43.04
    , 44.54
    , 64.03
    , 65.31
    , 32.54
    , 42.47
    , 41.35
    , 47.78
    , 48.63
    , 24.77
    , 62.35
    , 44.96
    , 38.5
    , 65.49
    , 71.63
    , 40.69
    , 46.84
    , 42.5
    , 28.24
    , 26.81
    , 51.29
    , 30.99
    , 47.87
    , 62.48
    , 63.94
    , 54.27
    , 35.0
    , 69.68
    , 28.33
    , 51.15
    , 64.4
    , 60.18
    , 57.17
    , 41.25
    , 48.48
    , 39.28
    , 40.94
    , 72.25
    , 18.63
    , 32.66
    , 29.66
    , 59.6
    , 69.81
    , 60.25
    , 47.35
    , 32.54
    , 43.85
    , 39.65
    , 59.03
    , 74.34
    , 49.72
    , 31.39
    , 55.7
    , 46.07
    , 35.37
    , 68.7
    ]
