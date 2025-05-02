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


module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import File exposing (File)
import File.Download as Download
import File.Select as Select
import GradeBarChart exposing (view)
import GradeHistogram
import GradeSuggestions exposing (autoPartition)
import Html exposing (Html, br, button, div, h1, h2, h3, hr, input, label, li, ol, option, p, pre, select, span, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, disabled, max, min, name, placeholder, scope, selected, size, step, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import RandomStudentData exposing (generateRandomData)
import Student exposing (..)
import Task


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Separator
    = Comma
    | Tab
    | AutoDetect


type alias Model =
    { students : List Student
    , grades : List Grade
    , separator : Separator -- Comma, tab etc. for separating columns
    , content : String -- raw textarea content
    , sortEntries : Bool -- should we sort the graded list?
    , histogramBins : Int -- number of histogram bins
    , column_names : List String -- name of columns
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { students = []
      , grades =
            [ PassGrade "AP" 100.0
            , PassGrade "AA" 90.0
            , PassGrade "AB" 80.0
            , PassGrade "BB" 70.0
            , PassGrade "BC" 60.0
            , PassGrade "CC" 50.0
            , PassGrade "CD" 40.0
            , PassGrade "DD" 30.0
            , FailGrade "FR"
            ]
      , separator = AutoDetect
      , content = ""
      , sortEntries = True
      , histogramBins = 100
      , column_names = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeSeparator String
    | ContentChange String
    | NewGradeBoundary String String
    | ToggleSort Bool
    | HistogramBinChange String
    | ExportData
    | SuggestPartitions
    | UploadRequested
    | UploadSelected File
    | UploadLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSort sortEntries ->
            ( { model | sortEntries = sortEntries }, Cmd.none )

        ChangeSeparator sep ->
            let
                separator =
                    case sep of
                        "," ->
                            Comma

                        "\t" ->
                            Tab

                        _ ->
                            AutoDetect

                parsed_rows =
                    model.grades
                        |> parseStudentRows separator model.content
            in
            ( { model
                | separator = separator
                , students = Tuple.first parsed_rows
                , column_names = Tuple.second parsed_rows
              }
            , Cmd.none
            )

        ContentChange textAreaContent ->
            let
                parsed_rows =
                    model.grades
                        |> parseStudentRows model.separator textAreaContent
            in
            ( { model
                | content = textAreaContent
                , students = Tuple.first parsed_rows
                , column_names = Tuple.second parsed_rows
              }
            , Cmd.none
            )

        NewGradeBoundary grade newBoundaryStr ->
            let
                newBoundary =
                    case String.toFloat newBoundaryStr of
                        Nothing ->
                            0.0

                        Just x ->
                            x

                newGradeBoundaries =
                    model.grades
                        |> List.map
                            (\n ->
                                case n of
                                    PassGrade grade_ boundary ->
                                        if grade_ == grade && newBoundary >= 0.0 then
                                            PassGrade grade newBoundary

                                        else
                                            n

                                    _ ->
                                        n
                            )

                parsed_rows =
                    newGradeBoundaries
                        |> parseStudentRows model.separator model.content
            in
            ( { model
                | grades =
                    newGradeBoundaries
                , students = Tuple.first parsed_rows
                , column_names = Tuple.second parsed_rows
              }
            , Cmd.none
            )

        HistogramBinChange newBins ->
            ( { model
                | histogramBins =
                    case String.toInt newBins of
                        Nothing ->
                            model.histogramBins

                        Just x ->
                            x
              }
            , Cmd.none
            )

        ExportData ->
            ( model
            , model.students
                |> List.filter (\n -> isGradedStudent n)
                |> List.map (\n -> getStudentRollno n ++ "\t" ++ getStudentGradeStr n)
                |> String.join "\u{000D}\n"
                |> download
            )

        SuggestPartitions ->
            let
                newGradeBoundaries =
                    List.concat
                        [ autoPartition (List.length model.grades - 1) (List.map getStudentScore model.students)
                            |> List.reverse
                            |> List.map2 (\x y -> PassGrade x y) (List.map gradeToStr model.grades)
                        , [ case List.head (List.reverse model.grades) of
                                Nothing ->
                                    FailGrade "FR"

                                Just x ->
                                    x
                          ]
                        ]

                parsed_rows =
                    newGradeBoundaries
                        |> parseStudentRows model.separator model.content
            in
            ( { model
                | grades = newGradeBoundaries
                , students = Tuple.first parsed_rows
                , column_names = Tuple.second parsed_rows
              }
            , Cmd.none
            )

        UploadRequested ->
            ( model, Select.file [ "text/csv", "text/tsv" ] UploadSelected )

        UploadSelected file ->
            ( model
            , Task.perform UploadLoaded (File.toString file)
            )

        UploadLoaded content ->
            let
                parsed_rows =
                    model.grades
                        |> parseStudentRows model.separator content
            in
            ( { model
                | content = content
                , students = Tuple.first parsed_rows
                , column_names = Tuple.second parsed_rows
              }
            , Cmd.none
            )


parseStudentRows : Separator -> String -> List Grade -> ( List Student, List String )
parseStudentRows separator content grade_boundaries =
    let
        contentHead =
            case List.head (String.split "\n" (String.trim content)) of
                Just a ->
                    a

                Nothing ->
                    ""

        detectedSeparator =
            if String.contains "," contentHead && not (String.contains "\t" contentHead) then
                ","

            else if String.contains "\t" contentHead && not (String.contains "," contentHead) then
                "\t"

            else
                "\t"

        separatorStr =
            case separator of
                AutoDetect ->
                    detectedSeparator

                _ ->
                    getSeparator separator

        firstRowHeader =
            case List.head (List.reverse (String.split separatorStr contentHead)) of
                Just isScore ->
                    case String.toFloat isScore of
                        Just _ ->
                            False

                        Nothing ->
                            True

                Nothing ->
                    False

        column_names =
            let
                first_row =
                    String.split separatorStr contentHead
            in
            if firstRowHeader then
                first_row

            else
                List.concat
                    [ [ "Roll number" ]
                    , List.range 2 (List.length first_row)
                        |> List.map
                            (\n -> "Column " ++ String.fromInt n)
                    ]
    in
    ( content
        |> String.trim
        |> String.split "\n"
        |> List.map (String.split separatorStr)
        |> List.filter (\n -> List.length n > 1)
        |> List.filter
            (\n ->
                case List.head n of
                    Nothing ->
                        False

                    Just x ->
                        not (String.isEmpty x)
            )
        |> List.map rowToUngradedStudent
        |> List.map (assignStudentGrade grade_boundaries)
        |> List.filter
            (\n ->
                case n of
                    GradedStudent _ _ _ _ ->
                        True

                    _ ->
                        False
            )
    , column_names
    )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Grading Helper" ]
        , div [ class "row" ]
            [ div [ class "col-sm border" ] <| dataEntryView model ]
        , div [ class "row" ]
            [ div [ class "col-sm-3 border" ] <| gradeBoundaryView model
            , model.grades
                |> histogramView model.histogramBins model.students
                |> div [ class "col-sm border" ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm border" ] <| gradedRowsView model ]
        ]


dataEntryView : Model -> List (Html Msg)
dataEntryView model =
    [ h2 [] [ text "Step 1: Data Entry" ]
    , p []
        [ text "Choose your data separator:"
        , select [ onInput ChangeSeparator ]
            [ option [ value "", selected True ] [ text "Auto-detect" ]
            , option [ value "\t" ] [ text "Tab" ]
            , option [ value "," ] [ text "Comma" ]
            ]
        ]
    , p [] [ text "Paste your CSV/Tab separated grades here. The first column is considered as the roll number, and the last one as the score out of 100." ]
    , textarea
        [ placeholder "Paste data here."
        , onInput ContentChange
        , value model.content
        , style "width" "40em"
        ]
        []
    , br [] []
    , button
        [ class "btn btn-primary"
        , onClick UploadRequested
        ]
        [ text "Upload data..." ]
    , button
        [ disabled (model.content /= "")
        , class "btn btn-primary"
        , generateRandomData 100 (model.separator |> getSeparator)
            |> ContentChange
            |> onClick
        ]
        [ text "Insert sample data" ]
    , p [] [ text "Example (Tab separated data):" ]
    , pre [ style "background-color" "#eee" ] [ text "rollno1\tName 1\t90.4\nrollno2\tName 2\t45.6\nrollno3\tName 3\t75.5\n…" ]
    ]


gradeBoundaryView : Model -> List (Html Msg)
gradeBoundaryView model =
    [ h2 [] [ text "Step 2: Specify the grade boundaries:" ]
    , p [] [ text "Hint: use the mouse scroll button or arrow keys to change inputs in the input boxes for fine grained control." ]
    , p []
        [ span [] <|
            List.concat
                [ [ text "Grade boundaries" ]
                , [ br [] [] ]
                , List.concatMap gradeBoundaryInputs model.grades
                ]
        ]
    , button
        [ onClick SuggestPartitions
        , disabled <| List.length model.students < 15
        , class "btn btn-primary"
        ]
        [ text "Suggest partitions" ]
    ]


gradeBoundaryInputs : Grade -> List (Html Msg)
gradeBoundaryInputs gradeBoundary =
    case gradeBoundary of
        PassGrade grade _ ->
            [ label [ attribute "for" ("grade" ++ grade) ] [ text (grade ++ ": ") ]
            , input
                [ placeholder <| grade
                , name ("grade" ++ grade)
                , type_ "number"
                , step "0.5"
                , style "width" "4em"
                , onInput (NewGradeBoundary <| grade)
                , value <| String.fromFloat <| getGradeBoundary gradeBoundary
                ]
                []
            , br [] []
            ]

        FailGrade grade ->
            [ text <| grade ++ ": 0" ]


histogramView : Int -> List Student -> List Grade -> List (Html Msg)
histogramView nBins studentList grade_boundaries =
    let
        data =
            List.map getStudentScore studentList

        boundaries =
            List.map getGradeBoundary grade_boundaries

        gradeLabels =
            List.map gradeToStr grade_boundaries

        grade_counts =
            countGradeDist studentList grade_boundaries
    in
    if List.reverse boundaries == List.sort boundaries then
        List.concat
            [ [ h2 [] [ text "Step 3: Histogram" ] ]
            , grade_counts
                |> List.map
                    (\n ->
                        n.grade ++ ": " ++ String.fromInt n.count ++ " "
                    )
                |> List.map text
            , [ nBins
                    |> GradeHistogram.view
                        (data |> List.filter (\n -> n >= 0))
                        (gradeLabels |> List.map2 Tuple.pair boundaries)
              , br [] []
              , text "Number of histogram bins: "
              ]
            , [ input
                    [ placeholder "Bins"
                    , type_ "number"
                    , step "1"
                    , onInput HistogramBinChange
                    , value <| String.fromInt nBins
                    ]
                    []
              ]
            , [ br [] [] ]
            , [ hr [] [] ]
            , [ h3 [] [ text "Grade distribution" ] ]
            , [ GradeBarChart.view (grade_counts |> List.map (\n -> ( n.grade, n.count ))) ]
            ]

    else
        [ span [ style "color" "red" ] [ text "Grade ordering incorrect!" ] ]


countGradeDist : List Student -> List Grade -> List GradeCount
countGradeDist studentList gradeBoundaries =
    gradeBoundaries
        |> List.map
            (\n ->
                case n of
                    PassGrade g _ ->
                        g

                    FailGrade g ->
                        g
            )
        |> List.map
            (\n ->
                { grade = n
                , count =
                    studentList
                        |> List.filter
                            (\m -> getStudentGradeStr m == n)
                        |> List.length
                }
            )


gradedRowsView : Model -> List (Html Msg)
gradedRowsView model =
    let
        boundaries =
            List.map getGradeBoundary model.grades

        listEntries =
            if model.sortEntries then
                model.students
                    |> List.sortBy (\n -> -(getStudentScore n))

            else
                model.students

        dataCols =
            case List.head model.students of
                Nothing ->
                    -1

                Just x ->
                    List.length (getStudentData x)

        fallback_column_names =
            List.concat
                [ [ "Roll number" ]
                , List.range 1 dataCols
                    |> List.map
                        (\n -> "Column " ++ String.fromInt n)
                , [ "Score" ]
                ]

        column_names =
            if List.length model.column_names == List.length fallback_column_names then
                model.column_names

            else
                fallback_column_names
    in
    if List.reverse boundaries == List.sort boundaries then
        [ h2 [] [ text "Step 4: Verify grades" ]
        , text "Sort by score?"
        , input [ type_ "checkbox", checked model.sortEntries, onCheck ToggleSort ] []
        , br [] []
        , button
            [ class "btn btn-primary"
            , onClick ExportData
            , disabled <| List.length model.students == 0
            ]
            [ text "Export for ASC upload" ]
        , br [] []
        , table [ class "table" ]
            [ thead []
                [ List.concat
                    [ column_names
                        |> List.map (\n -> th [ scope "col" ] [ text <| n ])
                    , [ th [ scope "col" ] [ text "Grade" ] ]
                    ]
                    |> tr []
                ]
            , listEntries
                |> List.map
                    (\n ->
                        [ List.concat
                            [ [ th [ scope "row" ] [ text (getStudentRollno n) ] ]
                            , getStudentData n |> List.map (\m -> td [] [ text m ])
                            , [ td [] [ String.fromFloat (getStudentScore n) |> text ] ]
                            , [ td [] [ text (getStudentGradeStr n) ] ]
                            ]
                            |> tr [ class <| "grade" ++ getStudentGradeStr n ]
                        ]
                    )
                |> List.concat
                |> tbody []
            ]
        ]

    else
        [ span [ style "color" "red" ] [ text "Grade ordering incorrect!" ] ]


download : String -> Cmd msg
download ascData =
    Download.string "ascdata.txt" "text/plain" ascData



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getSeparator : Separator -> String
getSeparator sep =
    case sep of
        Comma ->
            ","

        Tab ->
            "\t"

        AutoDetect ->
            "\t"
