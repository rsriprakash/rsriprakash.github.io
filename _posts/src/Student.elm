module Student exposing (Grade(..), GradeCount, Student(..), assignStudentGrade, getGradeBoundary, getStudentData, getStudentGrade, getStudentGradeStr, getStudentRollno, getStudentScore, gradeToStr, isGradedStudent, rowToUngradedStudent)


type Student
    = InvalidStudent
    | UngradedStudent String (List String) Float
    | GradedStudent String (List String) Float Grade


type Grade
    = PassGrade String Float
    | FailGrade String


type alias GradeCount =
    { grade : String
    , count : Int
    }


getGradeBoundary : Grade -> Float
getGradeBoundary grade =
    case grade of
        PassGrade _ b ->
            b

        FailGrade _ ->
            0.0


gradeToStr : Grade -> String
gradeToStr grade =
    case grade of
        PassGrade g _ ->
            g

        FailGrade g ->
            g


getStudentScore : Student -> Float
getStudentScore s =
    case s of
        InvalidStudent ->
            -2.0

        UngradedStudent _ _ score ->
            score

        GradedStudent _ _ score _ ->
            score


isGradedStudent : Student -> Bool
isGradedStudent s =
    case s of
        InvalidStudent ->
            False

        UngradedStudent _ _ score ->
            False

        GradedStudent _ _ score _ ->
            True


getStudentRollno : Student -> String
getStudentRollno s =
    case s of
        InvalidStudent ->
            "INVALID"

        UngradedStudent r _ _ ->
            String.toUpper r

        GradedStudent r _ _ _ ->
            String.toUpper r


getStudentGradeStr : Student -> String
getStudentGradeStr s =
    gradeToStr (getStudentGrade s)


getStudentGrade : Student -> Grade
getStudentGrade s =
    case s of
        InvalidStudent ->
            FailGrade "INVALID"

        UngradedStudent _ _ _ ->
            FailGrade "UNGRADED"

        GradedStudent _ _ _ g ->
            g


getStudentData : Student -> List String
getStudentData s =
    case s of
        InvalidStudent ->
            []

        UngradedStudent _ d _ ->
            d

        GradedStudent _ d _ _ ->
            d


assignStudentGrade : List Grade -> Student -> Student
assignStudentGrade grade_boundaries student =
    case student of
        InvalidStudent ->
            InvalidStudent

        UngradedStudent rollno data score ->
            case
                grade_boundaries
                    |> List.map (\n -> PassGrade (gradeToStr n) (score - getGradeBoundary n))
                    |> List.filter (\n -> getGradeBoundary n >= 0)
                    |> List.head
            of
                Nothing ->
                    UngradedStudent rollno data score

                Just x ->
                    GradedStudent rollno data score x

        GradedStudent _ _ _ _ ->
            student


rowToUngradedStudent : List String -> Student
rowToUngradedStudent stringList =
    case stringList of
        [] ->
            InvalidStudent

        [ a ] ->
            InvalidStudent

        [ a, b ] ->
            let
                score =
                    case String.toFloat b of
                        Nothing ->
                            -2.0

                        Just x ->
                            x
            in
            if score < 0 then
                InvalidStudent

            else
                UngradedStudent a [] score

        a :: rest ->
            let
                realRest =
                    List.reverse rest

                scoreStr =
                    case List.head realRest of
                        Nothing ->
                            "-3.0"

                        Just x ->
                            x

                score =
                    case String.toFloat scoreStr of
                        Nothing ->
                            -3.0

                        Just x ->
                            x

                data =
                    case List.tail realRest of
                        Nothing ->
                            []

                        Just x ->
                            List.reverse x
            in
            if score < 0 then
                InvalidStudent

            else
                UngradedStudent a data score
