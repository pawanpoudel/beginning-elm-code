module Playground exposing (main)

import Html


escapeEarth myVelocity mySpeed fuelStatus =
    let
        escapeVelocityInKmPerSec =
            11.186

        orbitalSpeedInKmPerSec =
            7.67

        whereToLand =
            if fuelStatus == "low" then
                "Land on droneship"

            else
                "Land on launchpad"
    in
    if myVelocity > escapeVelocityInKmPerSec then
        "Godspeed"

    else if mySpeed == orbitalSpeedInKmPerSec then
        "Stay in orbit"

    else
        whereToLand


computeSpeed distance time =
    distance / time


computeTime startTime endTime =
    endTime - startTime


add a b =
    a + b


multiply c d =
    c * d


divide e f =
    e / f


weekday dayInNumber =
    case dayInNumber of
        0 ->
            "Sunday"

        1 ->
            "Monday"

        2 ->
            "Tuesday"

        3 ->
            "Wednesday"

        4 ->
            "Thursday"

        5 ->
            "Friday"

        6 ->
            "Saturday"

        _ ->
            "Unknown day"


hashtag dayInNumber =
    case weekday dayInNumber of
        "Sunday" ->
            "#SinDay"

        "Monday" ->
            "#MondayBlues"

        "Tuesday" ->
            "#TakeMeBackTuesday"

        "Wednesday" ->
            "#HumpDay"

        "Thursday" ->
            "#ThrowbackThursday"

        "Friday" ->
            "#FlashbackFriday"

        "Saturday" ->
            "#Caturday"

        _ ->
            "#Whatever"


main =
    Html.text <| hashtag 5
