module TestDP exposing (Config)

import Date exposing (Date)
import DatePicker.Date as Helpers
import Html as H exposing (Html)
import Html.Attributes as Att exposing (placeholder, selected, tabindex, type_, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput, targetValue)
import Html.Keyed
import Json.Decode as Json
import Task
import Time


type State
    = State
        { open : Bool
        , forceOpen : Bool
        , focused : Maybe Date -- date currently center-focused by picker, but not necessarily chosen
        }



--selectedDate : Maybe Date lives in the model, everything else is a view function


type alias Config msg =
    ( SimpleConfig msg, CustomConfig )


type alias SimpleConfig msg =
    { inputText : String
    , datePicked : ( Date, State ) -> msg
    , dateTextChanged : String -> msg
    , dateTextSubmitted : String -> msg
    , stateChanged : State -> msg
    }


type alias CustomConfig =
    { placeholder : String
    , isDisabled : Date -> Bool
    , firstDayOfWeek : Time.Weekday
    , classNamespace : String
    , containerClassList : List ( String, Bool )
    , inputClassList : List ( String, Bool )
    , inputAttributes : List (H.Attribute Msg)
    , inputName : Maybe String
    , inputId : Maybe String
    , today : Maybe Date
    , monthFormatter : Date.Month -> String
    , yearFormatter : Int -> String
    , dayFormatter : Time.Weekday -> String
    }


defaultCustomConfig : CustomConfig
defaultCustomConfig =
    { placeholder = "Select a date"
    , classNamespace = "elm-datepicker--"
    , containerClassList = []
    , inputClassList = []
    , inputName = Nothing
    , inputId = Nothing
    , inputAttributes =
        [ Att.required False
        ]
    , isDisabled = always False
    , firstDayOfWeek = Time.Sun
    , monthFormatter = Helpers.formatMonth
    , yearFormatter = String.fromInt
    , dayFormatter = Helpers.formatDay
    , today = Nothing
    }


makeConfig : SimpleConfig msg -> Config msg
makeConfig config =
    ( config, defaultCustomConfig )



--todo: onclick attribute for the cell formatter
-- type alias VeryCustomConfig msg =
--     { cellFormatter : String -> Html msg
--     }


type Msg
    = ChangeFocus Date
    | Focus
    | Blur
    | MouseDown
    | MouseUp
    | Pick Date


view : Config msg -> State -> Maybe Date -> H.Html msg
view ( sConfig, cConfig ) (State state) pickedDate =
    let
        potentialInputId =
            cConfig.inputId
                |> Maybe.map Att.id
                |> (List.singleton >> List.filterMap identity)

        inputClasses =
            [ ( cConfig.classNamespace ++ "input", True ) ]
                ++ cConfig.inputClassList

        inputCommon xs =
            H.input
                ([ Att.classList inputClasses
                 , Att.name (cConfig.inputName |> Maybe.withDefault "")
                 , type_ "text"
                 , on "change" (Json.succeed SubmitText)
                 , onInput Text
                 , onBlur Blur
                 , onClick Focus --state changed
                 , onFocus Focus
                 ]
                    ++ cConfig.inputAttributes
                    ++ potentialInputId
                    ++ xs
                )
                []

        dateInput =
            inputCommon
                [ placeholder cConfig.placeholder
                , value sConfig.inputText
                ]

        containerClassList =
            ( cConfig.classNamespace ++ "container", True ) :: cConfig.containerClassList
    in
    H.div
        [ Att.classList containerClassList ]
        [ dateInput
        , if state.open then
            datePicker pickedDate ( sConfig, cConfig ) (State state)

          else
            H.text ""
        ]


prepareDates : Date -> Time.Weekday -> { currentMonth : Date, currentDates : List Date }
prepareDates date firstDayOfWeek =
    let
        weekdayAsInterval =
            Helpers.weekdayToInterval firstDayOfWeek

        firstOfMonth =
            Date.fromCalendarDate (Date.year date) (Date.month date) 1

        -- First shown date
        -- If the first of a month is a sunday and firstDayOfWeek is sunday then its the first of the month
        -- Otherwise the daterange starts in the month before the current month
        start =
            Date.fromCalendarDate (Date.year date) (Date.month date) 1
                |> Date.floor weekdayAsInterval

        end =
            Date.add Date.Months 1 firstOfMonth
                |> Date.ceiling weekdayAsInterval
    in
    { currentMonth = date
    , currentDates = Date.range Date.Day 1 start end
    }


datePicker : Maybe Date -> Config msg -> State -> Html msg
datePicker pickedDate ( sConfig, cConfig ) (State state) =
    let
        currentDate =
            state.focused |> maybeOr pickedDate |> maybeOr cConfig.today |> Maybe.withDefault (Date.fromCalendarDate 1970 Time.Jan 1)

        { currentMonth, currentDates } =
            prepareDates currentDate cConfig.firstDayOfWeek

        dpClass =
            mkClass cConfig

        firstDayOffset =
            Date.weekdayToNumber cConfig.firstDayOfWeek - 1

        arrow className message =
            H.button
                [ dpClass className
                , onClick message
                , tabindex -1
                ]
                []

        picked d =
            pickedDate
                |> Maybe.map (\pdate -> Date.toRataDie pdate == Date.toRataDie d)
                |> Maybe.withDefault False

        isToday d =
            case cConfig.today of
                Just t ->
                    Date.toRataDie d == Date.toRataDie t

                Nothing ->
                    False

        isOtherMonth d =
            Date.month currentDate /= Date.month d

        dayList =
            groupDates currentDates
                |> List.map
                    (\rowDays ->
                        H.tr [ dpClass "row" ]
                            (List.map (viewDay cConfig picked isOtherMonth isToday) rowDays)
                    )

        onChange handler =
            on "change" <| Json.map handler targetValue

        isCurrentYear selectedYear =
            Date.year currentMonth == selectedYear

        yearOption index selectedYear =
            ( String.fromInt index
            , H.option [ value (String.fromInt selectedYear), selected (isCurrentYear selectedYear) ]
                [ H.text <| String.fromInt selectedYear ]
            )

        dropdownYear =
            Html.Keyed.node "select"
                [ onChange (Helpers.changeYear currentDate >> ChangeFocus), dpClass "year-menu" ]
                (List.indexedMap yearOption [])
    in
    H.div
        [ dpClass "picker"
        , Html.Events.stopPropagationOn "mousedown" <| Json.succeed ( MouseDown, True )
        , Html.Events.stopPropagationOn "mouseup" <| Json.succeed ( MouseUp, True )
        ]
        [ H.div [ dpClass "picker-header" ]
            [ H.div [ dpClass "prev-container" ]
                [ arrow "prev" (ChangeFocus (Date.add Date.Months -1 currentDate)) ]
            , H.div [ dpClass "month-container" ]
                [ H.span [ dpClass "month" ]
                    [ H.text <| sConfig.formatMonth <| Date.month currentMonth ]
                , H.span [ dpClass "year" ]
                    [ H.text <| sConfig.yearFormatter <| Date.year currentMonth
                    ]
                ]
            , H.div [ dpClass "next-container" ]
                [ arrow "next" (ChangeFocus (Date.add Date.Months 1 currentDate)) ]
            ]
        , H.table [ dpClass "table" ]
            [ H.thead [ dpClass "weekdays" ]
                [ H.tr []
                    ([ Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri, Time.Sat, Time.Sun ]
                        |> List.repeat 2
                        |> List.concat
                        |> List.drop firstDayOffset
                        |> List.take 7
                        |> List.map (\d -> H.td [ dpClass "dow" ] [ H.text <| cConfig.dayFormatter d ])
                    )
                ]
            , H.tbody [ dpClass "days" ] dayList
            ]
        ]


viewDay : Config msg -> State -> (Date -> Bool) -> (Date -> Bool) -> (Date -> Bool) -> Date -> Html msg
viewDay ( sConfig, cConfig ) state picked isOtherMonth isToday d =
    let
        disabled =
            cConfig.isDisabled d

        classList =
            mkClassList cConfig

        props =
            if not disabled then
                [ onClick <| sConfig.datePicked ( update (Pick d) state, d ) ]

            else
                []
    in
    H.td
        ([ classList
            [ ( "day", True )
            , ( "disabled", disabled )
            , ( "picked", picked d )
            , ( "today", isToday d )
            , ( "other-month", isOtherMonth d )
            ]
         ]
            ++ props
        )
        -- cell formatter here
        [ d |> Date.day |> String.fromInt |> H.text ]


{-| Turn a list of dates into a list of date rows with 7 columns per
row representing each day of the week.
-}
groupDates : List Date -> List (List Date)
groupDates dates =
    let
        go i xs racc acc =
            case xs of
                [] ->
                    List.reverse acc

                x :: xxs ->
                    if i == 6 then
                        go 0 xxs [] (List.reverse (x :: racc) :: acc)

                    else
                        go (i + 1) xxs (x :: racc) acc
    in
    go 0 dates [] []


mkClass : CustomConfig -> String -> H.Attribute msg
mkClass config c =
    Att.class (config.classNamespace ++ c)


mkClassList : CustomConfig -> List ( String, Bool ) -> H.Attribute msg
mkClassList config cs =
    List.map (\( c, b ) -> ( config.classNamespace ++ c, b )) cs
        |> Att.classList


maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr lhs rhs =
    case rhs of
        Just _ ->
            rhs

        Nothing ->
            lhs


{-| The date picker update function. The second tuple member represents a user action to change the
date.
-}
update : Msg -> State -> State
update msg (State state) =
    case msg of
        ChangeFocus date ->
            State { state | focused = Just date }

        Pick date ->
            State
                { state
                    | open = False
                    , focused = Nothing
                }

        Focus ->
            State { state | open = True, forceOpen = False }

        Blur ->
            State { state | open = state.forceOpen }

        MouseDown ->
            State { state | forceOpen = True }

        MouseUp ->
            State { state | forceOpen = False }
