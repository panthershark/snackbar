module Snackbar exposing
    ( Snackbar, Msg, AutoHide(..)
    , action, hidden, link, message
    , update, view
    , visible
    )

{-| A Material Design Snackbar!


# Definition

@docs Snackbar, Msg, AutoHide


# Show / Hide Snackbars

@docs action, hidden, link, message


# Integrate with an application

@docs update, view


# Utils

@docs visible

-}

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Process
import Task
import Time exposing (Posix, now, posixToMillis)


type alias WithAction msg =
    { btn : String
    , ref : msg
    }


type alias WithHref =
    { btn : String
    , ref : String
    }


type alias Snack a =
    { a
        | str : String
        , id : Int
    }


{-| The Snackbar model
-}
type Snackbar msg
    = None
    | Message (Snack {})
    | Href (Snack WithHref)
    | Action (Snack (WithAction msg))


{-| The Snackbar msgs
-}
type Msg msg
    = EndDelay Int
    | StartDelay Float Posix


default_id : Int
default_id =
    -1


{-| From MD spec - <https://material.io/design/components/snackbars.html#behavior>

> Snackbars appear without warning, and don't require user interaction. They automatically disappear from the screen after a minimum of four seconds, and a maximum of ten seconds.

If you need to customize, you can. The auto-hide in example below is 6 seconds.

      Snackbar.CustomDelay 6000

-}
type AutoHide
    = ShowForever
    | DefaultDelay
    | CustomDelay Float


delayCmd : Float -> Cmd (Msg msg)
delayCmd ms =
    Task.perform (StartDelay ms) Time.now


{-| Show a snackbar with only a message. DefaultDelay is 4 seconds

      Snackbar.message DefaultDelay "Hi!"

-}
message : AutoHide -> String -> ( Snackbar msg, Cmd (Msg msg) )
message delay str =
    ( Message { str = str, id = default_id }
    , case delay of
        ShowForever ->
            Cmd.none

        DefaultDelay ->
            delayCmd 4000

        CustomDelay ms ->
            delayCmd ms
    )


{-| Show a snackbar with a link to another url. DefaultDelay is 10 seconds which gives a user time to click

      Snackbar.link DefaultDelay "Your upload is ready." "GO" "https://path-to-thing"

-}
link : AutoHide -> String -> String -> String -> ( Snackbar msg, Cmd (Msg msg) )
link delay str btn target =
    ( Href
        { str = str
        , id = default_id
        , btn = btn
        , ref = target
        }
    , case delay of
        ShowForever ->
            Cmd.none

        DefaultDelay ->
            delayCmd 10000

        CustomDelay ms ->
            delayCmd ms
    )


{-| Show a snackbar with a button that issues a msg. DefaultDelay is 10 seconds which gives a user time to click

      Snackbar.action DefaultDelay "Your thing was deleted." "UNDO" (UndoDelete)

-}
action : AutoHide -> String -> String -> msg -> ( Snackbar msg, Cmd (Msg msg) )
action delay str btn ref =
    ( Action
        { str = str
        , id = default_id
        , btn = btn
        , ref = ref
        }
    , case delay of
        ShowForever ->
            Cmd.none

        DefaultDelay ->
            delayCmd 10000

        CustomDelay ms ->
            delayCmd ms
    )


{-| Hide the snackbar. You might use it in an update function like this

      { model | snackbar = Snackbar.hidden }

-}
hidden : Snackbar msg
hidden =
    None


unboxId : Snackbar msg -> Int
unboxId model =
    case model of
        Message { id } ->
            id

        Href { id } ->
            id

        Action { id } ->
            id

        _ ->
            0


{-| Tells if the snackbar currently visible.
-}
visible : Snackbar msg -> Bool
visible mod =
    case mod of
        None ->
            False

        _ ->
            True


{-| Update function for mapping into your app's update

      type Msg =
        SnackMessage (Snackbar.Msg Msg)

      SnackMessage submsg ->
        let
          ( sb, cmd ) =
              Snackbar.update submsg model.snackbar
        in
        ( { model | snackbar = sb }, Cmd.map SnackMessage cmd )

-}
update : Msg msg -> Snackbar msg -> ( Snackbar msg, Cmd (Msg msg) )
update msg model =
    case msg of
        EndDelay id_to_hide ->
            if unboxId model == id_to_hide then
                ( None, Cmd.none )

            else
                ( model, Cmd.none )

        StartDelay millis ticks ->
            if unboxId model == default_id then
                let
                    id =
                        posixToMillis ticks
                in
                case model of
                    None ->
                        ( model, Cmd.none )

                    Message x ->
                        ( Message { x | id = id }, Task.perform (always <| EndDelay id) <| Process.sleep millis )

                    Href x ->
                        ( Href { x | id = id }, Task.perform (always <| EndDelay id) <| Process.sleep millis )

                    Action x ->
                        ( Action { x | id = id }, Task.perform (always <| EndDelay id) <| Process.sleep millis )

            else
                ( model, Cmd.none )


{-| Render a snackbar. Typical usage:

        Html.map SnackMessage <| Snackbar.view model.snackbar

-}
view : Snackbar msg -> Html msg
view snack =
    case snack of
        Message { str } ->
            div [ class "snackbar sb_message" ]
                [ span [] [ text str ]
                ]

        Href { str, btn, ref } ->
            div [ class "snackbar sb_with_action" ]
                [ span [] [ text str ]
                , a [ class "sb_action", href ref ] [ text btn ]
                ]

        Action { str, btn, ref } ->
            div [ class "snackbar sb_with_action" ]
                [ span [] [ text str ]
                , span [ class "sb_action", onClick ref ] [ text btn ]
                ]

        None ->
            div [ class "snackbar sb_hidden" ]
                [ span [] [ text " " ]
                ]
