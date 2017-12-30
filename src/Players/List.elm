module Players.List exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, colspan, placeholder)
import Html.Events exposing (onClick, onInput)
import Models exposing (Player)
import Msgs exposing (Msg)
import RemoteData exposing (WebData)
import Routing exposing (playerPath)


view : WebData (List Player) -> Maybe Player -> Html Msg
view response new =
    div []
        [ maybeList response new
        ]

maybeList : WebData (List Player) -> Maybe Player -> Html Msg
maybeList response new =
    case response of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success players ->
            list players new

        RemoteData.Failure error ->
            text (toString error)


list : List Player -> Maybe Player -> Html Msg
list players new =
    div [ class "p2" ]
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Id" ]
                    , th [] [ text "Name" ]
                    , th [] [ text "Level" ]
                    , th [colspan 2] [ text "Actions" ]
                    ]
                ]
            , tbody [] ( playerRow (Maybe.withDefault (Models.Player "0" "" 0) new) :: List.map playerRow players)
            ]
        , text "Add new: "
        , input [ placeholder "Id" ][]
        , input [ placeholder "Name", onInput Msgs.ChangeName ][]
        , button [ ] [ text "OK" ]
        ]


playerRow : Player -> Html Msg
playerRow player =
    tr []
        [ td [] [ text player.id ]
        , td [] [ text player.name ]
        , td [] [ text (toString player.level) ]
        , td []
            [ editBtn player ]
        , td []
            [ removeBtn player ]
        ]


editBtn : Player -> Html.Html Msg
editBtn player =
    let
        path =
            playerPath player.id
    in
        a
            [ class "btn regular"
            , href path
            ]
            [ i [ class "fa fa-pencil mr1" ] [], text "Edit" ]

removeBtn : Player -> Html.Html Msg
removeBtn player =
    let
        message =
            Msgs.DeletePlayer player
    in
        a
            [ class "btn regular"
            , onClick message
            ]
            [ i [ class "fa fa-remove" ] [], text "Remove" ]
