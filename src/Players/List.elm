module Players.List exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, colspan, placeholder)
import Html.Events exposing (onClick)
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
            , tbody [] ( newPlayerRow new :: List.map editPlayerRow players)
            ]
        ]

newPlayerRow : Maybe Player -> Html Msg
newPlayerRow player =
    let
        toPlayerRow it =
            playerRow it (addButton it)
    in
        Maybe.withDefault nothing (Maybe.map toPlayerRow player)

nothing = text ""

editPlayerRow : Player -> Html Msg
editPlayerRow player =
    playerRow player (editButtons player)

playerRow : Player -> List (Html Msg) -> Html Msg
playerRow player buttons =
    tr []
            ([ td [] [ text player.id ]
            , td [] [ text player.name ]
            , td [] [ text (toString player.level) ]
            ]
            ++
            buttons)

type alias ButtonCreator = Player -> List (Html Msg)

addButton : ButtonCreator
addButton player =
    let
        message =
            Msgs.DeletePlayer player
    in
        [td []
        [a
            [ class "btn regular"
            , onClick message
            ]
            [ i [ class "fa fa-plus" ] [], text "Add" ]
        ]
        ]


editButtons : ButtonCreator
editButtons player =
    [td []
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
