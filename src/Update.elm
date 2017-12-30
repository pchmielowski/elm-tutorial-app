module Update exposing (..)

import Commands exposing (savePlayerCmd, removePlayerCmd)
import Models exposing (Model, Player)
import Msgs exposing (Msg)
import Routing exposing (parseLocation)
import RemoteData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnFetchPlayers response ->
            ( { model | players = response }, Cmd.none )

        Msgs.OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        Msgs.ChangeLevel player howMuch ->
            let
                updatedPlayer =
                    { player | level = player.level + howMuch }
            in
                ( model, savePlayerCmd updatedPlayer )

        Msgs.DeletePlayer player ->
            ( model, removePlayerCmd player )

        Msgs.OnPlayerSave (Ok player) ->
            ( updatePlayer model player, Cmd.none )

        Msgs.OnPlayerSave (Err error) ->
            ( model, Cmd.none )

        Msgs.OnPlayerRemoved (Ok player) ->
            ( removePlayer model player, Cmd.none )

        Msgs.OnPlayerRemoved (Err error) ->
            Debug.crash (toString error)
            -- ( model, Cmd.none )

        Msgs.ChangeName name ->
            ( {model | new = Maybe.Just (Player "0" name 0)}, Cmd.none )


updatePlayer : Model -> Player -> Model
updatePlayer model updatedPlayer =
    let
        pick currentPlayer =
            if updatedPlayer.id == currentPlayer.id then
                updatedPlayer
            else
                currentPlayer

        updatePlayerList players =
            List.map pick players

        updatedPlayers =
            RemoteData.map updatePlayerList model.players
    in
        { model | players = updatedPlayers }

removePlayer : Model -> Player -> Model
removePlayer model removed =
    let
        withoutRemoved currentPlayer =
            removed.id /= currentPlayer.id
            
        updatePlayerList players =
            List.filter withoutRemoved players

        updatedPlayers =
            RemoteData.map updatePlayerList model.players
    in
        { model | players = updatedPlayers }
