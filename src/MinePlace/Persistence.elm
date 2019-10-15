----------------------------------------------------------------------
--
-- Persistence.elm
-- Functions for maintaining the state of the MinePlace board.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module MinePlace.Persistence exposing
    ( PersistentThing(..)
    , boardIdKey
    , decodePersistentThing
    , modelKey
    , playerIdKey
    , prefix
    , readAllBoardIds
    , readAllBoardPlayerIds
    , readAllWallsKeys
    , readThing
    , writeBoard
    , writeModel
    , writePlayer
    )

import Json.Encode as JE exposing (Value)
import MinePlace.Board exposing (simpleBoard)
import MinePlace.EncodeDecode as ED
import MinePlace.Types
    exposing
        ( Board
        , Location
        , Model
        , Msg(..)
        , PaintedWall
        , PaintedWalls
        , Player
        , SavedModel
        , currentBoardId
        , currentPlayerId
        , defaultSavedModel
        , initialPlayer
        )
import PortFunnel.LocalStorage as LocalStorage exposing (Key, Message, Value)


{-| LocalStorage key prefix.
-}
prefix : String
prefix =
    "minespace"


{-| We don't do simulated storage, so this doesn't need to be in the model.
-}
state : LocalStorage.State
state =
    LocalStorage.initialState prefix


modelKey : String
modelKey =
    "M:"


send : (Value -> Cmd msg) -> Message -> Cmd msg
send cmdPort message =
    LocalStorage.send cmdPort message state


writeModel : Model -> (Value -> Cmd msg) -> Cmd msg
writeModel model cmdPort =
    ED.modelEncoder model
        |> Just
        |> LocalStorage.put modelKey
        |> send cmdPort


playerKey : Player -> String
playerKey player =
    playerIdKey player.boardid player.id


playerIdKey : String -> String -> String
playerIdKey boardid playerid =
    "P:" ++ boardid ++ "/" ++ playerid


boardKey : Board -> String
boardKey board =
    boardIdKey board.id


boardIdKey : String -> String
boardIdKey id =
    "B:" ++ id


wallsKey : Location -> String
wallsKey location =
    "W:" ++ (ED.locationEncoder location |> JE.encode 0)


readAllBoardIds : (Value -> Cmd msg) -> Cmd msg
readAllBoardIds cmdPort =
    LocalStorage.listKeys "B:"
        |> send cmdPort


readAllBoardPlayerIds : (Value -> Cmd msg) -> String -> Cmd msg
readAllBoardPlayerIds cmdPort boardid =
    LocalStorage.listKeys ("P:" ++ boardid ++ "/")
        |> send cmdPort


readAllWallsKeys : (Value -> Cmd msg) -> Cmd msg
readAllWallsKeys cmdPort =
    LocalStorage.listKeys "W:"
        |> send cmdPort


readThing : (Value -> Cmd msg) -> String -> Cmd msg
readThing cmdPort key =
    LocalStorage.get key
        |> send cmdPort


writeBoard : Board -> (Value -> Cmd msg) -> Cmd msg
writeBoard board cmdPort =
    ED.boardEncoder board
        |> Just
        |> LocalStorage.put (boardKey board)
        |> send cmdPort


writePlayer : Player -> (Value -> Cmd msg) -> Cmd msg
writePlayer player cmdPort =
    ED.playerEncoder player
        |> Just
        |> LocalStorage.put (playerKey player)
        |> send cmdPort


type PersistentThingType
    = PersistentBoardType
    | PersistentPlayerType
    | PersistentModelType
    | PersistentWallsType
    | UnknownType


type PersistentThing
    = PersistentBoard Board
    | PersistentPlayer Player
    | PersistentModel SavedModel
    | PersistentWalls PaintedWalls


keyType : String -> PersistentThingType
keyType string =
    if String.startsWith "B:" string then
        PersistentBoardType

    else if String.startsWith "P:" string then
        PersistentPlayerType

    else if String.startsWith "M:" string then
        PersistentModelType

    else if String.startsWith "W:" string then
        PersistentWallsType

    else
        UnknownType


decodePersistentThing : Key -> Value -> Result String PersistentThing
decodePersistentThing key value =
    case keyType key of
        PersistentBoardType ->
            case ED.decodeBoard value of
                Ok board ->
                    Ok <| PersistentBoard board

                Err msg ->
                    Ok <| PersistentBoard simpleBoard

        PersistentPlayerType ->
            case ED.decodePlayer value of
                Ok player ->
                    Ok <| PersistentPlayer player

                Err msg ->
                    Ok <| PersistentPlayer initialPlayer

        PersistentModelType ->
            case ED.decodeModel value of
                Ok model ->
                    Ok <| PersistentModel model

                Err msg ->
                    Ok <| PersistentModel defaultSavedModel

        PersistentWallsType ->
            case ED.decodePaintedWalls value of
                Ok walls ->
                    Ok <| PersistentWalls walls

                Err msg ->
                    Ok <| PersistentWalls []

        _ ->
            Err <| "Unknown key type for \"" ++ key ++ "\""
