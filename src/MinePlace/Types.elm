----------------------------------------------------------------------
--
-- SharedTypes.elm
-- Types used everywhere.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module MinePlace.Types exposing
    ( Appearance(..)
    , Board
    , BoardSpec
    , Cell
    , Colors
    , Direction(..)
    , ErrorKind(..)
    , FullPlayer
    , Game
    , GameDescription
    , GameName
    , GamePlayer
    , Image(..)
    , Layout(..)
    , Location
    , Message(..)
    , Model
    , Msg(..)
    , Operation(..)
    , OwnedPlace
    , OwnedPlacement
    , PaintedWall
    , PaintedWalls
    , Player
    , PlayerName
    , Point
    , Row
    , SavedModel
    , SideImages
    , Size
    , Started(..)
    , StaticImages
    , Url
    , WallSpec
    , Walls
    , Write(..)
    , currentBoardId
    , currentPlayerId
    , darkColors
    , defaultSavedModel
    , directionToString
    , initialPlayer
    , lightColors
    , operationToDirection
    , stringToDirection
    , sumLocations
    )

import Array exposing (Array)
import Browser exposing (Document, UrlRequest)
import Dict exposing (Dict)
import Json.Encode as JE exposing (Value)
import Mastodon.PortFunnels as PortFunnels exposing (FunnelDict, Handler(..), State)
import Svg.Button as Button exposing (Button)
import Task exposing (Task)
import Url
import WebSocketFramework.Types exposing (GameId, PlayerId)


type alias Size =
    { width : Float
    , height : Float
    }


type Started
    = NotStarted
    | StartedReadingModel
    | Started


type alias Colors =
    { windowBackground : String
    , textColor : String
    , errorBackground : String
    , errorColor : String
    , borderStroke : String
    , borderFill : String
    , playerStroke : String
    , playerFill : String
    , editorHighlightStroke : String
    , editorHighlightFill : String
    , lineStroke : String
    }


nearWhite : String
nearWhite =
    "#ececec"


nearBlack : String
nearBlack =
    "#141414"


lightColors : Colors
lightColors =
    { windowBackground = "white"
    , textColor = "black"
    , errorBackground = "red"
    , errorColor = "white"
    , borderStroke = "black"
    , borderFill = "white"
    , playerStroke = "black"
    , playerFill = "lightgray"
    , editorHighlightStroke = "lightblue"
    , editorHighlightFill = "lightblue"
    , lineStroke = "black"
    }


darkColors : Colors
darkColors =
    { windowBackground = "darkgray"
    , textColor = nearWhite
    , errorBackground = nearBlack
    , errorColor = "red"
    , borderStroke = nearWhite
    , borderFill = nearBlack
    , playerStroke = "white"
    , playerFill = "darkgray"
    , editorHighlightStroke = "lightblue"
    , editorHighlightFill = "lightblue"
    , lineStroke = nearWhite
    }


type alias Model =
    { msg : Maybe String
    , windowSize : Size
    , board : Board
    , player : Player
    , wallsDict : Dict Location PaintedWalls
    , layout : Layout
    , isTouchAware : Bool
    , delayLeft : Float
    , forwardButton : Button Operation
    , backButton : Button Operation
    , colors : Colors
    , subscription : Maybe ( Float, Operation, Button.Msg )
    , started : Started
    , funnelState : State Msg
    }


type alias SavedModel =
    { layout : Layout
    , colors : Colors
    }


defaultSavedModel : SavedModel
defaultSavedModel =
    { layout = NormalLayout
    , colors = lightColors
    }


type Layout
    = NormalLayout
    | TopViewLayout
    | EditingLayout
    | NoLayout


type Operation
    = TurnRight
    | TurnLeft
    | GoForward
    | GoBack
    | ToggleLayout
    | ToggleWall Direction Location
    | AddColumn Int
    | AddRow Int
    | EditMaze
    | GetMaze
    | SaveMaze


type Write
    = WriteBoard Board
    | WritePlayer Player
    | WriteModel Model


type Msg
    = InitialSize Size
    | Resize Size
    | ToggleColors
    | DownKey String
    | RepeatButtonMsg Float Operation Button.Msg
    | ButtonMsg Operation Button.Msg
    | DoWrites (List Write)
    | ReceiveTask (Result String Message)
    | Process Value
    | Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url.Url


type Direction
    = North
    | South
    | East
    | West


directionToString : Direction -> String
directionToString direction =
    case direction of
        North ->
            "north"

        South ->
            "south"

        East ->
            "east"

        West ->
            "west"


stringToDirection : String -> Maybe Direction
stringToDirection string =
    case string of
        "north" ->
            Just North

        "south" ->
            Just South

        "east" ->
            Just East

        "west" ->
            Just West

        _ ->
            Nothing


operationToDirection : Operation -> Direction
operationToDirection operation =
    case operation of
        GoForward ->
            North

        GoBack ->
            South

        TurnLeft ->
            West

        TurnRight ->
            East

        _ ->
            North


type alias Location =
    ( Int, Int )


sumLocations : Location -> Location -> Location
sumLocations ( r1, c1 ) ( r2, c2 ) =
    ( r1 + r2, c1 + c2 )


type alias Walls =
    { north : Bool
    , south : Bool
    , east : Bool
    , west : Bool
    }


currentPlayerId : String
currentPlayerId =
    "currentPlayer"


type alias Player =
    { id : String
    , boardid : String
    , name : String
    , location : Location
    , direction : Direction
    }


initialPlayer : Player
initialPlayer =
    { id = currentPlayerId
    , boardid = currentBoardId
    , name = "Joe Bob"
    , location = ( 0, 0 )
    , direction = South
    }


type alias Cell =
    { location : Location
    , walls : Walls
    , players : List Player
    }


type alias Row =
    Array Cell


currentBoardId : String
currentBoardId =
    "current"


type alias Board =
    { id : String
    , rows : Int
    , cols : Int
    , contents : Array Row
    }


type alias BoardSpec =
    List String


type alias WallSpec =
    List Bool



{--- Types for the server, and the wire. ---}


{-| (x, y)
-}
type alias Point =
    ( Float, Float )


type alias Url =
    String


{-| This will eventually include an entire SVG image package.

For now, it either a URL of an image or a list of line lists.

-}
type Image
    = UrlImage Url
    | VectorImage (List (List Point))


type alias SideImages =
    { front : List Image
    , back : List Image
    , left : List Image
    , right : List Image
    }


type alias StaticImages =
    { front : Image
    , back : Image
    , left : Image
    , right : Image
    }


type Appearance
    = InvisibleAppearance
    | DefaultAppearance
    | StaticImageAppearance StaticImages
    | VaryingAppearance SideImages


type alias PlayerName =
    String


type alias FullPlayer =
    { id : PlayerId
    , name : PlayerName
    , appearance : Appearance
    , location : Location
    , direction : Direction
    }


type alias PaintedWall =
    { owner : PlayerName
    , location : Location
    , direction : Direction
    , image : Image
    }


type alias PaintedWalls =
    List PaintedWall


type alias GameName =
    String


type alias Game =
    { id : GameId
    , name : GameName
    , description : String
    , owner : PlayerName
    , board : Board
    , playerDict : Dict PlayerName FullPlayer
    , playerNamesDict : Dict Location (List PlayerName)
    , wallsDict : Dict Location PaintedWalls
    }


type alias GameDescription =
    { name : GameName
    , description : String
    , owner : PlayerName
    }


type alias GamePlayer =
    { player : PlayerName
    , gameid : GameId
    }


{-| Stored in server database.

The salt is mixed with the `passwordHash` in the `LoginWithPasswordReq`
Message, and hashed to the `hash`.

-}
type alias Account =
    { userid : String
    , oauthProvider : Maybe String
    , salt : String
    , hash : String

    -- These are stored under "<hash salt userid>.currentGame"
    -- and ".allGames" -> List Int, and .game.<int> -> Player.
    , currentGame : GamePlayer
    , allGames : List GamePlayer
    }



{---- The wire protocol ----}


type alias OwnedPlace =
    { player : GamePlayer
    , location : Location
    }


type alias OwnedPlacement =
    { player : GamePlayer
    , location : Location
    , direction : Direction
    }


type ErrorKind
    = ValidationFailedError
    | UnknownPlayerIdError PlayerId
    | UnknownPlayerError GamePlayer
    | IllegalMoveError OwnedPlace
    | IllegalWallLocationError OwnedPlacement
    | UnknownAppearanceError String
    | UnknownImageError String
    | RandomError String


type alias Error =
    { kind : ErrorKind
    , message : String
    }


type
    Message
    -- A request for a pong, just to ensure the server is up.
    = PingReq { message : String }
    | PongRsp { message : String }
    | TaskReq { task : Task String Message }
    | CmdReq { cmd : Cmd Msg }
      -- Returned when an error occurs
    | ErrorRsp
        { error : ErrorKind
        , message : String
        }
      -- When I figure out how to do it, there will also be
      -- a way to login from an oauthProvier
      -- passwordHash is a simple hash of the user-typed password
    | CheckLoginReq { userid : String }
    | CheckLoginRsp { userid : String, exists : Bool }
    | NewLoginReq { userid : String, passwordHash : String }
    | NewLoginRsp { userid : String }
    | LoginWithPasswordReq { userid : String, passwordHash : String }
      -- Sent to only the requester
    | LoginRsp
        { playerid : PlayerId
        , currentGame : Maybe GameId
        , allGames : List GamePlayer
        }
      -- Logout of the current session. Do NOT exit any games.
    | LogoutReq { playerid : PlayerId }
      -- Sent to everybody, so they all know these players are now inactive.
    | LogoutRsp { players : List GamePlayer }
      -- Join an existing game as the given player.
      -- Can be either a brand new player, or a player already associated
      -- with this login and the given GameId.
    | JoinGameReq
        { playerid : PlayerId
        , player : GamePlayer
        }
      -- Create a brand new game, with a brand new GameId.
      -- The game is initially private, meaning you have to know its name
      -- to join it.
      -- The game.owner must already be in the passed Game, so
      -- this returns a JoinGameNotificationRsp, not a JoinGameRsp.
      -- If the owner is NOT in the tables, an error is returned.
      --
      -- It would be nice to be able to rename a game. No message for that yet.
    | NewGameReq
        { playerid : PlayerId
        , game : Game
        }
      -- Sent to the joiner only, if he didn't yet have a player in this game.
      -- Otherwise, the joiner will get a JoinGameNotificationRsp, like
      -- everybody else.
    | JoinGameRsp
        { player : GamePlayer
        , game : Game
        }
      -- Sent to all existing members of the game when someone joins.
    | JoinGameNotificationRsp
        { player : GamePlayer
        , location : Location
        , direction : Direction
        }
      -- Sent to become idle, but not exit from a game.
    | LeaveReq
        { playerid : PlayerId
        , player : GamePlayer
        }
      -- Sent to all members of the game when a player leaves.
    | LeaveRsp { player : GamePlayer }
      -- Exit from a game and disown all of your wall images.
    | ExitReq
        { playerid : PlayerId
        , player : GamePlayer
        }
      -- Sent to all members of a game when a player exits.
    | ExitRsp { player : GamePlayer }
      -- Move location and/or change direction.
      -- It will usually be illegal to move more than one square or through a wall.
      -- This may be allowed by some future God mode.
    | MoveReq
        { playerid : PlayerId
        , player : GamePlayer
        , location : Maybe Location
        , direction : Maybe Direction
        }
      -- Sent to all members of a game after a player moves.
    | MoveRsp
        { player : GamePlayer
        , location : Location
        , direction : Direction
        }
      -- Change your appearance.
    | SetApearanceReq
        { playerid : PlayerId
        , player : GamePlayer
        , appearance : Appearance
        }
      -- Sent to all members of a game when a player changes his appearance.
    | SetAppearanceRsp
        { player : GamePlayer
        , appearance : Appearance
        }
      -- Paint a wall, or change the painting if the existing painting is
      -- either yours or unowned.
      -- If `Nothing` is sent for the `image`, unpaints.
    | PaintWallReq
        { playerid : PlayerId
        , player : GamePlayer
        , location : Location
        , direction : Direction
        , image : Maybe Image
        }
      -- Sent to all members when a member paints/unpaints a wall.
    | PaintWallRsp
        { player : GamePlayer
        , location : Location
        , direction : Direction
        , image : Maybe Image
        }
      -- Make a game that you own public.
    | ListGameReq
        { playerid : PlayerId
        , player : GamePlayer
        }
      -- Make a game that you own no longer public.
      -- Optionally switch its ownership.
      -- If switchOwnership is `Just ""`, make the game unowned.
      -- Otherwise, a new owner must be a member.
      -- As soon as all members exit a private game, it will be destroyed.
    | UnlistGameReq
        { playerid : PlayerId
        , gameid : GameId
        , switchOwnership : Maybe PlayerName
        }
      -- Sent to all members when a game is listed or unlisted.
    | ListGameRsp
        { player : GamePlayer
        , isListed : Bool
        }
      -- Request a list of public games.
    | GetListedGamesReq { playerid : PlayerId }
      -- The list of public games.
    | GetListedGamesRsp { games : List GameDescription }
      -- Send a chat message to a game.
    | ChatReq
        { playerid : PlayerId
        , player : GamePlayer
        , message : String
        }
      -- Receive that chat message.
    | ChatRsp
        { player : GamePlayer
        , message : String
        }
      -- Save an appearance for lookup by ListAppearancesReq or GetAppearanceReq
    | SaveAppearanceReq
        { playerid : PlayerId
        , name : String
        , appearance : Appearance
        }
      -- Acknowledge a SaveAppearanceReq
    | SaveAppearanceRsp { name : String }
      -- Request a list of saved appearance names
    | ListAppearancesReq { playerid : PlayerId }
      -- Return that list of saved appearance names
    | ListAppearancesRsp { names : List String }
      -- Request a saved appearance
    | GetAppearanceReq
        { playerid : PlayerId
        , name : String
        }
      -- Return that saved appearance
    | GetAppearanceRsp
        { name : String
        , appearance : Appearance
        }
      -- Save an image for lookup by ListImagesReq or GetImageReq
    | SaveImageReq
        { playerid : PlayerId
        , name : String
        , image : Image
        }
      -- Acknowledge a SaveImageReq
    | SaveImageRsp { name : String }
      -- Request a list of saved appearance names
    | ListImagesReq { playerid : PlayerId }
      -- Return that list of saved appearance names
    | ListImagesRsp { names : List String }
      -- Request a saved appearance
    | GetImageReq
        { playerid : PlayerId
        , name : String
        }
      -- Return that saved appearance
    | GetImageRsp
        { name : String
        , image : Image
        }
