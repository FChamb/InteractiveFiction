module World where

import Data.List
import Text.Read (readMaybe)

data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving Eq

instance Show Object where
   show obj = obj_longname obj

data Exit = Exit { exit_dir :: Direction,
                   exit_desc :: String,
                   room :: String }
   deriving Eq

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object],
                   containers :: [Box] }
   deriving Eq

data Box = Box { box_name :: String,
                  items :: [Object],
                  opened :: Bool }
   deriving Eq

instance Show Box where
   show box
      | opened box && not (items box == []) = box_name box ++ " (containing " ++ concat (intersperse ", " (map obj_name (items box))) ++ "). "
      | items box == [] = box_name box ++ " (empty)"
      | otherwise = box_name box ++ " (closed)"

data GameData = GameData { location_id :: String, -- where player is
                           world :: [(String, Room)],
                           inventory :: [Object], -- objects player has
                           poured :: Bool, -- coffee is poured
                           caffeinated :: Bool, -- coffee is drunk
                           eaten :: Bool, -- food was eaten
                           brushed :: Bool, -- teeth brushed
                           showered :: Bool, -- took a shower
                           lightOn :: Bool, -- turned on the lights
                           torchLightOn :: Bool, -- turned on torch
                           lightsOnEver :: Bool, -- to track if the player ever turned the lights on (achievement)
                           torchOnEver :: Bool, -- to track if the player ever turned the torch on (achievement)
                           finished :: Bool -- set to True at the end
                         }

instance Show GameData where
    show (GameData locId wrld inv p c e b sh l tl lOnEv tOnEv fin) =
        "GameData \"" ++ locId ++ "\" " ++ (formatGameworld wrld) ++ " " ++ formatInv inv ++
        " " ++ show p ++ " " ++ show c ++ " " ++ show e ++ " " ++ show b ++
        " " ++ show sh ++ " " ++ show l ++ " " ++ show tl ++ " " ++ show lOnEv ++
        " " ++ show tOnEv ++ " " ++ show fin

{-
readGameData :: String -> Maybe GameData
readGameData inputString = GameData locId wrld inv p c e b sh l tl lOnEv tOnEv fin
   where
      locId = _ --get first string encased in quotes
      wrld = _ -- get [(String,Room)] string and format as [(String,Room)]
      inv = _ -- get [Object] string and format as [Object]
      p = _ -- first formatted bool
      c = _ -- second
      e = _ -- etc
      b = _
      sh = _
      l = _
      tl = _
      lOnEv = _ 
      tOnEv = _
      fin = _ 
-}

-- example: GameData "bedroom" [("bedroom", bedroom), ("bathroom", bathroom), ("kitchen", kitchen), ("hall", hall), ("street", street)] [] False False False False False False False False False False

won :: GameData -> Bool
won gd = location_id gd == "street"

instance Show Room where
    show (Room desc exits objs boxes) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs ++ showBoxes boxes
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x ++ ". "
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
             showBoxes [] = ""
             showBoxes xs = "There is also: " ++ showBoxes' xs
             showBoxes' [x] = show x
             showBoxes' (x:xs) = show x ++ ", " ++ showBoxes' xs

-- Things which do something to an object and update the game state
type Action = Object -> GameData -> (GameData, String)

-- Things which just update the game state
type Rule = GameData -> (GameData, String)

-- Things which define directions
data Direction = North | East | West | South | Out | In
   deriving (Show, Eq)

recipes = [(fullmug, milk, [milkyCoffeeMug, emptyMilk]), (emptyTorch, batteries, [torch]), (eggs, bread, [eggyBread])]

mug, fullmug, milkyCoffeeMug, emptyMilk, coffeepot, torch, emptyTorch, batteries, toothbrush, usedToothbrush, shower, lightswitch, milk, eggs, bread :: Object
mug            = Obj "mug" "a coffee mug" "A coffee mug"
fullmug        = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
milkyCoffeeMug = Obj "mug" "a full and milky coffee mug" "A coffee mug containing freshly brewed milky coffee"
emptyMilk      = Obj "milk" "an empty jug of milk" "An empty plastic container smelling faintly of milk."
coffeepot      = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"
emptyTorch     = Obj "torch" "a black torch" "A black torch with no batteries"
torch          = Obj "torch" "a black torch" "A black torch with fresh batteries"
batteries      = Obj "batteries" "two AA batteries" "Two round AA batteries. They look crunchy. Don't put them in your mouth."
toothbrush     = Obj "toothbrush" "a blue and white toothbrush" "A blue and white toothbrush with toothpaste on it"
usedToothbrush = Obj "used toothbrush" "a blue and white toothbrush" "A blue and white toothbrush with no toothpaste on it. It's still wet."
shower         = Obj "shower" "a shower" "It's a shower. It looks like it's never been used. Ew."
lightswitch    = Obj "lightswitch" "a lightswitch" "It's a lightswitch. What more could you need to know?"
milk           = Obj "milk" "a jug of milk" "It's unclear what animal or plant it came from, but it seems to still be fresh?"
eggs           = Obj "eggs" "a box of eggs" "A box with some eggs. Very droppable."
bread          = Obj "bread" "a loaf of bread" "Bread, with an unknown amount or lack thereof of gluten."
eggyBread      = Obj "eggy bread" "a soggy, eggy loaf of bread" "Like french toast, if you squint. It's raw, but I'm sure that's fine."
frenchToast    = Obj "french toast" "a decent meal of french toast" "A decent breakfast. I'm proud of you!"
foodPoisoning  = Obj "food poisoning" "food poisoning" "You know what you did."
satisfaction   = Obj "satisfaction" "a sense of satisfaction" "Look at you, cooking a proper breakfast. Well done!"
oven           = Obj "oven" "an oven" "A nice little oven. It deserves more love."

kitchenCupboard :: Box
kitchenCupboard = Box "a cupboard" [eggs, bread, batteries] False

bedroom, bathroom, kitchen, hall, street :: Room

bedroom = Room "You are in your bedroom."
               [Exit North "To the north is a kitchen. " "kitchen",
                Exit South "To the south is a bathroom. " "bathroom"]
               [mug]
               []

bathroom = Room "You are in the bathroom. There is a shower and sink."
                [Exit North "To the north is your bedroom. " "bedroom"]
                [toothbrush, shower]
                []

kitchen = Room "You are in the kitchen."
               [Exit South "To the south is your bedroom. " "bedroom",
                Exit West "To the west is a hallway. " "hall"]
               [coffeepot, emptyTorch, milk, oven]
               [kitchenCupboard]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit East "To the east is a kitchen. " "kitchen"]
            []
            []

-- New data about the hall for when we open the door

openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit East "To the east is a kitchen. " "kitchen",
               Exit Out "You can go outside. " "street"]

street = Room "You have made it out of the house."
              [Exit In "You can go back inside if you like. " "hall"]
              []
              []

gameworld = [("bedroom", bedroom),
             ("bathroom", bathroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("street", street)]

initState :: GameData
initState = GameData "bedroom" gameworld [] False False False False False False False False False False

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))

{-readGameData :: String -> Maybe GameData
readGameData str = readMaybe str-}

formatRoomPair :: (String, Room) -> String
formatRoomPair (name, room) = "(\"" ++ name ++ "\", " ++ name ++ ")"

formatGameworld :: [(String, Room)] -> String
formatGameworld rooms = "[" ++ intercalate ", " (map formatRoomPair rooms) ++ "]"

formatInv:: [Object] -> String
formatInv xs = "[" ++ intercalate ", " (map obj_name xs) ++ "]"