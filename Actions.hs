module Actions where

import World
import Parsing
import Data.Maybe

data Command = Go Direction   | Get Object   |
               Drop Object    | Pour Object  |
               Examine Object | Drink Object |
               Use Object     | Open         |
               LightsOn Object| Combine Object Object
   deriving Show

{-
actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions _         = Nothing
-}

action :: String -> String -> Maybe Command
action "go" "north" = Just (Go North)
action "go" "east" = Just (Go East)
action "go" "west" = Just (Go West)
action "go" "south" = Just (Go South)
action "go" "in" = Just (Go In)
action "go" "inside" = Just (Go In)
action "go" "out" = Just (Go Out)
action "go" "outside" = Just (Go Out)
action "get" "mug" = Just (Get mug)
action "get" "coffee mug" = Just (Get mug)
action "get" "toothbrush" = Just (Get toothbrush)
action "get" "pot" = Just (Get coffeepot)
action "get" "coffeepot" = Just (Get coffeepot)
action "get" "torch" = Just (Get torch)
action "drop" "mug" = Just (Drop mug)
action "drop" "toothbrush" = Just (Drop toothbrush)
action "drop" "pot" = Just (Drop coffeepot)
action "drop" "torch" = Just (Drop torch)
action "pour" "coffee" = Just (Pour coffeepot)
action "examine" "mug" = Just (Examine mug)
action "examine" "toothbrush" = Just (Examine toothbrush)
action "examine" "coffee" = Just (Examine fullmug)
action "examine" "coffeepot" = Just (Examine coffeepot)
action "examine" "pot" = Just (Examine coffeepot)
action "examine" "torch" = Just (Examine torch)
action "drink" "coffee" = Just (Drink fullmug)
action "use" "toothbrush" = Just (Use toothbrush)
action "use" "shower" = Just (Use shower)
action "open" "door" = Just (Open)
action "use" "lightswitch" = Just (LightsOn lightswitch)
action "use" "torch" = Just (LightsOn torch)
action _ _ = Nothing

action2 :: String -> String -> String -> Maybe Command
action2 "combine" "coffee" "milk" = Just (Combine fullmug milk)
action2 _ _ _ = Nothing

completeAction :: Command -> GameData -> (GameData, String)
completeAction (Go direction) gd = go direction gd
completeAction (Get object) gd = get object gd
completeAction (Drop object) gd = put object gd
completeAction (Pour object) gd = pour object gd
completeAction (Examine object) gd = examine object gd
completeAction (Drink object) gd = drink object gd
completeAction (Use object) gd = use object gd
completeAction (Open) gd = open coffeepot gd
completeAction (LightsOn object) gd = switchLight object gd
completeAction (Combine object object2) gd = combine object object2 gd

rule :: String -> Maybe Rule
rule "quit"      = Just quit
rule "inventory" = Just inv
rule "help"      = Just help
rule _           = Nothing

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: Direction -> Room -> Maybe String
move dir rm = if (result == []) then Nothing else Just (room (head result))
     where result = filter (\x -> dir == exit_dir x) (exits rm)

{- Return True if the object appears in the room. -}

objectHere :: Object -> Room -> Bool
objectHere o rm = o `elem` (objects rm)

{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: Object -> Room -> Room
removeObject o rm = rm {objects = filter (/= o) (objects rm)}

{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = rm {objects = (objects rm) ++ [o]}

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}

findObj :: String -> [Object] -> Object
findObj o ds = head (filter (\x -> (obj_name x) == o) ds)

{- Use 'findObj' to find an object in a room description -}

objectData :: String -> Room -> Object
objectData o rm = findObj o (objects rm)

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = if lookup rmid (world gd) /= Nothing
                               then gd {world=[if (x==rmid) then (rmid, rmdata) else (x,y) | (x,y) <- world gd]}
                               else gd {world=(world gd ++ [(rmid, rmdata)])}

{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

addInv :: GameData -> Object -> GameData
addInv gd obj
    | objectHere obj (getRoomData gd) = gd {inventory = (inventory gd) ++ [obj]}
    | otherwise = gd

{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> Object -> GameData
removeInv gd obj
    | carrying gd obj = gd {inventory = filter (\x -> x /= obj) (inventory gd)}
    | otherwise = gd

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> Object -> Bool
carrying gd obj = elem obj (inventory gd)

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Direction -> GameData -> (GameData, String)
go dir state = case move dir (getRoomData state) of
    Just next -> (state {location_id = next}, "OK")
    Nothing -> (state, "You can not move that direction!")

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}

get :: Action
get obj state
    | obj == shower = (state, "You cannot pick up a shower! Who are you, the Hulk?")
    | objectHere obj (getRoomData state) = (state'', "OK")
    | otherwise = (state, "That item is not in this room!")
        where
            state' = addInv state obj
            rmid = location_id state'
            rmdata = removeObject obj (getRoomData state')
            state'' = updateRoom state' rmid rmdata

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

put :: Action
put obj state
    | carrying state obj = (state'', "OK")
    | otherwise = (state, "That item is not in your inventory!")
        where
            state'' = updateRoom state' rmid rmdata
            rmdata = addObject obj (getRoomData state')
            rmid = location_id state'
            state' = removeInv state obj

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action
examine obj state =
   do
      let objectFull = checkForObj obj state
      if checkDefined (objectFull) then (state, ("Object: " ++ obj_longname objectFull ++ "\nDescription: " ++ obj_desc objectFull))
      else (state, "No " ++ (obj_name obj) ++ " found.")

{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Action
pour obj state
    | carrying state mug && carrying state coffeepot = (state'', "OK")
    | otherwise = (state, "You can not pour right now!")
        where
            state' = state {poured = True}
            state'' = state' {inventory = filter (/= mug) (inventory state) ++ [fullmug]}

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Action
drink obj state
    | carrying state fullmug && (poured state) = (state'', "OK")
    | otherwise = (state, "You can not drink right now!")
        where
            state' = state {caffeinated = True}
            state'' = state' {inventory = filter (/= fullmug) (inventory state) ++ [mug]}

use :: Action
use obj state
    | (obj == toothbrush) && (getRoomData state == bathroom) && (carrying state toothbrush) = (toothState'', "OK")
    | (obj == toothbrush) && (getRoomData state == bathroom) = (state, "What are you going to brush your teeth with, your fingers?")
    | (obj == toothbrush) && (carrying state toothbrush) = (state, "You need to be at the bathroom sink to brush your teeth, you animal!")
    | (obj == toothbrush) = (state, "You need to go to the bathroom first!")
    | (obj == toothbrush) && (carrying state usedToothbrush) = (state, "You've already used this toothbrush, there's no toothpaste left on it.")
    | (obj == shower) && (getRoomData state == bathroom) = (showerState', "OK")
    | (obj == shower) = (state, "...You know you have to shower... *in* the shower, right?")
    | otherwise = (state, "You can not use that right now!")
        where
            toothState' = state {brushed = True}
            toothState'' = toothState' {inventory = filter (/= toothbrush) (inventory state) ++ [usedToothbrush]}
            showerState' = state {showered = True}

switchLight :: Action -- #comment me and use later
switchLight obj state
    | (obj == lightswitch) = (lightState, "OK")
    | (obj == torch) && (carrying state torch) = (torchState, "OK")
    | otherwise = (state, "You need a torch for this.")
        where
            lightState = state {lightOn = not (lightOn state), lightsOnEver = True}
            torchState = state {torchLightOn = not (torchLightOn state), torchOnEver = True}

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: Action
open obj state
    | (caffeinated state) && getRoomData state == hall = (state', "OK")
    | caffeinated state = (state, "Can't open the door if you're not at the door!")
    | otherwise = (state, "Can't open the door until you drink coffee!")
        where
            state' = updateRoom state rmid rmdata
            rmid = "hall"
            rmdata = (Room openedhall openedexits [])

combine :: Object -> Object -> GameData -> (GameData, String)
combine obj obj2 gd = (gd, "OK")


{- Don't update the game state, just list what the player is carrying -}

inv :: Rule
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

help :: Rule
help state = (state, showCommands)
   where showCommands = "list of commands!!\
      \\n\
      \\n- go (direction) - go to the room to your (direction) [eg. 'go north']\
      \\n- get (object) - pick up an (object) and put it in your inventory (if that object is in the room)\
      \\n- drop (object) - drop an (object) into the room (if that object is in your inventory)\
      \\n- pour (liquid) - pour liquid into a mug (if you have both liquid and an empty mug in your inventory)\
      \\n- examine (object) - get information about an object (if it is in your inventory or in the room)\
      \\n- drink (liquid) - drink a mug of liquid (if you have a mug of liquid)\
      \\n - use (object) - use (object) [for shower, toothbrush, torch, lightswitch]\
      \\n- open (door) - open the front door\
      \\n- inventory - see inventory\
      \\n- help - see list of commands\
      \\n- quit - quit the game\n"

quit :: Rule
quit state = (state { finished = True }, "Bye bye")

{- Helper function to return object by name if in the current room or inventory, returns empty object if not -}

checkForObj :: Object -> GameData -> Object
checkForObj obj state 
   | carrying state obj = findObj (obj_name obj) (inventory state)
   | objectHere obj (getRoomData state) = objectData (obj_name obj) (getRoomData state)
   | otherwise = Obj "" "" "" -- empty object but check if can get Maybe to work!!

{- Helper function to check whether a given object has valid values for name, longname and description -}

checkDefined :: Object -> Bool
checkDefined x
   | (obj_name x) == "" || (obj_longname x) == "" || (obj_desc x) == "" = False
   | otherwise = True

