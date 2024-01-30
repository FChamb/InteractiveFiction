module Actions where

import World
import Parsing
import Data.Maybe

{-
data Command = Go Direction   | Get Object   |
               Drop Object    | Pour Object  |
               Examine Object | Drink Object |
               Open
   deriving Show
-}

actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions _         = Nothing

{-
action :: String -> String -> Maybe Command
action "go" "north" = Just (Go North)
action "go" "east" = Just (Go East)
action "go" "west" = Just (Go West)
action "go south" = Just (Go South)
-}

--completeAction :: GameData -> Command -> (GameData, String)
--completeAction :: 

commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: String -> Room -> Maybe String
move dir rm = if (result == []) then Nothing else Just (room (head result))
     where result = filter (\x -> dir == exit_dir x) (exits rm)

{- Return True if the object appears in the room. -}

objectHere :: String -> Room -> Bool
objectHere o rm = o `elem` (map (\x -> obj_name x) (objects rm))

{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: String -> Room -> Room
removeObject o rm = rm {objects = filter (\x -> (obj_name x /= o)) (objects rm)}

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
updateRoom gd rmid rmdata = if roomExist gd rmid
                               then gd {world=[if (x==rmid) then (rmid, rmdata) else (x,y) | (x,y) <- world gd]}
                               else gd {world=(world gd ++ [(rmid, rmdata)])}

roomExist :: GameData -> String -> Bool
roomExist gd rmid = if filter (\(x,y) -> x == rmid) (world gd) == []
                       then False
                       else True

{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

addInv :: GameData -> String -> GameData
addInv gd obj = let room = getRoomData gd
                    desiredObj = objectData obj room
                in gd {inventory = (inventory gd) ++ [desiredObj]}

{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> String -> GameData
removeInv gd obj
    | carrying gd obj = gd {inventory = filter (\x -> (obj_name x /= obj)) (inventory gd)}
    | otherwise = gd

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> String -> Bool
carrying gd obj = elem obj (map (\x -> obj_name x) (inventory gd))

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Action
go dir state = undefined

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
get obj state = undefined

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

put :: Action
put obj state = undefined

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action
examine obj gd = 
   do
      let objectFull = checkForObj obj gd
      if checkDefined (objectFull) then (gd, (obj_name objectFull ++ obj_desc objectFull))
      else (gd, "No " ++ obj ++ " found.")

{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Action
pour obj state = undefined
   {- if carrying state mug && carrying state coffeepot then
      do 
         state {poured -> True}
-}
{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Action
drink obj state = undefined

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: Action
open obj state = undefined

{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")

checkForObj :: String -> GameData -> Object
checkForObj obj state 
   | carrying state obj = findObj obj (inventory state)
   | objectHere obj (getRoomData state) = objectData obj (getRoomData state)
   | otherwise = Obj "" "" "" -- empty object but check if can get Maybe to work!!

checkDefined :: Object -> Bool
checkDefined x
   | (obj_name x) == "" || (obj_longname x) == "" || (obj_desc x) == "" = False
   | otherwise = True

