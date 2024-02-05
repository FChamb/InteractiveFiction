module Actions where

import World
import Parsing
import Data.Maybe

{-
Data types introduced to instead of the previous
string declarations. A command exists for every one
of the major functions in the game.
-}
data Command = Go Direction   | Get Object   |
               Drop Object    | Pour Object  |
               Examine Object | Drink Object |
               Use Object     | Eat Object   |
               LightsOn Object| Open         |
               OpenC Box      | Combine Object Object
   deriving Show

{-
Action type which translates the user provided input of length
two into valid Command Data types above.
-}
action :: GameData -> String -> String -> Maybe Command
action gd "go" "north" = Just (Go North)
action gd "go" "east" = Just (Go East)
action gd "go" "west" = Just (Go West)
action gd "go" "south" = Just (Go South)
action gd "go" "in" = Just (Go In)
action gd "go" "inside" = Just (Go In)
action gd "go" "out" = Just (Go Out)
action gd "go" "outside" = Just (Go Out)

action gd "get" "mug"
    | checkObj milkyCoffeeMug gd = Just (Get milkyCoffeeMug)
    | checkObj fullmug gd = Just (Get fullmug)
    | otherwise = Just (Get mug)
action gd "get" "coffee" = Just (Get fullmug)
action gd "get" "milk" = Just (Get milk)
action gd "get" "toothbrush"
    | checkObj toothbrush gd = Just (Get toothbrush)
    | otherwise = Just (Get usedToothbrush)
action gd "get" "pot" = Just (Get coffeepot)
action gd "get" "coffeepot" = Just (Get coffeepot)
action gd "get" "torch"
    | checkObj torch gd = Just (Get torch)
    | otherwise = Just (Get emptyTorch)
action gd "get" "shower" = Just (Get shower)
action gd "get" "batteries" = Just (Get batteries)
action gd "get" "eggs" = Just (Get eggs)
action gd "get" "bread" = Just (Get bread)
action gd "get" "oven" = Just (Get oven)

action gd "drop" "mug"
    | checkObj milkyCoffeeMug gd = Just (Drop milkyCoffeeMug)
    | checkObj fullmug gd = Just (Drop fullmug)
    | otherwise = Just (Drop mug)
action gd "drop" "coffee" = Just (Drop fullmug)
action gd "drop" "milk" = Just (Drop milk)
action gd "drop" "toothbrush"
    | checkObj toothbrush gd = Just (Drop toothbrush)
    | otherwise = Just (Drop usedToothbrush)
action gd "drop" "pot" = Just (Drop coffeepot)
action gd "drop" "coffeepot" = Just (Drop coffeepot)
action gd "drop" "torch"
    | checkObj torch gd = Just (Drop torch)
    | otherwise = Just (Drop emptyTorch)
action gd "drop" "batteries" = Just (Drop batteries)
action gd "drop" "eggs" = Just (Drop eggs)
action gd "drop" "bread" = Just (Drop bread)

action gd "pour" "coffee" = Just (Pour coffeepot)

action gd "examine" "mug"
    | checkObj milkyCoffeeMug gd = Just (Examine milkyCoffeeMug)
    | checkObj fullmug gd = Just (Examine fullmug)
    | otherwise = Just (Examine mug)
action gd "examine" "coffee" = Just (Examine fullmug)
action gd "examine" "toothbrush"
    | checkObj toothbrush gd = Just (Examine toothbrush)
    | otherwise = Just (Examine usedToothbrush)
action gd "examine" "coffeepot" = Just (Examine coffeepot)
action gd "examine" "pot" = Just (Examine coffeepot)
action gd "examine" "torch"
    | checkObj torch gd = Just (Examine torch)
    | otherwise = Just (Examine emptyTorch)
action gd "examine" "batteries" = Just (Examine batteries)
action gd "examine" "shower" = Just (Examine shower)
action gd "examine" "lightswitch" = Just (Examine lightswitch)
action gd "examine" "eggs" = Just (Examine eggs)
action gd "examine" "milk" = Just (Examine milk)
action gd "examine" "bread" = Just (Examine bread)
action gd "examine" "satisfaction" = Just (Examine satisfaction)
action gd "examine" "oven" = Just (Examine oven)

action gd "drink" "coffee"
    | checkObj milkyCoffeeMug gd = Just (Drink milkyCoffeeMug)
    | checkObj fullmug gd = Just (Drink fullmug)

action gd "eat" "egg" = Just (Eat eggs)
action gd "eat" "eggs" = Just (Eat eggs)
action gd "eat" "bread" = Just (Eat bread)

action gd "use" "toothbrush" = Just (Use toothbrush)
action gd "use" "shower" = Just (Use shower)
action gd "use" "lightswitch" = Just (LightsOn lightswitch)
action gd "use" "torch" = Just (LightsOn torch)
action gd "use" "oven" = Just (Use oven)

action gd "open" "door" = Just (Open)

action gd "open" "cupboard" = Just (OpenC kitchenCupboard)
action gd "close" "cupboard" = Just (OpenC kitchenCupboard)
action _ _ _ = Nothing

{-
Action2 type which translates the user provided input of length
three into valid Command Data types above.
-}
action2 :: GameData -> String -> String -> String -> Maybe Command
action2 gd "combine" "coffee" "milk" = Just (Combine fullmug milk)
action2 gd "combine" "milk" "coffee" = Just (Combine milk fullmug) -- combine doesn't care about order so when parsing is fixed we don't need to define this twice
action2 gd "combine" "torch" "batteries" = Just (Combine emptyTorch batteries)
action2 gd "combine" "batteries" "torch" = Just (Combine batteries emptyTorch)
action2 gd "combine" "eggs" "bread" = Just (Combine bread eggs)
action2 gd "combine" "bread" "eggs" = Just (Combine eggs bread)

-- until parsed is fixed i'm just going to put the multiword commands here so they work
-- #to do, make get coffee mug/mug and get torch interchangeable so they work for both items
action2 gd "eat" "eggy" "bread" = Just (Eat eggyBread)
action2 gd "eat" "french" "toast" = Just (Eat frenchToast)

action2 gd "drink" "milky" "coffee" = Just (Drink milkyCoffeeMug)

action2 gd "get" "empty" "milk" = Just (Get emptyMilk)
action2 gd "get" "empty" "torch" = Just (Get emptyTorch)
action2 gd "get" "coffee" "mug"
    | checkObj milkyCoffeeMug gd = Just (Get milkyCoffeeMug)
    | checkObj fullmug gd = Just (Get fullmug)
    | otherwise = Just (Get mug)
action2 gd "get" "used" "toothbrush" = Just (Get usedToothbrush)
action2 gd "get" "milky" "coffee" = Just (Get milkyCoffeeMug)
action2 gd "get" "eggy" "bread" = Just (Get eggyBread)
action2 gd "get" "french" "toast" = Just (Get frenchToast)

action2 gd "drop" "empty" "milk" = Just (Drop emptyMilk)
action2 gd "drop" "empty" "torch" = Just (Drop emptyTorch)
action2 gd "drop" "coffee" "mug" 
    | checkObj milkyCoffeeMug gd = Just (Drop milkyCoffeeMug)
    | checkObj fullmug gd = Just (Drop fullmug)
    | otherwise = Just (Drop mug)
action2 gd "drop" "used" "toothbrush" = Just (Drop usedToothbrush)
action2 gd "drop" "milky" "coffee" = Just (Drop milkyCoffeeMug)
action2 gd "drop" "eggy" "bread" = Just (Drop eggyBread)
action2 gd "drop" "french" "toast" = Just (Drop frenchToast)

action2 gd "examine" "milky" "coffee" = Just (Examine milkyCoffeeMug)
action2 gd "examine" "empty" "milk" = Just (Examine emptyMilk)
action2 gd "examine" "coffee" "mug"
    | checkObj milkyCoffeeMug gd = Just (Examine milkyCoffeeMug)
    | checkObj fullmug gd = Just (Examine fullmug)
    | otherwise = Just (Examine mug)
action2 gd "examine" "empty" "torch" = Just (Examine emptyTorch)
action2 gd "examine" "used" "toothbrush" = Just (Examine usedToothbrush)
action2 gd "examine" "eggy" "bread" = Just (Examine eggyBread)
action2 gd "examine" "french" "toast" = Just (Examine frenchToast)
action2 gd "examine" "food" "poisoning" = Just (Examine foodPoisoning)

action2 _ _ _ _ = Nothing

{-
Complete Action takes a specific command and GameData state and
validates the command provided by the user into the defined
Functions below.
-}
completeAction :: Command -> GameData -> (GameData, String)
completeAction (Go direction) gd = go direction gd
completeAction (Get object) gd = get object gd
completeAction (Drop object) gd = put object gd
completeAction (Pour object) gd = pour object gd
completeAction (Examine object) gd = examine object gd
completeAction (Drink object) gd = drink object gd
completeAction (Eat object) gd = eat object gd
completeAction (Use object) gd = use object gd
completeAction (Open) gd = open coffeepot gd
completeAction (OpenC box) gd = openC box gd
completeAction (LightsOn object) gd = switchLight object gd
completeAction (Combine object object2) gd = combine object object2 gd

{-
Rule is a specific type, similar to Action, however these are
defined separately as they do not change game state, but rather
just reveal information to the user.
-}
rule :: String -> Maybe Rule
rule "quit"      = Just quit
rule "inventory" = Just inv
rule "help"      = Just help
rule _           = Nothing

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists. Uses an if then else statement to
   determine what the valid directions are and return the possibilities.
-}
move :: Direction -> Room -> Maybe String
move dir rm = if (result == []) then Nothing else Just (room (head result))
     where result = filter (\x -> dir == exit_dir x) (exits rm)

{- Return True if the object appears in the room.
   Uses elem to see if the object is contained in the room
-}
objectHere :: Object -> Room -> Bool
objectHere o rm = o `elem` (objects rm)

{- Return True (and which container) if the object appears in one of the containers in the room. -}
objectInCupboard :: Object -> Room -> (Bool, Box)
objectInCupboard o rm
   | inBox = (inBox, box)
   | otherwise = (inBox, emptyBox)
   where
      inBox = foldr (||) False (map (inCupboard o) (filter opened (containers rm)))
      emptyBox = Box "" [] False
      box = findBoxContainingItem o (containers rm)

findBoxContainingItem :: Object -> [Box] -> Box
findBoxContainingItem obj boxes = b
   where (b:_) = filter (\box -> obj `elem` items box) boxes

inCupboard :: Object -> Box -> Bool
inCupboard o bx = o `elem` (items bx)

{- Given an object id and a room description, return a new room description
   without that object. Uses filter to find the object in the room and remove
   it.
-}
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

{- Does the inventory in the game state contain the given object?
   Again uses elem to see if the object is in the inventory.
-}
carrying :: GameData -> Object -> Bool
carrying gd obj = elem obj (inventory gd)

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.
Uses move to find the next valid direction and returns the valid game state
if the
-}
go :: Direction -> GameData -> (GameData, String)
go dir state = case move dir (getRoomData state) of
    Just next -> (state {location_id = next}, "Moved successfully.")
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
    | obj == shower = (state, "You can't pick up a whole shower.")
    | obj == oven = (state, "You can't pick up a whole oven.")
    | objectHere obj (getRoomData state) = (state'', "Item added to inventory.")
    | inBox = (statec, "Item added to inventory.")
    | otherwise = (state, "That item is not in this room!")
        where
            -- in the room
            state' = addInv state obj
            rmid = location_id state'
            rmdata = removeObject obj (getRoomData state')
            state'' = updateRoom state' rmid rmdata
            -- in the cupboard
            (inBox, box) = objectInCupboard obj (getRoomData state)
            statec = updateRoom (state {inventory = (inventory state) ++ [obj]}) rmidc room'
            box' = box {items = filter (/= obj) (items box)}
            room = getRoomData state
            room' = room {containers = filter (/= box) (containers room) ++ [box']}
            rmidc = location_id state

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}
put :: Action
put obj state
    | carrying state obj = (state'', "Left item in room.")
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
    | carrying state mug && carrying state coffeepot = (state'', "Poured successfully.")
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
    | not (obj == fullmug || obj == milkyCoffeeMug) = (state, "This is not drinkable.")
    | carrying state obj && (poured state) = (state'', "Drank successfully - refreshing!")
    | otherwise = (state, "You can not drink right now! Did you remember to pour the coffee first?")
        where
            state'
               | obj == milkyCoffeeMug = state {caffeinated = True, barista = True}
               | otherwise = state {caffeinated = True}
            state'' = state' {inventory = filter (/= obj) (inventory state) ++ [mug]}

{- Eat food (depending on what it is you may get food poisoning). Once done, also update the 'eaten' flag in the game state. -}
eat :: Action
eat obj state
    | not (obj == eggs || obj == bread || obj == eggyBread || obj == frenchToast) = (state, "This is not edible.")
    | not (carrying state obj) = (state, "You have to be carrying something to eat it.")
    | obj == eggs = (badState, "You ate raw eggs. For... the protein?")
    | obj == bread = (state'', "You devoured a loaf of bread like a hungry Victorian child.")
    | obj == eggyBread = (badState, "You ate a loaf of bread dipped in egg. Mmm, salmonella.")
    | obj == frenchToast = (goodState, "Ate some delicious French toast.")
        where
            state' = state {eaten = True}
            state'' = state' {inventory = filter (/= obj) (inventory state)}
            goodState = state' {inventory = (inventory state'') ++ [satisfaction]}
            badState = state' {inventory = (inventory state'') ++ [foodPoisoning]}

{- Used to use various objects to implement more puzzles for the player. -}
use :: Action
use obj state
    | (obj == toothbrush) && (location_id state == "bathroom") && (carrying state toothbrush) = (toothState', "Brushed teeth.")
    | (obj == toothbrush) && (carrying state usedToothbrush) = (state, "You've already used this toothbrush, there's no toothpaste left on it.")
    | (obj == toothbrush) && (location_id state == "bathroom") = (state, "What are you gonna brush your teeth with, your fingers?")
    | (obj == toothbrush) && (carrying state toothbrush) = (state, "You need to be at the bathroom sink to brush your teeth, you animal!")
    | (obj == shower) && (location_id state == "bathroom") = (showerState, "Showered successfully. Ignore the fact that the water turned black beneath you, I'm sure it's not important.")
    | (obj == shower) = (state, "...You know you have to shower... *in* the shower, right?")
    | (obj == oven) && (location_id state == "kitchen") && (carrying state eggyBread) = (frenchToastState, "Cooked French toast! Added to inventory.")
    | (obj == oven) && (location_id state == "kitchen") = (state, "Make some eggy bread to cook first.")
    | (obj == oven) = (state, "You must be in the kitchen to use the oven.")
    | otherwise = (state, "You can not use that right now!")
        where
            toothState = state {brushed = True}
            toothState' = toothState {inventory = filter (/= toothbrush) (inventory state) ++ [usedToothbrush]}
            showerState = state {showered = True}
            frenchToastState = state {inventory = filter (/= eggyBread) (inventory state) ++ [frenchToast]}

{-Turn the light on: game can be played with the lights off but no descriptions will be given.
If the player never turns on the light but completes the game they get an achievement. -}
switchLight :: Action 
switchLight obj state
    | (obj == lightswitch) = (lightState, "Lights turned on/off.")
    | (obj == torch) && (carrying state torch) = (torchState, "Switched torch on/off.")
    | otherwise = (state, "You need a torch (with batteries) for this.")
        where
            lightState = state {lightOn = not (lightOn state), lightsOnEver = True}
            torchState = state {torchLightOn = not (torchLightOn state), torchOnEver = True}

{- Open cupboard and then update the room-}
openC :: Box -> GameData -> (GameData, String)
openC box state
    | hasBox box room = (state', "Opened successfully!")
    | hasBox box' room = openC box' state
    | otherwise = (state, "Can't open something that is not in this room")
        where
            state' = updateRoom state rmid room'
            box' = box {opened = not (opened box)}
            room = getRoomData state
            room' = room {containers = filter (/= box) (containers room) ++ [box']}
            rmid = location_id state

{- Does the room contain the given box? -}
hasBox :: Box -> Room -> Bool
hasBox box room = elem box (containers room)

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}
open :: Action
open obj state
    | (caffeinated state) && getRoomData state == hall = (state', "Opened front door.")
    | caffeinated state = (state, "Can't open the door if you're not at the door!")
    | otherwise = (state, "Can't open the door until you drink coffee!")
        where
            state' = updateRoom state rmid rmdata
            rmid = "hall"
            rmdata = (Room openedhall openedexits [] [])

tripleSearch :: Object -> Object -> [(Object, Object, [Object])] -> [Object]
tripleSearch _ _ [] = []
tripleSearch x y ((a,a1,b):xs)
    | (x == a && y == a1) = b
    | (y == a && x == a1) = b
    | otherwise = tripleSearch x y xs

{- Combine is a kind of modified action taking two objects as input, removing both of them from the player's inventory and 
adding the outputs to their inventory. It uses a list of triples where the first two values are the inputs (which it searches
for in any order) and the third is a list of outputs. -}
combine :: Object -> Object -> GameData -> (GameData, String)
combine obj obj2 state
   | outcome == [] = (state, "You cannot combine these.")
   | outcome /= [] && (carrying state obj) && (carrying state obj2) = (state'', "Combined successfully! Check inventory for result.")
   | otherwise = (state, "You can not combine things you haven't picked up!")
        where
            outcome = tripleSearch obj obj2 recipes
            state'' = combineAdd outcome (state')
            state' = state {inventory = filter (\x -> x /= obj && x /= obj2) (inventory state)}

combineAdd :: [Object] -> GameData -> GameData
combineAdd [] gd = gd
combineAdd (x:xs) gd = combineAdd xs gd {inventory = (inventory gd) ++ [x]}

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
      \\n- SAVE (SaveFile) - save the current game state to a name of your choice\
      \\n- LOAD (SaveFile) - load the SaveFile game state to where left off\
      \\n- go (direction) - go to the room to your (direction) [eg. 'go north']\
      \\n- get (object) - pick up an (object) and put it in your inventory (if that object is in the room)\
      \\n- drop (object) - drop an (object) into the room (if that object is in your inventory)\
      \\n- pour (liquid) - pour liquid into a mug (if you have both liquid and an empty mug in your inventory)\
      \\n- examine (object) - get information about an object (if it is in your inventory or in the room)\
      \\n- drink (liquid) - drink a mug of liquid (if you have a mug of liquid)\
      \\n- eat (food) - eat food (only works on eatable objects)\
      \\n- use (object) - use (object) [for shower, toothbrush, torch, lightswitch]\
      \\n- combine (object) (object) - combine two objects together to get something new\
      \\n- open (door/cupboard) - open the front door or a cupboard\
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

checkObj :: Object -> GameData -> Bool
checkObj obj state
    | (carrying state obj) || (objectHere obj room) || (x) = True
    | otherwise = False
        where
            (x, y) = objectInCupboard obj room
            room = (getRoomData state)