module Main where

import World
import Actions

import Control.Monad
import System.IO
import System.Exit

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Prelude


{-
Make win message takes a GameData state and returns a string. This function
enables the repl loop to create an output depending on the various choices the
user made in their play through of the game.
-}
makeWinMessage :: GameData -> String
makeWinMessage gd = winMessage
   where
      startWinMessage = "Congratulations, you have made it out of the house.\n"
      cleanMessage
         | brushed gd && showered gd = "You brushed your teeth and took a shower. Congratulations on being a hygienic member of society.\n"
         | showered gd = "You took a shower but forgot to brush your teeth. Congratulations on being an almost hygienic member of society.\n"
         | brushed gd = "You brushed your teeth but forgot to shower. Congratulations on being an almost hygienic member of society.\n"
         | otherwise = "You didn't brush your teeth or take a shower. You disgust me.\n"
      eatMessage
         | eaten gd = "You ate something! Congratulations on being a functional human being.\n"
         | otherwise = "You forgot to eat breakfast. I hope you have time to go to Pret before class...\n"
      foodMessage
         | carrying gd satisfaction = "In fact, you made a fantastic meal. You should be proud of yourself! [Achievement: Michelin Star]\n"
         | carrying gd foodPoisoning = "You did, however, consume something deeply questionable. Your stomach will not be thanking you later.\n"
         | otherwise = ""
      lightMessage
         | not (lightsOnEver gd || torchOnEver gd) = "...entirely in the dark? How did you do that? [Achievement: Echolocation]\n"
         | not (lightsOnEver gd) = "...without using the light switch once? Why did you do it all by torch light? [Achievement: Paranormal Investigator]\n"
         | not (lightOn gd) = "...and you turned the light off before you left! How eco-friendly.\n"
         | otherwise = ""
      baristaMessage
         | barista gd = "You even made a \"fancy\" coffee while you're at it. Well done. [Achievement: Barista]\n"
         | otherwise = ""
      gameMessage
         | score == 0 = "You didn't shower, brush your teeth or eat. You do know that's the bare minimum of a morning routine, right?\n"
         | score == 3 = "You showered, brushed your teeth and ate breakfast! I'm very proud of you for doing the bare minimum of human function.\n"
         | otherwise = cleanMessage ++ eatMessage
      score = length (filter (==True) [brushed gd, eaten gd, showered gd])
      endWinMessage = "\nNow go to your lectures..."
      winMessage = "\ESC[32m" ++ startWinMessage ++ gameMessage ++ foodMessage ++ baristaMessage ++ lightMessage ++ endWinMessage ++ "\ESC[0m"

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user.
   added the possibility for a new type of process which accepts
   three arguments. -}
process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case action state cmd arg of
                                 Just fn -> completeAction fn state
                                 Nothing -> (state, "I don't understand.")
process state [cmd]          = case rule cmd of
                                 Just fn -> fn state
                                 Nothing -> (state, "I don't understand.")
process state [cmd,arg,arg'] = case action2 state cmd arg arg' of
                                 Just fn -> completeAction fn state
                                 Nothing -> (state, "I don't understand.")
process state _ = (state, "I don't understand.")

{-
Repl loop which implements Haskeline features. The loop checks if the player
has enabled the lights and changes the output dependent on that. Next the command
is taken in from the user and processed. If the command equals SAVE or LOAD the argument
is instantly processed and calls the save or load functions. Other commands are processed
using the above process functions.
-}
repl :: GameData -> InputT IO GameData
repl state | finished state = return state
repl state = do
                if (lightOn state || torchLightOn state) then outputStrLn (showRoom (getRoomData state)) else outputStrLn "You cannot see anything, the lights are off.\n"
                outputStrLn "What now? "
                cmd <- getInputLine ">> \ESC[31m"
                outputStrLn "\ESC[0m------------------------------------------------------------------\n"
                case cmd of
                    Just fn -> case words fn of
                     ["save", filename] -> do liftIO (save state filename)
                                              repl state

                     ["load", filename] -> do processLoad state (load filename)

                     otherCommand -> do
                           let (state', msg) = process state otherCommand
                               message = "\ESC[97m" ++ msg ++ "\ESC[0m"
                           outputStrLn message
                           if won state'
                              then do
                                 outputStrLn (makeWinMessage state')
                                 return state'
                              else repl state'
                    Nothing -> do outputStrLn "Enter a command: "
                                  repl state

{-
This is the function called when the game states. Basically just starts
the repl loop using Haskeline.
-}
main :: IO ()
main = do putStr "------------------------------------------------------------------\n[Game start! Type help for list of commands and quit to exit.]\n"
          runInputT defaultSettings (repl initState)
          return ()

{-
Save takes a game data state and a filename. Using writeFile, the
game data is converted to a string using show and then written to the
provided file name is the saves folder.
-}

save :: GameData -> String -> IO ()
save gd filename = do writeFile ("saves/" ++ filename) (show gd)
                      putStrLn ("Saved game! Saved to saves/" ++ filename ++ ". \n")
                      return ()

processLoad :: GameData -> IO (GameData, Bool) -> InputT IO GameData
processLoad gd ioAction = do (loadedGameData, success) <- liftIO ioAction
                             case success of
                                 True  -> do liftIO $ putStrLn "Loaded successfully.\n"
                                             repl loadedGameData
                                 False -> do liftIO $ putStrLn "Error while loading. Continuing game from previous state.\n"
                                             repl gd

{- Load takes a file path and using a do statement reads the file in the saves folder.
Then using the default defined reads function the output is put into a tuple of the
GameData and boolean for successful load.
-}
load :: FilePath -> IO (GameData, Bool)
load filePath = do
                  contents <- readFile ("saves/" ++ filePath)
                  case reads contents :: [(GameData, String)] of
                     [(gameData, "")] -> return (gameData, True)
                     _                -> return (initState, False)