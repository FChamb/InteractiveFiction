module Main where

import World
import Actions

import Control.Monad
import System.IO
import System.Exit

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Prelude

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
      winMessage = startWinMessage ++ gameMessage ++ foodMessage ++ lightMessage ++ endWinMessage

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}


process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case action cmd arg of
                                 Just fn -> completeAction fn state
                                 Nothing -> (state, "I don't understand.")
process state [cmd]          = case rule cmd of
                                 Just fn -> fn state
                                 Nothing -> (state, "I don't understand.")
process state [cmd,arg,arg'] = case action2 cmd arg arg' of
                                 Just fn -> completeAction fn state
                                 Nothing -> (state, "I don't understand.")
process state _ = (state, "I don't understand.")

repl :: GameData -> InputT IO GameData
repl state | finished state = return state
repl state = do
                if (lightOn state || torchLightOn state) then outputStrLn (showRoom (getRoomData state)) else outputStrLn "You cannot see anything, the lights are off.\n"
                outputStrLn "What now? "
                cmd <- getInputLine ">> "
                outputStrLn "------------------------------------------------------------------\n"
                case cmd of
                    Just fn -> case words fn of
                     ["SAVE", filename] -> do liftIO (save state filename)
                                              repl state

                     ["LOAD", filename] -> do processLoad state (load filename)

                     otherCommand -> do
                           let (state', msg) = process state otherCommand
                           outputStrLn msg
                           if won state'
                              then do
                                 outputStrLn (makeWinMessage state')
                                 return state'
                              else repl state'
                    Nothing -> do outputStrLn "Enter a command: "
                                  repl state

main :: IO ()
main = do putStr "------------------------------------------------------------------\n[Game start! Type help for list of commands and quit to exit.]\n"
          runInputT defaultSettings (repl initState)
          return ()

save :: GameData -> String -> IO ()
save gd filename = do writeFile ("saves/" ++ filename) (show gd)
                      putStrLn ("Saved game! Saved to saves/" ++ filename ++ ". \n")
                      return ()
                      -- # maybe edit this so we give our own error message if the save doesn't work (currently ghc gives and exception and exits the game immediately)


processLoad :: GameData -> IO (GameData, Bool) -> InputT IO GameData
processLoad gd ioAction = do (loadedGameData, success) <- liftIO ioAction
  -- now you can use loadedGameData within io monad (seemingly the only way to convert io gamedata to useable gamedata)
                             case success of
                                 True  -> do liftIO $ putStrLn "Loaded successfully.\n"
                                             repl loadedGameData
                                 False -> do liftIO $ putStrLn "Error while loading. Continuing game from previous state.\n"
                                             repl gd
                             
load :: FilePath -> IO (GameData, Bool)
load filePath = do
                  contents <- readFile ("saves/" ++ filePath)
                  case reads contents :: [(GameData, String)] of
                     [(gameData, "")] -> return (gameData, True)
                     _                -> return (initState, False)

  --return (initState, True)
{-
   how to complete:
   1) read string from saves/filename: do fileString <- readFile "../saves/" ++ filename
   2) [ implement working readGameData function which can convert a gamedata string representation to gamedata (output Maybe GameData) ]
   3) let loadData = readGameData fileString
   3) case loadData of
         Just gameData  -> return (loadData, True)
         Nothing        -> return (initState, False)
-}