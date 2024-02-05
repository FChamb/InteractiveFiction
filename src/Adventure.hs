module Main where

import World
import Actions

import Control.Monad
import System.IO
import System.Exit

makeWinMessage :: GameData -> String
makeWinMessage gd = winMessage
   where
      startWinMessage = "Congratulations, you have made it out of the house.\n"
      cleanMessage
         | (brushed gd) == True && (showered gd) == True = "You brushed your teeth and took a shower. Congratulations on being a hygienic member of society.\n"
         | (showered gd) == True = "You took a shower but forgot to brush your teeth. Congratulations on being an almost hygienic member of society.\n"
         | (brushed gd) == True = "You brushed your teeth but forgot to shower. Congratulations on being an almost hygienic member of society.\n"
         | otherwise = "You didn't brush your teeth or take a shower. You disgust me.\n"
      eatMessage
         | (eaten gd) == True = "You ate something! Congratulations on being a functional human being."
         | otherwise = "You forgot to eat breakfast. I hope you have time to go to Pret before class..."
      lightMessage
         | not ((lightsOnEver gd) || (torchOnEver gd)) = "...entirely in the dark? How did you do that? [Achievement: Echolocation]\n"
         | not (lightsOnEver gd) = "...without using the light switch once? Why did you do it all by torch light? [Achievement: Paranormal Investigator]\n" 
         | not (lightOn gd) = "...and you turned the light off before you left! How eco-friendly.\n"
         | otherwise = ""  
      gameMessage
         | score == 0 = "You didn't shower, brush your teeth or eat. You do know that's the bare minimum of a morning routine, right?\n"
         | score == 3 = "You showered, brushed your teeth and ate breakfast! I'm very proud of you for doing the bare minimum of human function.\n"
         | otherwise = cleanMessage ++ eatMessage
      score = length (filter (==True) [(brushed gd), (eaten gd), (showered gd)])
      endWinMessage = "\nNow go to your lectures..."
      winMessage = startWinMessage ++ gameMessage ++ lightMessage ++ endWinMessage

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}


process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg]      = case action cmd arg of
                                 Just fn -> completeAction fn state
                                 Nothing -> (state, "I don't understand")
process state [cmd]          = case rule cmd of
                                 Just fn -> fn state
                                 Nothing -> (state, "I don't understand")
process state [cmd,arg,arg'] = case action2 cmd arg arg' of
                                 Just fn -> completeAction fn state
                                 Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")

repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do 
                if (lightOn state || torchLightOn state) then print state else putStr "You cannot see anything, the lights are off.\n"
                putStr "What now? "
                hFlush stdout
                cmd <- getLine
                let (state', msg) = process state (words cmd)
                putStrLn msg
                if (won state') then do putStr (makeWinMessage state')
                                        return state'
                               else repl state'

main :: IO ()
main = do putStr "[Game start! Type help for list of commands and quit to exit.]\n\n"
          repl initState
          return ()