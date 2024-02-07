# CS2006 Week 4 Practical - Haskell Interactive Fiction

## Description:
#### This project is a text based game interactive fiction written in haskell. The practical implements all the basic requirements defined by the specification. On top of that this implementation completed all the easy and medium additional requirements. A save and load function was created as well as using Haskeline for retrieving user input.

## Instructions:
1. Open the terminal and cd into the project directory.
2. Enter the command **$** cabal update
3. Enter the command **$** cabal clean
4. Enter the command **$** cabal build
5. Enter the command **$** cabal run
6. Play the game.

### Game Commands:
* save (filename) - save the current game state to a name of your choice
* load (filename) - load the SaveFile game state to where left off
* go (direction) - go to the room to your (direction) [eg. 'go north']
* get (object) - pick up an (object) and put it in your inventory (if that object is in the room)
* drop (object) - drop an (object) into the room (if that object is in your inventory)
* pour (coffee) - pour coffee into a mug (if you have both coffee and an empty mug in your inventory)
* examine (object) - get information about an object (if it is in your inventory or in the room)
* drink (liquid) - drink a mug of liquid (if you have a mug of liquid)
* eat (food) - eat food (only works on eatable objects)
* use (object) - use (object) [for shower, toothbrush, torch, lightswitch]
* combine (object) (object) - combine two objects together to get something new
* open (door/cupboard) - open the front door or a cupboard
* inventory - see inventory
* help - see list of commands
* quit - quit the game
