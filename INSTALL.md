# Monopoly
## CS 3110 Final Project
## Peter Kaplinsky (pk445), Caitlin Stanton (cs968), Alex Wurm (aew98)

### How to Install
1. Download our code

### How to Test
1. See comment in Test.ml, lines 38-46

### How to Build
_Note: The actions needed to run and interact with our code are extremely similar
to those of A2 and A3_
1. Run `<make clean>` and `<make build>` to compile our code

### How to Play
_Note: The actions needed to run and interact with our code are extremely similar
to those of A2 and A3_
1. Run `<make play>`
2. Input a JSON file name; we have provided test_board.json and standard_board.json 
(only JSON files matching our parser will be accepted, otherwise it'll prompt another
input)
3. Input the number of players you'll be playing with (only integers between 2 and 6 
will be accepted, otherwise it'll prompt another input)

### Valid Actions
* roll 
  * Rolls two dice for the current player and moves their current location based 
on the value of the roll
  * Can only be done once per a player's turn
  * Must be performed before moving onto the next player
* buy
  * Used to purchase an unowned property that the player is currently located on
  * Player will be prompted to confirm the purchase. Type "yes" or "no".
* sell `<phrase>`
  * Used to sell the specified property so long as it's in the current player's inventory
  * Player will be prompted to confirm the purchase. Type "yes" or "no".
* next
  * Used to end the current player's turn
  * Must've been preceded by at least a roll command
* quit
  * Ends the game
* wallet
  * Prints the amount of cash being held by the current player
* items
  * Prints the cards every player possesses.
* build `<phrase>`
  * Used to specify the property to build houses/hotels on.
  * To build houses, type "build houses". To build hotels, type "build hotels"
  * Enter the name of the property to build on.
  * Enter the number of houses/hotels to build
* inventories
  * Prints the properties owned by all the players
* game
  * For each player, prints the cash in hand, properties owned, and location on
    the board

