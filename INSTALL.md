# Monopoly
## CS 3110 Final Project
## Peter Kaplinsky (pk445), Caitlin Stanton (cs968), Alex Wurm (aew98)

### How to Install
1. Download our code

### How to Test
1. See comment in Test.ml, lines 38-46

### How to Build
_Note: The actions needed to run and interact with our code are extremely 
similar to those of A2 and A3_
1. Run `<make clean>` and `<make build>` to compile our code

### How to Play
_Note: The actions needed to run and interact with our code are extremely 
similar to those of A2 and A3_
1. Run `<make play>`
2. Input a JSON file name; we have provided test_board.json and 
standard_board.json, as well as tax_board.json and test_goofj.json for testing
(only JSON files matching our parser will be accepted, otherwise it'll prompt 
another input)
3. Input the number of players you'll be playing with (only integers between 
2 and 6 will be accepted, otherwise it'll prompt another input)
4. Input the amount of wealth a player will have to accumulate to win the 
game (must be an integer between 1700 and 20580, the latter of which is the 
maximum amount of money present in a Monopoly game)

### Valid Actions
* roll 
  * Rolls two dice for the current player and moves their current location based 
on the value of the roll
  * Can only be done once per a player's turn, unless a double is rolled, in 
  which case the player must roll again
  * Must be performed before moving onto the next player
* buy
  * Used to purchase an unowned property that the player is currently located on
  * Player will be prompted to confirm the purchase. Type "yes" or "no".
  * Conditions apply to being able to buy a property (i.e. it's a property, the 
  player has enough money)
* sell `<phrase>`
  * Used to sell the specified property so long as it's in the current player's 
  inventory
  * Player will be prompted to confirm the purchase. Type "yes" or "no".
* next
  * Used to end the current player's turn
  * Must've been preceded by at least one roll command
* quit
  * Ends the game
  * Prints the player(s) that had the maximum amount of wealth, and therefore
  were closest to the win condition
* wallet
  * Prints the amount of cash being held by all players
* items
  * Prints the cards every player possesses
* build `<phrase>`
  * Used to specify the property to build houses/hotels on.
  * To build houses, type "build houses". To build hotels, type "build hotels"
  * Enter the name of the property to build on.
  * Enter the number of houses/hotels to build
* inventories
  * Prints the properties owned by all the players, as well as the number of 
  houses/hotels on eacg
* game
  * For each player, prints the cash in hand, properties owned, and location on
    the board

### Rules for Our Monopoly
1. All players start with no cards, no owned properties, and $1500
2. If a player rolls a double, they must roll again. However, they are 
allowed to perform other actions (i.e. buy, sell) before rolling again, if they
so choose.
3. If a player rolls three doubles consecutively in a single turn or land on 
"Go To Jail", they will be sent to Jail.
4. A player can only leave Jail if they roll a double on future turns, or play 
a "Get Out of Jail Free" card.
5. If a player lands on a square that's owned by another player, they must pay 
rent, which is based on the property's original value, plus the sum of the 
value of any houses or hotels built on that property. This rent also increases 
if all of the same color are owned by a player (a monopoly).
6. If a player lands on a tax card, a random die is rolled to calculate the 
amount of tax to pay (which is $10 multiplied by that die roll).
7. Players can only build one properties that they have a monopoly on.
8. Players can only build hotels once they've built three houses on that same 
monopoly.
9. Once a player passes "GO", they automatically earn $200.
10. If a player lands on a Chance or Community Chest card, they receive a card 
from the top of the deck. Unless that card is a "Get Out of Jail Free" card, 
its effects are applied immediately, otherwise it's stored in the players' 
items list.