# A Haskell implementation of Mastermind

## This game emulates a game of [Mastermind](https://en.wikipedia.org/wiki/Mastermind_(board_game)) with three different modes:

1. Human solver: in this mode the computer creates a secret code and the user tries to solve it on the console. This is the defeault mode
2. Hill Climbing: in this mode it is the user that thinks of a secret code and the computer uses a Heuristic Hill Climbing algorithm to solve it
3. Web Server: This mode is the backend server for my [Elm Fronend](https://github.com/maorleger/elm-mastermind)

## Usage
1. Install Haskell Stack
2. run `stack build && stack exec mastermind [ARG]` where ARG can be one of:
  1. `hill_climbing` -> for the hill climbing console mode
  2. `server_mode` -> for the hill climbing server mode (note you'll need to enable CORS in order to communicate between the different ports). For this mode you'll need to set the PORT environment variable prior to running
  3. Or if left blank it will default to the human solver mode
