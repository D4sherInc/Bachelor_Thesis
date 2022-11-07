import pyswip
from pyswip import Prolog
import random

prolog = Prolog()
prolog.consult("tic_tac_toe.pl")

_ = 0

newBoard = [
        _, _, _,
        _, _, _,
        _, _, _
]


def pretty_print(board):
    """prints current board
    in a 3x3 shape
    """
    print("-------")
    i = 1
    for e in board:
        print("%s" % e if e else '_', end='|' if i % 3 else '\n')
        i = i + 1
    print("-------")


def get_legal_moves(board):
    """get non-occupied tiles
    returns list of available indexes
    """
    result = list(prolog.query("Board=%s, legal_moves(Board, Legal_Moves)" % board, maxresult=2))
    if result:
        return result[0]['Legal_Moves']
    else:
        return False


def legal_print(legal_moves):
    """prints current legal moves
    in a 3x3 board"""
    for i in range(1, 10):
        print("%s" % i if i in legal_moves else '_', end='|' if i % 3 else '\n')


def apply_move(board, next_move, player):
    """apply the given next_move by player to board
    returns the new board
    """
    result = list(prolog.query("Board=%s, Player=%s, Move=%s, move(Board, Player, Move, NewBoard)"
                               % (board, player, next_move)))
    return _translate_board_from_prolog(result[0]['NewBoard'])


def check_game_state(board, legal_moves):
    """checks board if game is finished
    returns a list of set variables if 'Board' is set and Goal is executed until the end
    if no more moves are available, game is ended
    returns an empty list if goal was not successfully executed (=Goal failed, 'win(Board, Player)' is false)
    """
    # game is finished, but it is a tie
    if len(legal_moves) == 0:
        return False, "No one. It's a Cat's game!"

    b = _translate_board_from_prolog(board)
    r = list(prolog.query("Board = %s, wingame(Board, Player)" % b))
    if r:
        return False, r[0]['Player']
    else:
        return True, "No one"


def _translate_board_from_prolog(board):
    """translates prolog variables into python representation
    works only for one specified variable out of prolog
    for multiple translations: call this method for each variable
    returns: python-usable representation of 'board'
    """
    for index, el in enumerate(board):
        board[index] = el.value if isinstance(el, pyswip.Atom) else el
    return board


def get_next_player(current_player):
    """returns the other player
    """
    player = list(prolog.query("%s = X, other_player(X, Y)" % current_player))
    return player[0]['Y']


def play():
    """one playthrough
    interaction with player through input
    computer decides randomly by chance
    """
    board = newBoard
    current_player = 'o'
    print("-------------TIC-TAC-TOE-------------\nThe Board looks like this:\n")
    pretty_print(board)
    legal_moves = get_legal_moves(board)
    print("wanna play? Do your first move. Legal moves are: ")
    legal_print(legal_moves)
    game_is_running = True
    # ask for move, check for win / termination,
    # make computer move, check for win / termination
    # repeat
    winner = "No one"
    next_move = ""
    while game_is_running:
        # Player's turn
        if current_player == 'o':
            print("your move: 'o'\npress 'L' for legal moves")
            next_move = input()

            if next_move == 'L':
                legal_print(legal_moves)
                continue
            elif int(next_move) not in legal_moves:
                print("not allowed move. Please try again")
                continue

        # Computer's turn
        elif current_player == 'x':
            next_move = random.choice(legal_moves)

        board = apply_move(board, next_move, current_player)
        legal_moves = get_legal_moves(board)
        game_is_running, winner = check_game_state(board, legal_moves)
        current_player = get_next_player(current_player)
        pretty_print(board)

    print("GAME IS OVER")
    print("Winner is: %s" % winner)


def main():
    print("this is the pyswip-test_tic_tac_toe file")
    play()


if __name__ == "__main__":
    prolog = Prolog()
    main()
