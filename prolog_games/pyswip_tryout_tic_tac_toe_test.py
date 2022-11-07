import unittest
from unittest import TestCase
import pyswip_test_tic_tac_toe
from pyswip import Prolog

prolog = Prolog()


class TestPySwipTicTacToe(unittest.TestCase):
    def test_get_next_player(self):
        p = pyswip_test_tic_tac_toe.get_next_player('x')
        self.assertEqual(p, 'o')

    def test_get_next_player2(self):
        p = pyswip_test_tic_tac_toe.get_next_player('o')
        self.assertEqual(p, 'x')

    def test_translate_from_prolog(self):
        board = ['e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e']
        b = list(prolog.query('Board=%s' % board))[0]['Board']
        b2 = pyswip_test_tic_tac_toe._translate_board_from_prolog(b)
        self.assertEqual(b2, board)

    def test_translate_from_prolog2(self):
        board = ['e', 'x', 'o', 'e', 'e', 'e', 'e', 'x', 'e', 'o']
        b = list(prolog.query('Board=%s' % board))[0]['Board']
        b2 = pyswip_test_tic_tac_toe._translate_board_from_prolog(b)
        self.assertEqual(b2, board)

    def test_end_of_game(self):
        board = ['x','x','o',
                 'o','o','x',
                 'x','o','o']
        legal_moves = pyswip_test_tic_tac_toe.get_legal_moves(board)
        game_end, winner = pyswip_test_tic_tac_toe.check_game_state(board, legal_moves)
        self.assertEqual(game_end, True)
        self.assertEqual(winner, "No one")


if __name__ == '__main__':
    unittest.main()
