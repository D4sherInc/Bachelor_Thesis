"""Testfile
check the returns of every possible query return type
takes returns as Generators
"""

import unittest

import pyswip_returns as pyret

tic_tac_toe = "../prolog/tic_tac_toe.pl"
kuhn_poker = "../prolog/kuhn_poker.pl"
iterated_prisoners_dilemma = "../prolog/iterated_prisoners_dilemma.pl"

ttt_empty_board = [0, 0, 0, 0, 0, 0, 0, 0, 0]


class PySWIPReturnsTest(unittest.TestCase):
    # ----------------------------------------
    # Tic-Tac-Toe Tests
    # ----------------------------------------
    def test_tic_tac_toe_init(self):
        gen = pyret.init_returns(tic_tac_toe)
        g1 = list(gen)
        g2 = g1[0]

        self.assertEqual(g2["InitState"], ttt_empty_board)
        self.assertEqual(g2["Current_Player"], 0)

    def test_tic_tac_toe_terminal_empty(self):
        gen = pyret.is_terminal_returns(tic_tac_toe, ttt_empty_board)
        gen1 = list(gen)
        self.assertFalse(bool(gen1))

    def test_tic_tac_toe_terminal_with_winner(self):
        gen = list(pyret.is_terminal_returns(tic_tac_toe, ["x", 0, 0, "x", 0, 0, "x", 0, 0]))
        gen2 = list(pyret.is_terminal_returns(tic_tac_toe, [0, "o", 0, 0, "o", 0, 0, "o", 0]))
        gen3 = list(pyret.is_terminal_returns(tic_tac_toe, ["x", 0, 0, 0, "x", 0, 0, 0, "x"]))
        self.assertTrue(bool(gen))
        self.assertTrue(bool(gen2))
        self.assertTrue(bool(gen3))

    def test_tic_tac_toe_terminal_end_game_no_winner(self):
        gen = list(pyret.is_terminal_returns(tic_tac_toe, ["x", "o", "x", "o", "o", "x", "x", "x", "o"]))
        self.assertTrue(bool(gen))

    def test_tic_tac_toe_legal_actions_start(self):
        gen = pyret.legal_actions_returns(tic_tac_toe)
        legal_actions = list(gen)[0]["Legal_actions"]
        self.assertEqual([0, 1, 2, 3, 4, 5, 6, 7, 8], legal_actions)

    def test_tic_tac_toe_legal_actions_partially_filled_board(self):
        gen = pyret.legal_actions_returns(tic_tac_toe, 0, 5, 4, 7, 2)
        legal_actions = list(gen)[0]["Legal_actions"]
        self.assertEqual([1, 3, 6, 8], legal_actions)

    def test_tic_tac_toe_legal_actions_winning_board_not_full(self):
        gen = pyret.legal_actions_returns(tic_tac_toe, 0, 1, 3, 4, 6)
        legal_actions = list(gen)[0]["Legal_actions"]
        self.assertEqual([], legal_actions)

    def test_tic_tac_toe_legal_actions_tie(self):
        gen = pyret.legal_actions_returns(tic_tac_toe, 3, 0, 7, 6, 1, 4, 2, 5, 8)
        legal_actions = list(gen)[0]["Legal_actions"]
        self.assertEqual([], legal_actions)

    def test_tic_tac_toe_apply_action(self):
        list_of_dicts = list(pyret.apply_action_returns(tic_tac_toe, 2))
        self.assertEqual(list_of_dicts[0]["NewGameState"], [0, 0, "x", 0, 0, 0, 0, 0, 0])

    def test_tic_tac_toe_apply_action_false(self):
        gen = pyret.apply_action_returns(tic_tac_toe, 2, 2)
        self.assertFalse(gen)
