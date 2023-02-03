import unittest
import AAAAAAA_Prolog_Game_with_saving as PrologGame


class PrologSavingStatesTicTacToeInit(unittest.TestCase):
    """set Up for every test case"""

    def setUp(self):
        self.game = PrologGame.PrologGameWithSaving("tic_tac_toe")
        self.state = self.game.new_initial_state()


class PrologSavingTicTacToeInit(PrologSavingStatesTicTacToeInit):
    """Test class for initialised state"""
    def test_init_board(self):
        self.assertEqual("[['.', '.', '.'], ['.', '.', '.'], ['.', '.', '.']]", self.state.__str__())

    def test_init_player(self):
        self.assertEqual(0, self.state.current_player())

    def test_init_legalActions(self):
        self.assertEqual([0, 1, 2, 3, 4, 5, 6, 7, 8], self.state.legal_actions())

    def test_init_isTerminal(self):
        self.assertFalse(self.state.is_terminal())

    def test_init_returns(self):
        self.assertEqual([0.0, 0.0], self.state.returns())

    def test_move(self):
        self.state.apply_action(4)
        self.assertEqual("[['.', '.', '.'], ['.', 'x', '.'], ['.', '.', '.']]", self.state.__str__())
        self.assertEqual(1, self.state.current_player())
        self.assertEqual([0, 1, 2, 3, 5, 6, 7, 8], self.state.legal_actions())
        self.assertFalse(self.state.is_terminal())
        self.assertEqual([0.0, 0.0], self.state.returns())

    def test_illegal_move(self):
        strings = ["[['.', '.', '.'], ['.', 'x', '.'], ['.', '.', '.']]", "4", "failed"]

        self.state.apply_action(4)
        with self.assertRaises(ValueError) as cm:
            self.state.apply_action(4)

        the_exception = cm.exception
        for s in strings:
            self.assertTrue(s in the_exception.__str__())
