"""Test for functionality of PrologGame object on minimax"""
import random

from Prolog_Game import PrologGame
from open_spiel.python.algorithms import minimax

from absl import app


def main(_):
    game = PrologGame("tic_tac_toe")
    perfect_minimax_wins = 0
    perfect_minimax_losses = 0
    num_episodes = 100

    # test perfect play: Min against Max
    for Max in range(2):
        for j in range(num_episodes):
            state = game.new_initial_state()
            while not state.is_terminal():
                _, action = minimax.alpha_beta_search(game, state=state, maximizing_player_id=Max)
                state.apply_action(action)
            rewards = state.returns()
            if rewards[Max] != 0:
                if rewards[Max] == 1:
                    perfect_minimax_wins += 1
                else:
                    perfect_minimax_losses += 1

    minimax_wins = 0
    minimax_losses = 0
    # test2: Max against random
    for Max in range(2):
        for j in range(num_episodes):
            state = game.new_initial_state()
            while not state.is_terminal():
                if state.current_player() == Max:
                    _, action = minimax.alpha_beta_search(game, state=state, maximizing_player_id=Max)
                    state.apply_action(action)
                # not Max is random
                else:
                    legal_actions = state.legal_actions()
                    action = random.choice(legal_actions)
                    state.apply_action(action)

            rewards = state.returns()
            if rewards[Max] != 0:
                if rewards[Max] == 1:
                    minimax_wins += 1
                else:
                    minimax_losses += 1

    print("perfect rations: %s wins, %s losses" % (perfect_minimax_wins, perfect_minimax_losses))
    print("minimax against random: %s wins, %s losses" % (minimax_wins, minimax_losses))


if __name__ == "__main__":
    app.run(main)
