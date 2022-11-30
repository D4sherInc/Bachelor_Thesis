import random
import numpy as np
import pyspiel

from pyswip_tic_tac_toe_game import TicTacToeGame as TTTGame
from open_spiel.python import rl_environment
from open_spiel.python.algorithms import random_agent
from open_spiel.python.algorithms import tabular_qlearner

import logging
from absl import app
from absl import flags

FLAGS = flags.FLAGS

flags.DEFINE_string("game", "tic_tac_toe", "Name of the game")
flags.DEFINE_integer("players", None, "Number of players")
flags.DEFINE_string("load_state", None,
                    "A file containing a string to load a specific state")

flags.DEFINE_integer("num_episodes", int(5e4), "Number of train episodes.")
flags.DEFINE_boolean(
    "iteractive_play", True,
    "Whether to run an interactive play with the agent after training.")


def eval_against_random_bots(env, agents, random_agents, param):
    pass


def main(_):
    print("trying to create a game:" + FLAGS.game)
    game = TTTGame()

    env = rl_environment.Environment(game)
    num_actions = env.action_spec()["num_actions"]
    n_players = env.num_players

    agents = [
            tabular_qlearner.QLearner(player_id=idx, num_actions=num_actions)
            for idx in range(n_players)
    ]

    random_agents = [
            random_agent.RandomAgent(player_id=idx, num_actions=num_actions)
            for idx in range(n_players)
    ]

    training_episodes = FLAGS.num_episodes
    for cur_episode in range(training_episodes):
        if cur_episode % int(1e4) == 0:
            win_rates = eval_against_random_bots(env, agents, random_agents, 1000)
            logging.info("Starting episode %s, win_rates %s", cur_episode, win_rates)
        time_step = env.reset()
        print(time_step)

    state = game.new_initial_state()
    while not state.is_terminal:
        print(str(state))

        action = random.choice(state.legal_actions())

        action_string = state.action_to_string(state.current_player(), action)
        print("samples action: " + action_string)

        state.apply_action(action)

    print("game finished")


if __name__ == "__main__":
    app.run(main)
