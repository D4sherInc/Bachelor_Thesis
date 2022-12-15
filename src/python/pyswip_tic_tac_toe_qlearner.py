""" Tabular Q-Learner Tic Tac Toe, combined with Game Definition in Prolog

Based on tic_tac_toe_qlearner.py from OpenSpiel


Two Q-Learning agents are trained by playing against each other. Then, the game
can be played against the agents from the command line.

After about 10**5 training episodes, the agents reach a good policy: win rate
against random opponents is around 99% for player 0 and 92% for player 1.
"""

import logging

import pyspiel
from absl import app
from absl import flags
import numpy as np
import Prolog_Game as pyswip_tic_tac_toe_game

from open_spiel.python import rl_environment
from open_spiel.python.algorithms import random_agent
from open_spiel.python.algorithms import tabular_qlearner

FLAGS = flags.FLAGS

flags.DEFINE_integer("num_episodes", int(5e4), "Number of train episodes")
flags.DEFINE_boolean(
        "interactive_play", True,
        "Whether tO run an interactive play with the agent after training.")


def pretty_board(time_step):
    """Returns the board in 'time_step' in a human-readable format"""


def command_line_action(time_step):
    """Gets a valid action from the user on the command line."""


def eval_win_results(env, trained_agents, random_agents, num_episodes):
    """Evaluates 'trained_agents' against 'random_agents' for 'num_episodes'"""
    wins = np.zeros(2)
    for player_pos in range(2):
        if player_pos == 0:
            cur_agents = [trained_agents[0], random_agents[1]]
        else:
            cur_agents = [random_agents[0], trained_agents[1]]
        for _ in range(num_episodes):
            time_step = env.reset()
            while not time_step.last():
                player_id = time_step.observations["current_player"]
                agent_output = cur_agents[player_id].step(time_step, is_evaluation=True)
                time_step = env.step([agent_output.action])
            if time_step.rewards[player_pos] > 0:
                wins[player_pos] += 1
    return wins / num_episodes


def main(_):
    # as string:
    # game = "tic_tac_toe"

    # as pyspiel.Game instance
    # game = pyspiel.load_game("tic_tac_toe")

    # as my own pyswip game instance
    game = pyswip_tic_tac_toe_game.PrologGame()

    num_players = 2

    env = rl_environment.Environment(game)
    num_actions = env.action_spec()["num_actions"]

    qlearner_agents = [
            tabular_qlearner.QLearner(player_id=idx, num_actions=num_actions)
            for idx in range(num_players)
    ]

    random_agents = [
            random_agent.RandomAgent(player_id=idx, num_actions=num_actions)
            for idx in range(num_players)
    ]

    # 1) Train the agents
    training_episodes = FLAGS.num_episodes
    for cur_episodes in range(training_episodes):
        if cur_episodes % int(1e4) == 0:
            win_rates = eval_win_results(env, qlearner_agents, random_agents, 1000)
            logging.info("Starting episode %s, winrates %s", cur_episodes, win_rates)
        time_step = env.reset()
        while not time_step.last():
            player_id = time_step.observations["current_player"]
            agent_output = qlearner_agents[player_id].step(time_step)
            time_step = env.step([agent_output.action])

        # Episode over step all agents with final info state
        for agent in qlearner_agents:
            agent.step(time_step)

    if not FLAGS.interactive_play:
        return


if __name__ == "__main__":
    app.run(main)
