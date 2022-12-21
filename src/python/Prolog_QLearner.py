"""Playthrough of a zero-sum sequential move 2 player Prolog game
games only with Game Logic, not with saving states
QLearner agent vs  random agent from OpenSpiel
training for 50000 Episodes, evaluation every 10000 episodes
supported games so far: tic_tac_toe and nim"""
from time import sleep

import numpy as np

from Prolog_Game import PrologGame
from open_spiel.python import rl_environment
from open_spiel.python.algorithms import random_agent
from open_spiel.python.algorithms import tabular_qlearner
from open_spiel.python.algorithms import boltzmann_tabular_qlearner

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
        "interactive_play", True,
        "Whether to run an interactive play with the agent after training.")


def eval_against_other_agents(env, agents, random_agents, num_episodes):
    """Evaluates `trained_agents` against `random_agents` for `num_episodes`."""
    wins = np.zeros(2)
    for player_pos in range(2):
        if player_pos == 0:
            cur_agents = [agents[0], random_agents[1]]
        else:
            cur_agents = [random_agents[0], agents[1]]
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
    logging.basicConfig(filename="mylogging.log", filemode="a",
                        format='%(asctime)s,%(msecs)d %(name)s %(levelname)s %(message)s',
                        datefmt='%H:%M:%S',
                        level=logging.DEBUG)

    logging.info("please enter what game should be played. Supported Games:")
    sleep(0.5)
    for g in PrologGame.supported_games:
        print(g, end=" ")
    print("\n-->")

    while True:
        try:
            game_name = input()
            game = PrologGame(game_name)
            break

        except ValueError:
            logging.info("Input Game not found. Supported Games are:")
            sleep(0.5)
            for g in PrologGame.supported_games:
                print(g, end=" ")
            print()

    env = rl_environment.Environment(game)
    num_actions = env.action_spec()["num_actions"]
    n_players = env.num_players

    qlearner_agents = [
            tabular_qlearner.QLearner(player_id=idx, num_actions=num_actions)
            for idx in range(n_players)
    ]

    random_agents = [
            random_agent.RandomAgent(player_id=idx, num_actions=num_actions)
            for idx in range(n_players)
    ]

    boltzmann_tabular_qlearner_agents = [
            boltzmann_tabular_qlearner.BoltzmannQLearner(player_id=idx, num_actions=num_actions)
            for idx in range(n_players)
    ]

    training_episodes = FLAGS.num_episodes

    training_agents = [qlearner_agents, boltzmann_tabular_qlearner_agents]

    # train standard qlearner agents second, evaluate every 10000 episodes against randoms, then against itself
    # train boltzmann qlearner agents first, evaluate every 10000 episodes against randoms, then against itself

    for agents in training_agents:
        match type(agents[0]):
            case tabular_qlearner.QLearner:
                logging.info("selected agents: QLearner")
            case boltzmann_tabular_qlearner.BoltzmannQLearner:
                logging.info("selected agents: Boltzmann QLearner")
        logging.info("Starting agent training:")
        for cur_episode in range(training_episodes):
            if cur_episode % int(1e4) == 0:
                win_rates = eval_against_other_agents(env, agents, random_agents, 1000)
                logging.info("Starting episode %s, win_rates %s", cur_episode, win_rates)
            time_step = env.reset()
            while not time_step.last():
                player_id = time_step.observations["current_player"]
                agent_output = agents[player_id].step(time_step)
                time_step = env.step([agent_output.action])

            # episode over: step all agents with final info state
            for agent in boltzmann_tabular_qlearner_agents:
                agent.step(time_step)

        # final evaluation
        win_rates = eval_against_other_agents(env, agents, random_agents, 1000)
        logging.info("finished %s episodes. win_rates against random: %s", training_episodes, win_rates)
        win_rates = eval_against_other_agents(env, agents, qlearner_agents, 1000)
        logging.info("finished %s episodes. win_rates against itself: %s", training_episodes, win_rates)

        logging.info("game is finished\n")


if __name__ == "__main__":
    app.run(main)
