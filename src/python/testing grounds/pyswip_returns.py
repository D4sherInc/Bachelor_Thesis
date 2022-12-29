from pyswip import Prolog
import pyswip

prolog = Prolog()


def _translate_pyswip_atoms_to_python(board):
    """translate pyswip representation of values
    if pyswip atom: replace by python value"""
    # if list of lists:
    if isinstance(board[0], list):
        for index, subl in enumerate(board):
            board[index] = _translate_pyswip_atoms_to_python(subl)
        return board

    # else: just translate each element
    for index, el in enumerate(board):
        board[index] = el.value if isinstance(el, pyswip.Atom) else el
    return board


def init_returns(game):
    """call init/3
    returns: generator gen
    get values with: list(gen)[0]["Identifier"] (e.g. 'InitState', 'Current_Player', 'Player0_score')"""
    prolog.consult(game)
    gen = prolog.query("init(InitState, Current_Player, Player0_Score)")
    return gen


def is_terminal_returns(game, game_state):
    """call is_terminal/1 on game state
    returns: generator gen
    get bool with bool(gen) = True / False"""
    prolog.consult(game)
    gen = prolog.query("is_terminal(%s)" % game_state)
    return gen


def legal_actions_returns(game, *moves):
    prolog.consult(game)
    state = list(init_returns(game))[0]["InitState"]
    state = _translate_pyswip_atoms_to_python(state)
    for move in moves:
        gen = list(prolog.query("apply_action(%s, %s, NewGameState)" % (state, move)))
        state = gen[0]["NewGameState"]
        state = _translate_pyswip_atoms_to_python(state)

    r = prolog.query("legal_actions(Legal_actions)")
    return r


def apply_action_returns(game, *moves):
    """apply move(s) to board state
    on success: returns new game state
    on failure: returns False"""
    gen = init_returns(game)
    l = list(gen)
    state = l[0]["InitState"]
    state = _translate_pyswip_atoms_to_python(state)
    for move in moves:
        # current_player = list(prolog.query("current_player(A)"))[0]["A"]
        gen = list(prolog.query("apply_action(%s, %s, NewGameState)" % (state, move)))

        # if move was not successful: return
        if not bool(gen):
            return False

        state = gen[0]["NewGameState"]
        state = _translate_pyswip_atoms_to_python(state)
    return gen


def what_prolog_returns(game):
    empty_board = [0, 0, 0, 0, 0, 0, 0, 0, 0]
    prolog.consult(game)
    r = list(prolog.query("init(InitState, Current_Player, Player0_Score)"))
    print("Initial game state:\n", r)

    state = r[0]["InitState"]
    r2 = prolog.query("is_terminal(%s)" % state)
    print("is_terminal on initState:\n %s = %s \n" % (list(r2), bool(r2)))

    r4 = prolog.query("is_terminal(%s)" % empty_board)
    print("is_terminal on empty board:\n %s = %s\n" % (list(r4), bool(r4)))

    r5 = prolog.query("is_terminal(%s)" % state)
    print("is_terminal without the listing on the query:\n %s\n" % r5)

    r6 = prolog.query("move(%s, x, 5, NewBoard)" % empty_board)
    r6l = list(r6)
    for index, el in enumerate(r6l[0]["NewBoard"]):
        print(el.value if isinstance(el, pyswip.Atom) else el, end='\n' if (index + 1) % 3 == 0 else '')
