import sys
import json 
import logging


# Helper functions
def debug(level, message):
    logging.debug( level * "\t" + message)


def gps(initial_states, goal_states, ops):
    prefix = "Executing " 

    for op in ops:
        op["add"].append(prefix + op["action"])

    final_states = achieve_all(initial_states, ops, goal_states, [])
    if not final_states:
        return None

    return [state for state in final_states if state.startswith(prefix)]


def achieve_all(states, ops, goals, goal_stack):
    for goal in goals:
        states = achieve(states, ops, goal, goal_stack)
        if not states:
            return None

    for goal in goals:
        if goal not in states:
            return None

    return states


def achieve(states, ops, goal, goal_stack):
    debug(len(goal_stack), "Achieving: %s" % goal)

    if goal in states:
        return states

    if goal in goal_stack:
        return None

    for op in ops:
        if goal not in op["add"]:
            continue
        result = apply_op(op, states, ops, goal, goal_stack)
        if result:
            return result


def apply_op(op, states, ops, goal, goal_stack):
    debug(len(goal_stack), "Consider: %s" % op["action"])

    result = achieve_all(states, ops, op["preconds"], [goal] + goal_stack)
    if not result:
        return None

    debug(len(goal_stack), "Action: %s" % op["action"])

    add_list = op["add"]
    del_list = op["delete"]
    return [state for state in result if state not in del_list] + add_list 


def main():
    with open(sys.argv[1], 'r') as f:
        problem = json.load(f)
    start, finish, ops = problem.values()
    deb = input("> Do you want to output a stack trace?[y/n] ")
    if deb == 'y' or deb == 'Y':
        logging.basicConfig(level=logging.DEBUG)
    for action in gps(start, finish, ops):
        print(action)


if __name__ == '__main__':
    main()
