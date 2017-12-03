from utils import (removeall, unique, first, argmax, probability,
                   isnumber, issequence, Expr, expr, subexpressions)
import agents
import itertools
import random
from collections import defaultdict


class KB:

    """A knowledge base, ask and tell sentences"""

    def __init__(self, sentence=None):
        raise NotImplementedError

    def tell(self, sentence):
        raise NotImplementedError

    def ask(self, query):
        return first(self.ask_generator(query), default=False)

    def ask_generator(self, query):
        raise NotImplementedError

    def retract(self, sentence):
        raise NotImplementedError


class PropKB(KB):

    def __init__(self, sentence=None):
        self.clauses = []
        if sentence:
            self.tell(sentence)

    def tell(self, sentence):
        self.clauses.extend(conjuncts(to_cnf(sentence)))

    def ask_generator(self, query):
        if tt_entails(Expr('&', *self.clauses), query):
            yield {}

    def ask_if_true(self, query):
        for _ in self.ask_generator(query):
            return True
        return False

    def retract(self, sentence):
        for c in conjuncts(to_cnf(sentence)):
            if c in self.clauses:
                self.clauses.remove(c)


def KB_AgentProgram(KB):
    steps = itertools.count()

    def program(percept):
        t = next(steps)
        KB.tell(make_percept_sentence(percept, t))
        action = KB.ask(make_action_query(t))
        KB.tell(make_action_sentence(action, t))
        return action

    def make_percept_sentence(percept, t):
        return Expr("Percept")(percept, t)

    def make_action_query(t):
        return expr(f"ShouldDo(action, {t})")

    def make_action_sentence(action, t):
        return Expr("Did")(action[expr('action')], t)

    return program

def is_symbol(s):
    return isinstance(s, str) and s[:1].isalpha()
