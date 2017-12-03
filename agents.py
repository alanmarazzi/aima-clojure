# Basic building blocks
import collections
import random
from utils import distance_squared

# Agents, objects and stuff 'living' in an environment
class Thing:
    """It's a thing"""

    def __repr__(self):
        return f'<{getattr(self, "__name__", self.__class__.__name__)}>'

    def is_alive(self):
        """If Thing is alive return True"""
        return hasattr(self, 'alive') and self.alive

    def show_state(self):
        """Show the internal state. To be implemented by subclasses"""
        print("Can't show the state")

    def display(self, canvas, x, y, width, height):
        """Display Thing on canvas"""
        pass

class Agent(Thing):
    """I'm an Agent, I don't do much unless you tell me so!
    I require a 'program' that takes a 'percept' and returns
    an 'action'. I can also measure 'performance'!"""

    def __init__(self, program=None):
        self.alive = True
        self.bump = False
        self.holding = []
        self.performance = 0

        if program is None or not isinstance(program, collections.Callable):
            print(f"Can't find a program for {self.__class__.__name__}."
                  + "Falling back to default.")

            def program(percept):
                return eval(input(f'Percept={percept}; action? '))

        self.program = program

    def can_grab(self, thing):
        """Can I take it?"""
        return False

# Environment, moving and other 'environmental' stuff
class Environment:
    """A general idea of an environment. To get real, subclass
    this and implement 'percept' and 'execute_action'."""

    def __init__(self):
        self.things = []
        self.agents = []

    def thing_classes(self):
        """Stuff that can go in Environment"""
        return []

    def percept(self, agent):
        """What the Agent perceives"""
        raise NotImplementedError

    def execute_action(self, agent, action):
        """How Environment changes after action"""
        raise NotImplementedError

    def default_location(self, thing):
        """Where do I place that Thing?"""
        return None

    def exogenous_change(self):
        """What can happen outside Agents actions?"""
        pass

    def is_done(self):
        """Default is if no Agent is living, exit"""
        return not any(agent.is_alive() for agent in self.agents)

    def step(self):
        """Time step, not space."""
        if not self.is_done():
            actions = []
            for agent in self.agents:
                if agent.alive:
                    actions.append(agent.program(self.percept(agent)))
                else:
                    actions.append('')
            for agent, action in zip(self.agents, actions):
                self.execute_action(agent, action)
            self.exogenous_change()

    def run(self, steps=1000):
        """Run Environment for given number of steps"""
        for step in range(steps):
            if self.is_done():
                return
            self.step()

    def list_things_at(self, location, tclass=Thing):
        """How many things there are here?"""
        return [thing for thing in self.things
                if thing.location == location and isinstance(thing, tclass)]

    def some_things_at(self, location, tclass=Thing):
        """Is there something here?"""
        return self.list_things_at(location, tclass) != []

    def add_thing(self, thing, location=None):
        """Let me put something at that location"""
        if not isinstance(thing, Thing):
            thing = Agent(thing)
        if thing in self.things:
            print("Can't add the same thing twice")
        else:
            thing.location = location if location is not None\
                             else self.default_location(thing)
            self.things.append(thing)
            if isinstance(thing, Agent):
                thing.performance = 0
                self.agents.append(thing)

    def delete_thing(self, thing):
        """Remove thing from the environment"""
        try:
            self.things.remove(thing)
        except ValueError as e:
            print(e)
            print(" in Environment delete_thing")
            print(f" Thing to be removed: {thing} at {thing.location}")
            print(" from list:"
                  + f"{[(thing, thing.location) for thing in self.things]}")
        if thing in self.agents:
            self.agents.remove(thing)

class Direction:
    """Jeez, moving is hard!"""

    R = 'right'
    L = 'left'
    U = 'up'
    D = 'down'

    def __init__(self, direction):
        self.direction = direction

    def __add__(self, heading):
        """Where do I face if turning 90°?"""
        if self.direction == self.R:
            return {
                self.R: Direction(self.D),
                self.L: Direction(self.U),
            }.get(heading, None)
        elif self.direction == self.L:
            return {
                self.R: Direction(self.U),
                self.L: Direction(self.D),
            }.get(heading, None)
        elif self.direction == self.U:
            return {
                self.R: Direction(self.R),
                self.L: Direction(self.L),
            }.get(heading, None)
        elif self.direction == self.D:
            return {
                self.R: Direction(self.L),
                self.L: Direction(self.R),
            }.get(heading, None)

    def move_forward(self, from_location):
        """Let's move on!"""
        x, y = from_location
        if self.direction == self.R:
            return (x + 1, y)
        elif self.direction == self.L:
            return (x - 1, y)
        elif self.direction == self.U:
            return (x, y - 1)
        elif self.direction == self.D:
            return (x, y + 1)

class XYEnvironment(Environment):
    """2D Environments rock!"""

    def __init__(self, width=10, height=10):
        super().__init__()

        self.width = width
        self.height = height
        self.observers = []
        self.x_start, self.y_start = 0, 0
        self.x_end, self.y_end = self.width, self.height

    def things_near(self, location, radius=1):
        """Lemme see if there's something around here"""
        radius2 = radius * radius
        return [(thing, radius2 - distance_squared(location, thing.location))
                for thing in self.things if distance_squared(
                    location,
                    thing.location
                ) <= radius2]

    def percept(self, agent):
        """Let the Agent actually perceive something"""
        return self.things_near(agent.location)

    def execute_action(self, agent, action):
        """Do something!"""
        agent.bump = False
        if action == 'TurnRight':
            agent.direction += Direction.R
        elif action == 'TurnLeft':
            agent.direction += Direction.L
        elif action == 'Forward':
            agent.bump = self.move_to(
                agent,
                agent.direction.move_forward(
                    agent.location
                ))
        elif action == 'Release':
            if agent.holding:
                agent.holding.pop()

    def default_location(self, thing):
        return random.choice(self.width), random.choice(self.height)

    def move_to(self, thing, destination):
        """Move something to another location"""
        thing.bump = self.some_things_at(destination, Obstacle)
        if not thing.bump:
            thing.location = destination
            for o in self.observers:
                o.thing_moved(thing)
            for t in thing.holding:
                self.delete_thing(t)
                self.add_thing(t, destination)
                t.location = destination
        return thing.bump

    def add_thing(self, thing, location=(1, 1), exclude_duplicates=False):
        """Put stuff in the environment!"""
        if self.is_inbounds(location):
            if (exclude_duplicates
               and any(isinstance(t, thing.__class__)
                       for t in self.list_things_at(location))):
                return
            super().add_thing(thing, location)

    def is_inbounds(self, location):
        """Let's not end up in the Upsidedown"""
        x, y = location
        return not (x < self.x_start or x >= self.x_end
                    or y < self.y_start or y >= self.y_end)

    def random_location_inbounds(self, exclude=None):
        location = (random.randint(self.x_start, self.x_end),
                    random.randint(self.y_start, self.y_end))
        if exclude is not None:
            while location == exclude:
                location = (random.randint(self.x_start, self.x_end),
                            random.randint(self.y_start, self.y_end))
        return location

    def delete_thing(self, thing):
        if isinstance(thing, Agent):
            for obj in thing.holding:
                super().delete_thing(obj)
                for o in self.observers:
                    o.thing_deleted(obj)

        super().delete_thing(thing)
        for o in self.observers:
            o.thing_deleted(thing)

    def add_walls(self):
        for x in range(self.width):
            self.add_thing(Wall(), (x, 0))
            self.add_thing(Wall(), (x, self.height - 1))
        for y in range(self.height):
            self.add_thing(Wall(), (0, y))
            self.add_thing(Wall(), (self.width - 1, y))

        self.x_start, self.y_start = 1, 1
        self.x_end, self.y_end = self.width - 1, self.height - 1

    def add_observer(self, observer):
        self.observers.append(observer)

    def turn_heading(self, heading, inc):
        return turn_heading(heading, inc)

class Obstacle(Thing):
    pass

class Wall(Obstacle):
    pass
