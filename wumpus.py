# Wumpus World Implementation
from blocks import Thing, Agent, Direction, XYEnvironment, Wall
import random

class Gold(Thing):
    """A very nice pot of G-O-L-D"""

    def __eq__(self, rhs):
        """All Gold is G-O-L-D"""
        return rhs.__class__ == Gold

    pass

class Bump(Thing):
    """D'Oh!"""
    pass

class Glitter(Thing):
    """Bling bling!"""
    pass

class Pit(Thing):
    """A bottomless pit"""
    pass

class Breeze(Thing):
    """Mmmmh... A breeze, there must be a Pit very close"""
    pass

class Arrow(Thing):
    """An arrow. Funny, there's no bow..."""
    pass

class Scream(Thing):
    """YAAAARGGGHHHLAGGBARGHLAAAAGBBBB"""
    pass

class Stench(Thing):
    """Someone ate beans..."""
    pass

class Wumpus(Agent):
    """WHAT THE HECK IS THAT!"""
    screamed = False
    pass

class Explorer(Agent):
    holding = []
    has_arrow = True
    killed_by = ""
    direction = Direction("right")

    def can_grab(self, thing):
        """I want the bling bling!"""
        return thing.__class__ == Gold

class WumpusEnvironment(XYEnvironment):
    """Welcome to the Wumpus cave!!!"""

    pit_probability = 0.2

    def __init__(self, agent_program, width=6, height=6):
        super().__init__(width, height)
        self.init_world(agent_program)

    def init_world(self, program):
        """Randomly place all elements in the environment"""
        # Add walls
        self.add_walls()

        # Add pits
        for x in range(self.x_start, self.x_end):
            for y in range(self.y_start, self.y_end):
                if random.random() < self.pit_probability:
                    self.add_thing(Pit(), (x, y), True)
                    self.add_thing(Breeze(), (x - 1, y), True)
                    self.add_thing(Breeze(), (x, y - 1), True)
                    self.add_thing(Breeze(), (x + 1, y), True)
                    self.add_thing(Breeze(), (x, y + 1), True)

        # Wumpus
        w_x, w_y = self.random_location_inbounds(exclude=(1, 1))
        self.add_thing(Wumpus(lambda x: ""), (w_x, w_y), True)
        self.add_thing(Stench(), (w_x - 1, w_y), True)
        self.add_thing(Stench(), (w_x + 1, w_y), True)
        self.add_thing(Stench(), (w_x, w_y - 1), True)
        self.add_thing(Stench(), (w_x, w_y + 1), True)

        # Gold
        self.add_thing(
            Gold(),
            self.random_location_inbounds(exclude=(1, 1)),
            True
        )

        # Agent
        self.add_thing(Explorer(program), (1, 1), True)

    def get_world(self, show_walls=True):
        result = []
        x_start, y_start = (0, 0) if show_walls else (1, 1)

        if show_walls:
            x_end, y_end = self.width, self.height
        else:
            x_end, y_end = self.width - 1, self.height - 1

        for x in range(x_start, x_end):
            row = []
            for y in range(y_start, y_end):
                row.append(self.list_things_at((x, y)))
            result.append(row)
        return result

    def percepts_from(self, agent, location, tclass=Thing):
        """What there is here?!"""
        thing_percepts = {
            Gold: Glitter(),
            Wall: Bump(),
            Wumpus: Stench(),
            Pit: Breeze()
        }

        # Agents don't perceive themselves
        thing_percepts[agent.__class__] = None

        # Gold glitters only in its spot
        if location != agent.location:
            thing_percepts[Gold] = None

        result = [thing_percepts.get(thing.__class__, thing)
                  for thing in self.things if thing.location == location
                  and isinstance(thing, tclass)]
        return result if len(result) else [None]

    def percept(self, agent):
        """What there is in closest squares?"""
        x, y = agent.location
        result = []
        result.append(self.percepts_from(agent, (x - 1, y)))
        result.append(self.percepts_from(agent, (x + 1, y)))
        result.append(self.percepts_from(agent, (x, y - 1)))
        result.append(self.percepts_from(agent, (x, y + 1)))
        result.append(self.percepts_from(agent, (x, y)))

        wumpus = [thing for thing in self.things if isinstance(thing, Wumpus)]
        if len(wumpus) and not wumpus[0].alive and not wumpus[0].screamed:
            result[-1].append(Scream())
            wumpus[0].screamed = True

        return result

    def execute_action(self, agent, action):
        """Let's do some work!"""
        if isinstance(agent, Explorer) and self.in_danger(agent):
            return

        agent.bump = False
        if action == 'TurnRight':
            agent.direction += Direction.R
            agent.performance -= 1
        elif action == 'TurnLeft':
            agent.direction += Direction.L
            agent.performance -= 1
        elif action == 'Forward':
            agent.bump = self.move_to(
                agent,
                agent.direction.move_forward(agent.location)
            )
            agent.performance -= 1
        elif action == 'Grab':
            things = [thing for thing in self.list_things_at(agent.location)
                      if agent.can_grab(thing)]
            if len(things):
                print('Grabbing', things[0].__class__.__name__)
                if len(things):
                    agent.holding.append(things[0])
            agent.performance -= 1
        elif action == 'Climb':
            if agent.location == (1, 1):
                agent.performance += 1000 if Gold() in agent.holding else 0
                self.delete_thing(agent)
        elif action == 'Shoot':
            if agent.has_arrow:
                arrow_travel = agent.direction.move_forward(agent.location)
                while(self.is_inbounds(arrow_travel)):
                    wumpus = [thing for thing in
                              self.list_things_at(arrow_travel)
                              if isinstance(thing, Wumpus)]
                    if len(wumpus):
                        wumpus[0].alive = false
                        break
                    arrow_travel = agent.direction.move_forward(agent.location)
                agent.has_arrow = False

    def in_danger(self, agent):
        """Knock, knock, is anyone here? I'm the death..."""
        explorer = [agent for agent in self.agents
                    if isinstance(agent, Explorer)]
        if len(explorer):
            if explorer[0].alive:
                return False
            else:
                print(f"Death by  {explorer[0].killed_by} [-1000]")
        else:
            print("Explorer climbed out {}"
                  .format("with Gold [+1000]" if Gold() not in self.things
                  else "without Gold [+0]"))
        return True
