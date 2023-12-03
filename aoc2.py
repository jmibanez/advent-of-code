from dataclasses import dataclass
from functools import reduce
from typing import List

@dataclass
class GameSet:
    r: int
    g: int
    b: int

@dataclass
class Game:
    i: int
    game_sets: List[GameSet]

    def can_fit(self, r: int, g: int, b: int) -> bool:
        for game_set in self.game_sets:
            if game_set.r > r or game_set.g > g or game_set.b > b:
                return False

        return True

    def fewest_cubes(self):
        r = 0
        g = 0
        b = 0
        for game_set in self.game_sets:
            r = max(game_set.r, r)
            g = max(game_set.g, g)
            b = max(game_set.b, b)
        return (r, g, b)

    def power(self):
        return reduce(lambda x, y: x * y, self.fewest_cubes(), 1)

def read_game(line: str) -> Game:
    id_line, game_sets_line = line.split(":")
    game_set_defs = game_sets_line.split(";")
    game_id = int(id_line.split(" ")[1])

    game_sets = []
    for set_def in game_set_defs:
        tuple_defs = set_def.split(",")
        r = 0
        g = 0
        b = 0
        for tup in tuple_defs:
            tuptup = tup.strip().split(" ")
            if tuptup[1] == "red":
                r = int(tuptup[0])
            if tuptup[1] == "green":
                g = int(tuptup[0])
            if tuptup[1] == "blue":
                b = int(tuptup[0])
        
        game_sets.append(GameSet(r=r, g=g, b=b))

    return Game(i=game_id, game_sets=game_sets)

with open("2.txt") as f:
    input = f.read()
    sum = 0
    for line in input.split("\n"):
        if len(line) == 0:
            continue
        g = read_game(line)
        # if g.can_fit(r=12, g=13, b=14):
        #     sum += g.i
        sum += g.power()

    print(sum)
