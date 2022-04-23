-module(foodchain).
-compile({parse_transform, liet_state_graph}).

sunlight() -> [shine].

grass() -> [grow|sunlight()].

cow() -> [moo|grass()].

rabbit() -> [hop|grass()].

dog() -> [bark|rabbit()].

human() -> [think|lists:usort(rabbit() ++ cow())].
