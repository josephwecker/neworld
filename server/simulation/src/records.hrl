-record(orc, {
    id,
    who,
    x,
    y,
    living = true,
    hit_points = 10,
    strength = 10,
    food = 3,
    mind
  }).

-record(target, {
    id,
    who,
    x,
    y,
    distance,
    living = true
  }).

-record(sinn, {
    body,
    memory = []
  }).
