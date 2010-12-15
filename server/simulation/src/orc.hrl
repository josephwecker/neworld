-record(orc, {
    id,
    who,
    x,
    y,
    living = true,
    target = none,
    hit_points = 10,
    strength = 10,
    food = 3
  }).

-record(target, {
    id,
    who,
    x,
    y,
    living = true
  }).
