let
  cross = xxs: yys:
    if builtins.length xxs == 0 || builtins.length yys == 0 then
      []
    else
      let
        x = builtins.head xxs;
        xs = builtins.tail xxs;
        ys = builtins.tail yys;
      in
        builtins.map (y: {a = x; b = y;}) yys ++ cross xs ys;

  numbers = builtins.map builtins.fromJSON (builtins.filter (x: x != "" && x != []) (builtins.split "\n" (builtins.readFile ./input)));
in builtins.map ({a, b}: a * b) (builtins.filter ({a, b}: a+b == 2020) (cross numbers numbers))
