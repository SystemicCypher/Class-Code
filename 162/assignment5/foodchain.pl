plant(grass).
plant(berry).
plant(clover).

animal(cow).
animal(deer).
animal(bear).
animal(tiger).
animal(spider).

animal(fred).

eats(cow, grass).
eats(cow, clover).

eats(deer, grass).

eats(bear, deer).
eats(bear, berry).

eats(tiger, cow).
eats(tiger, deer).

eats(fred, rocks).

herbivore(X) :-
  eats(X, Y),
  plant(Y),
  \+ (
    eats(X, Z),
    animal(Z)
  ).

carnivore(Z) :-
  eats(Z, B),
  animal(B),
  \+ omnivore(Z).

omnivore(D) :-
  eats(D, E),
  animal(E),
  eats(D, F),
  plant(F).
