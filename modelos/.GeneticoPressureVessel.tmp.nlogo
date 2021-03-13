turtles-own [
  ; Gens of each solution
  x1  ; Radius R
  x2  ; Length L
  x3  ; Shell thickness Ts
  x4  ; Dish-end thickness Th

  f-order ; Attribute to sort population
  fitness ; Objective function  (Pressure Vessel)
]

globals [
  evaluations
  winner
]

to setup
  clear-all

  ; http://ccl.northwestern.edu/netlogo/docs/dictionary.html#import-pcolors
  ;import-drawing "PV.jpg"
  import-pcolors "PV.jpg"

  set evaluations 0
  create-turtles population-size [

    ; Set random value in the respective domain of each gen
    set x1 min-x1 + random-float (max-x1 - min-x1 + 1)
    set x2 min-x2 + random-float (max-x2 - min-x2 + 1)
    set x3 min-x3 + random-float (max-x3 - min-x3 + 1)
    set x4 min-x4 + random-float (max-x4 - min-x4 + 1)

    ;set x1 41.0906
    ;set x2 189.8874
    ;set x3 0.7959
    ;set x4 0.3924

    ;set x1 38.860102
    ;set x2 221.365487
    ;set x3 0.7500
    ;set x4 0.3750

    ;show (word "Establecemos los valores " x1 " "x2 " " x3 " " x4)

    calculate-fitness
    hide-turtle
  ]
  set winner min-one-of turtles [f-order]
  update-display
  reset-ticks
end



to go
  if evaluations >= num-evaluations
    [ stop ]
  next-generation
  set winner min-one-of turtles [f-order]
  ask winner [calculate-fitness]
  update-display
  tick
end


to update-display
  ; Radius
  ask patch 207 142 [set plabel (word (precision ([x1] of winner) 3))
                     set plabel-color 74]
  ; Length
  ask patch 308 210 [set plabel (precision ([x2] of winner) 3)
                     set plabel-color 74]

  ;Shell thickness
  ask patch 150 226 [set plabel (precision ([x3] of winner) 3)
                     set plabel-color 74]
  ;Dish-end thickness
  ask patch 435 235 [set plabel (precision ([x4] of winner) 3)
                     set plabel-color 74]
end

;; Objective function
to calculate-fitness  ; turtle procedure

  ;; Constraints
  let g1 (hoop-stress * x1 - x3)
  ;show (word "G1 " hoop-stress " * " x1 " - " x3 " = " g1)

  let g2 (longitudinal-stress * x1 - x4)
  ;show (word "G2 " longitudinal-stress " * " x1 " - " x4 " = " g2)

  let g3 (v1 * v2 - ((4.0 / 3.0) * pi * (x1 * x1 * x1)) - (pi * x1 * x1 * x2))
  ;show word "G3 " g3

  let g4 (x2 - 240)
  ;show word "G4 " g4

  ; Here our main objective is to reduce the total manufacturing cost
  ; reducing weight and material cost of Pressure Vessel
  set fitness ((0.6224 * x1 * x2 * x3) +
               (1.7781 * x1 * x1 * x4) +
               (3.1661 * x2 * x3 * x3) +
               (19.84 * x3 * x3 * x1))

  let constraints-penalty (max (list 0 g1)) ^ 2 + (max (list 0 g2)) ^ 2 + (max (list 0 g3)) ^ 2 + (max (list 0 g4)) ^ 2
  set f-order fitness + 1000000 * constraints-penalty

  ;show "CALCULAMOS EL FITNESS"
  ;show (word "fitness " fitness " y f-order " f-order)

  ; We increment the number of evaluations
  set evaluations evaluations + 1
end


to next-generation

  let old-generation turtles with [true]
  ; Some number of the population is created by crossover each generation
  ; we divide by 2 because each time through the loop we create two children.
  let crossover-count (floor (population-size * crossover-rate / 100 / 2))

  repeat crossover-count
  [
    let parents []
    set parents selection old-generation
    ; create the two children, with their new genetic material
    crossover (item 0 parents) (item 1 parents)
  ]

  ; the remainder of the population is created by cloning
  ; selected members of the previous generation
  repeat (population-size - crossover-count * 2)
  [
    ask min-one-of (n-of 3 old-generation) [f-order]
      [ hatch 1 ]
  ]

  ask old-generation [ die ]

  ; there's a chance of mutations occurring
  if mutation? [mutate]
end

;; Selection operator
to-report selection [population]

  ; We use "tournament selection", with tournament size = 3 by default
  ; This means, we randomly pick 3 solutions from the previous generation
  ; and select the best one of those 3 to reproduce.
  let parent1 min-one-of (n-of 3 population) [f-order]
  let parent2 min-one-of (n-of 3 population) [f-order]
  report list parent1 parent2
end

;; Crossover operator
to crossover [father mother]

  let beta random-float 1

  ; Creates the two new solutions
  create-turtles 1 [
    set x1 ((beta * ([x1] of father)) + (1 - beta) * ([x1] of mother))
    set x2 ((beta * ([x2] of father)) + (1 - beta) * ([x2] of mother))
    set x3 ((beta * ([x3] of father)) + (1 - beta) * ([x3] of mother))
    set x4 ((beta * ([x4] of father)) + (1 - beta) * ([x4] of mother))
    calculate-fitness
    hide-turtle
  ]

  create-turtles 1 [
    set x1 (((1 - beta) * ([x1] of father)) + beta * ([x1] of mother))
    set x2 (((1 - beta) * ([x2] of father)) + beta * ([x2] of mother))
    set x3 (((1 - beta) * ([x3] of father)) + beta * ([x3] of mother))
    set x4 (((1 - beta) * ([x4] of father)) + beta * ([x4] of mother))
    calculate-fitness
    hide-turtle
  ]
end

;; Mutation operator
to mutate  ;; turtle procedure

  ; We randomly select mutation-count of gens of all population and
  ; change their value by a random number lying between the move limits
  ; corresponding to these variables

  let mutation-gens-count (floor (population-size * 4 * mutation-rate))

  repeat mutation-gens-count [

    let random-turtle one-of turtles
    let random-gen 1 + random 4

    ask random-turtle [

      if random-gen = 1
        [set x1 min-x1 + random-float (max-x1 - min-x1 + 1)]
      if random-gen = 2
        [set x2 min-x2 + random-float (max-x2 - min-x2 + 1)]
      if random-gen = 3
        [set x3 min-x3 + random-float (max-x3 - min-x3 + 1)]
      if random-gen = 4
        [set x4 min-x4 + random-float (max-x4 - min-x4 + 1)]

      calculate-fitness
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
379
124
1003
475
-1
-1
1.366
1
18
1
1
1
0
1
1
1
0
450
0
250
1
1
1
ticks
30.0

BUTTON
193
140
278
173
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
105
100
278
133
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
86
255
298
288
population-size
population-size
5
200
25.0
5
1
NIL
HORIZONTAL

PLOT
740
524
1183
883
Fitness Plot
gen #
raw fitness
0.0
20.0
0.0
101.0
true
true
"" ""
PENS
"best" 1.0 0 -2674135 true "" "plot min [fitness] of turtles"
"avg" 1.0 0 -10899396 true "" "plot mean [fitness] of turtles"
"worst" 1.0 0 -13345367 true "" "plot max [fitness] of turtles"

BUTTON
105
140
190
173
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
279
40
429
58
Mejor cromosoma
12
0.0
1

MONITOR
413
27
526
76
Fitness
[fitness] of winner
2
1
12

MONITOR
559
29
635
74
Radius
[x1] of winner
4
1
11

MONITOR
659
30
733
75
Length
[x2] of winner
4
1
11

MONITOR
752
30
855
75
Shell Thickness
[x3] of winner
4
1
11

MONITOR
871
31
994
76
Dish-end Thickness
[x4] of winner
4
1
11

TEXTBOX
430
505
616
539
Constraints Parameters
14
0.0
1

SLIDER
482
664
654
697
hoop-stress
hoop-stress
0
1
0.0193
0.01
1
NIL
HORIZONTAL

TEXTBOX
385
547
611
565
g1(x) = hoop-stresss x1 -  x4 <= 0
12
0.0
1

TEXTBOX
588
82
603
100
x1
12
0.0
1

TEXTBOX
687
83
702
101
x2
12
0.0
1

TEXTBOX
797
82
812
100
x3
12
0.0
1

TEXTBOX
923
86
938
104
x4
12
0.0
1

TEXTBOX
384
569
633
588
g2(x) = longitudinal-stress x1 - x3 <= 0
12
0.0
1

TEXTBOX
383
590
683
620
g3(x) = v1 x v2 - (4/3)pix1^3 - pix1^2 x2 <= 0
12
0.0
1

SLIDER
482
718
689
751
longitudinal-stress
longitudinal-stress
0
1
0.00954
0.001
1
NIL
HORIZONTAL

SLIDER
485
773
585
806
v1
v1
0
1000
750.0
10
1
NIL
HORIZONTAL

SLIDER
592
773
694
806
v2
v2
0
10000
1728.0
10
1
NIL
HORIZONTAL

MONITOR
385
658
471
703
g1
(hoop-stress * ([x1] of winner)) - ([x3] of winner)
3
1
11

MONITOR
383
714
470
759
g2
(longitudinal-stress * ([x1] of winner)) - ([x4] of winner)
3
1
11

MONITOR
383
771
471
816
g3
(v1 * v2 - ((4.0 / 3.0) * pi * (([x1] of winner) * ([x1] of winner) * ([x1] of winner))) - (pi * ([x1] of winner) * ([x1] of winner) * ([x2] of winner)))
3
1
11

TEXTBOX
124
447
274
465
Variable bounds
14
0.0
1

SLIDER
74
477
173
510
min-x1
min-x1
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
183
477
286
510
max-x1
max-x1
0
500
200.0
1
1
NIL
HORIZONTAL

SLIDER
74
529
173
562
min-x2
min-x2
0
220
10.0
1
1
NIL
HORIZONTAL

SLIDER
184
528
287
561
max-x2
max-x2
0
500
240.0
1
1
NIL
HORIZONTAL

SLIDER
63
580
173
613
min-x3
min-x3
0
5
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
185
580
289
613
max-x3
max-x3
0
100
6.2
0.1
1
NIL
HORIZONTAL

SLIDER
63
628
173
661
min-x4
min-x4
0
100
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
186
628
290
661
max-x4
max-x4
0
100
6.2
0.1
1
NIL
HORIZONTAL

SLIDER
87
205
296
238
num-evaluations
num-evaluations
0
1000000
1000000.0
1
1
NIL
HORIZONTAL

MONITOR
1060
468
1160
513
Nº Evaluations
evaluations
0
1
11

SLIDER
85
312
300
345
crossover-rate
crossover-rate
0
100
80.0
1
1
NIL
HORIZONTAL

SWITCH
214
369
324
402
mutation?
mutation?
0
1
-1000

SLIDER
62
368
208
401
mutation-rate
mutation-rate
0
1
0.125
0.005
1
NIL
HORIZONTAL

TEXTBOX
383
611
533
629
g4(x) = x2 - 240 <= 0
12
0.0
1

MONITOR
382
826
471
871
g4
([x2] of winner) - 240
3
1
11

TEXTBOX
427
85
577
103
f (x1,x2,x3,x4)
12
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model demonstrates the use of a genetic algorithm on a very simple problem.  Genetic algorithms (GAs) are a biologically-inspired computer science technique that combine notions from Mendelian genetics and Darwinian evolution to search for good solutions to problems (including difficult problems).  The GA works by generating a random population of solutions to a problem, evaluating those solutions and then using cloning, recombination and mutation to create new solutions to the problem.

In this model we use the simple "ALL-ONES" problem to demonstrate how this is possible. We use such a simple problem in this model in order to highlight the solution technique only. The idea of the "ALL-ONES" problem is to find a string of bits (that is, a sequence of just ones and zeros) that contains all ones, and no zeros.  Thus the string that best solves this problem is "111111...111".

## HOW IT WORKS

The genetic algorithm is composed of the following steps.

1) A population of random solutions is created.  Each solution consists of a string of randomly mixed "1"s and "0"s.

2) Each solution is evaluated on the basis of how well it solves the problem.  This measure of the "goodness" of the solution is called its "fitness".  In this model, our goal is simply to find a solution that consists of all "1"s.  (In real-world applications of the genetic algorithm, the goals are much more complex, but the solutions are still usually encoded as binary strings.)

3) There are two variants of the models to create the new generation of solutions:

- **Generational model**: A new generation of solutions is created from the old generation, where solutions that have a higher fitness scores are more likely to be chosen as "parent" solutions than those that have low fitness scores.

- **Estacionary model**: 

A) There are two selection methods in this model:

- **Tournament selection**: with a tournament size of 3 by default.
	  This means that 3 solutions are drawn randomly from the old generation, and the 	  one with the highest fitness is chosen to become a parent.

- **Roulette selection**: also known as roulette wheel selection. The fitness 
	  level is used to associate a probability of selection with each individual 
	  chromosome

B) Either one or two parents are chosen to create children.  With one parent, the child is a clone or copy of the parent.  With two parents, the process is the digital analog of sexual recombination -- the two children inherit part of their genetic material from one parent and part from the other.

C) There is also a chance that mutation will occur, and some of the child's bits will be changed from "1"s to "0"s, or vice versa.

4) Steps 2 and 3 above are repeated until a solution is found that successfully solves the problem.

## HOW TO USE IT

Press the SETUP button to create an initial random population of solutions.

Press the STEP button to have one new generation created from the old generation.

Press the GO button to have the genetic algorithm run until a solution has been found.

The best solution found in each generation is displayed in the VIEW.  Each white column represents a "1"-bit and each black column represents a "0"-bit.

=== Parameters ===

The POPULATION-SIZE slider controls the number of solutions that are present in each generation.

The CROSSOVER-RATE slider controls what percent of each new generation is created through sexual reproduction (recombination or crossover between two parents' genetic material), and what percent (100 - CROSSOVER-RATE) is created through asexual reproduction (cloning of one parent's genetic material).

The MUTATION-RATE slider controls the percent chance of mutation.  This chance applies to each position in the string of bits of a new individual.  For instance, if the string is 100 bits long, and the mutation-rate is set at 1%, then on average one bit will be changed during the creation of each new individual.

The PLOT-DIVERSITY? switch controls whether the amount of diversity within the population of solutions is plotted each generation, shown in the "Diversity Plot".  Turning off PLOT-DIVERSITY? significantly increases the speed of the model because calculating diversity requires a lot of computation.

The "Fitness Plot" is used to show the best, average, and worst fitness values of the solutions at each generation.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
need-to-manually-make-preview-for-this-model
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="hoop-stress">
      <value value="0.0193"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="longitudinal-stress">
      <value value="0.00954"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-x1">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-x2">
      <value value="240"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover-rate">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-x3">
      <value value="1.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-x1">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-x4">
      <value value="1.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-x2">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-ponderation">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-x3">
      <value value="0.0625"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-evaluations">
      <value value="100000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v1">
      <value value="750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v2">
      <value value="1728"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-size">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-x4">
      <value value="0.0625"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
