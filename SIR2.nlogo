; ==== SEIARHD with Day/Night Mixing (NetLogo Web, cleaned) ====
; States: S (susceptible), E (exposed), I (symptomatic infectious),
;         A (asymptomatic infectious), R (recovered), H (hospitalized), D (dead)
; Time unit: 1 tick = 1 hour (default 24 ticks/day)

breed [ people person ]

people-own [
  state               ; "S" "E" "I" "A" "R" "H" "D"
  hours-in-state      ; ticks spent in current state
  home-id             ; household id
  work-id             ; workplace/school id
  risk-level          ; 0 low risk, 1 high risk
  compliant?          ; adheres to isolation when symptomatic
  vaccinated?         ; vaccination status
  immunity            ; [0..1] protection, wanes over time
  target-px target-py ; current movement target (home/work)
]

patches-own [
  kind   ; "home" "work" "other"
  group  ; id for home/work
]

globals [
  tick-counter
  hour-of-day         ; 0..(ticks-per-day - 1)
  day-count

  ;; telemetry
  new-infections-today
  daily-incidence-list
]

; ---------------------- DEFAULTS ----------------------
to startup
  set population                600
  set num-households            200
  set household-size-mean       3
  set num-workplaces            30
  set ticks-per-day             24
  set speed                     0.6
  set contact-radius            1.2

  set base-beta-home            0.10
  set base-beta-work            0.05
  set base-beta-community       0.02

  set p-asym                    0.35
  set mean-incubation-days      3.5
  set mean-inf-days-sym         7
  set mean-inf-days-asym        5

  set hosp-prob-low             0.03
  set hosp-prob-high            0.10
  set mean-hosp-delay-days      5
  set mean-hosp-len-days        7
  set hosp-mortality            0.18

  set test-rate-sym-per-day     0.25
  set iso-effect                0.2
  set compliance-rate           0.8

  set vax-coverage              0.6
  set vax-effect-sus            0.5
  set vax-effect-inf            0.4
  set waning-per-day            0.002

  set wrap?                     true

  set daily-incidence-list      []
end

; ---------------------- SETUP ----------------------
to setup
  clear-all
  setup-patches
  setup-people
  seed-infections initial-infected-default
  reset-telemetry
  set tick-counter 0
  set hour-of-day 0
  set day-count 0
  reset-ticks
end

to-report initial-infected-default
  report 5  ;; change or add a slider named 'initial-infected' and call that instead
end

to setup-patches
  ask patches [ set kind "other" set group -1 ]
  let homes n-of num-households patches
  let hid 0
  ask homes [
    set kind "home"
    set group hid
    set pcolor brown - 1
    set hid hid + 1
  ]
  let works n-of num-workplaces patches with [ kind = "other" ]
  let wid 0
  ask works [
    set kind "work"
    set group wid
    set pcolor gray
    set wid wid + 1
  ]
end

to setup-people
  let home-patches patches with [ kind = "home" ]
  let work-patches patches with [ kind = "work" ]
  if (not any? home-patches) or (not any? work-patches) [ stop ]

  create-people population [
    initialize-person
    let hp one-of home-patches
    set home-id [group] of hp
    move-to hp
    let wp one-of work-patches
    set work-id [group] of wp
  ]
end

to initialize-person
  set size 1.1
  set color blue
  set state "S"
  set hours-in-state 0
  set risk-level (ifelse-value (random-float 1 < 0.25) [1] [0])
  set compliant? (random-float 1 < compliance-rate)
  set vaccinated? (random-float 1 < vax-coverage)
  set immunity (ifelse-value vaccinated? [ vax-effect-sus ] [ 0 ])
  set target-px pxcor
  set target-py pycor
end

to seed-infections [ n ]
  ask n-of n people with [ state = "S" ] [
    set state (ifelse-value (random-float 1 < p-asym) ["A"] ["I"])
    set hours-in-state 0
    recolor
  ]
end

to reset-telemetry
  set new-infections-today 0
  set daily-incidence-list []
end

; ---------------------- MAIN LOOP ----------------------
to go
  if all? people [ state = "R" or state = "D" ] [ stop ]
  update-daytime
  move-people
  make-contacts-and-transmit
  advance-disease-states
  wane-immunity
  update-telemetry
  set tick-counter tick-counter + 1
  tick
end

; ---------------------- TIME / SCHEDULE ----------------------
to update-daytime
  set hour-of-day (tick-counter mod ticks-per-day)
  if hour-of-day = 0 [
    set day-count day-count + 1
    set daily-incidence-list lput new-infections-today daily-incidence-list
    if length daily-incidence-list > 30 [
      set daily-incidence-list but-first daily-incidence-list
    ]
    set new-infections-today 0
  ]

  let daytime? (hour-of-day >= 8 and hour-of-day < 18)
  ask people with [ state != "H" and state != "D" ] [
    let dest one-of patches with [
      (daytime? and kind = "work" and group = [work-id] of myself) or
      ((not daytime?) and kind = "home" and group = [home-id] of myself)
    ]
    if dest != nobody [
      set target-px [pxcor] of dest
      set target-py [pycor] of dest
    ]
  ]
end

; ---------------------- MOVEMENT ----------------------
to move-people
  ask people with [ state != "H" and state != "D" ] [
    let spd speed
    if state = "I" and compliant? [ set spd (speed * iso-effect) ]
    facexy (target-px + random-float 0.6 - 0.3) (target-py + random-float 0.6 - 0.3)
    fd spd

    ifelse wrap? [
      ; world wraps in Web by default; nothing needed
    ] [
      if (pxcor = max-pxcor) or (pxcor = min-pxcor) [ set heading (180 - heading) ]
      if (pycor = max-pycor) or (pycor = min-pycor) [ set heading (- heading) ]
      set heading (heading mod 360)
    ]
  ]
end

; ---------------------- TRANSMISSION ----------------------
to make-contacts-and-transmit
  let daytime? (hour-of-day >= 8 and hour-of-day < 18)
  ask people with [ state = "I" or state = "A" ] [
    let base (ifelse-value daytime? [base-beta-work] [base-beta-home])
    let beta (base + base-beta-community)

    if state = "I" and compliant? [ set beta (beta * iso-effect) ]
    if vaccinated? [ set beta (beta * (1 - vax-effect-inf)) ]

    let nearby-people people in-radius contact-radius
    ask nearby-people with [ state = "S" ] [
      let sus 1
      if vaccinated? [ set sus (sus * (1 - vax-effect-sus)) ]
      set sus (max list 0 (1 - immunity))
      let p-infect beta * sus
      if random-float 1 < p-infect [
        set state "E"
        set hours-in-state 0
        recolor
        set new-infections-today (new-infections-today + 1)
      ]
    ]
  ]
end

; ---------------------- DISEASE PROGRESSION ----------------------
to advance-disease-states
  ask people with [ state != "D" ] [
    set hours-in-state (hours-in-state + 1)

    if state = "E" [
      if bernoulli-by-mean hours-in-state mean-incubation-days [
        ifelse (random-float 1 < p-asym) [ set state "A" ] [ set state "I" ]
        set hours-in-state 0
        recolor
      ]
    ]

    if state = "A" [
      if bernoulli-by-mean hours-in-state mean-inf-days-asym [
        set state "R"
        set hours-in-state 0
        set immunity min list 1 (immunity + 0.7)
        recolor
      ]
    ]

    if state = "I" [
      ; hospitalization consideration after delay
      if bernoulli-by-mean hours-in-state mean-hosp-delay-days [
        if random-float 1 < (ifelse-value (risk-level = 1) [hosp-prob-high] [hosp-prob-low]) [
          set state "H"
          set hours-in-state 0
          recolor
        ]
      ]
      ; recovery if not hospitalized
      if state = "I" and bernoulli-by-mean hours-in-state mean-inf-days-sym [
        set state "R"
        set hours-in-state 0
        set immunity min list 1 (immunity + 0.8)
        recolor
      ]
    ]

    if state = "H" [
      if bernoulli-by-mean hours-in-state mean-hosp-len-days [
        ifelse random-float 1 < hosp-mortality [
          set state "D"
        ] [
          set state "R"
          set immunity min list 1 (immunity + 0.9)
        ]
        set hours-in-state 0
        recolor
      ]
    ]
  ]
end

; With mean M days to event, per-tick probability ~ 1/(M * ticks-per-day)
to-report bernoulli-by-mean [ hrs mean-days ]
  let p (1 / (mean-days * ticks-per-day))
  report (random-float 1 < p)
end

; ---------------------- IMMUNITY WANING ----------------------
to wane-immunity
  if waning-per-day <= 0 [ stop ]
  let per-tick-wane (waning-per-day / ticks-per-day)
  ask people with [ state != "D" ] [
    set immunity max list 0 (immunity - per-tick-wane)
  ]
end

; ---------------------- TELEMETRY (optional hooks) ----------------------
to update-telemetry
  ;; placeholder so the body isn’t empty
  set new-infections-today new-infections-today
end


; ---------------------- COLORS ----------------------
to recolor
  if state = "S" [ set color blue    ]
  if state = "E" [ set color orange  ]
  if state = "A" [ set color cyan    ]
  if state = "I" [ set color red     ]
  if state = "H" [ set color magenta ]
  if state = "R" [ set color green   ]
  if state = "D" [ set color gray    ]
end

@#$#@#$#@
GRAPHICS-WINDOW
380
125
817
563
-1
-1
13
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30

BUTTON
5
20
120
50
setup
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

BUTTON
130
20
245
50
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
7
58
177
91
population
population
50
5000
600
1
1
NIL
HORIZONTAL

SLIDER
9
97
179
130
num-households
num-households
10
1000
200
1
1
NIL
HORIZONTAL

SLIDER
9
138
179
171
household-size-mean
household-size-mean
1
6
3
0.5
1
NIL
HORIZONTAL

SLIDER
10
178
180
211
num-workplaces
num-workplaces
5
200
30
1
1
NIL
HORIZONTAL

SLIDER
10
218
180
251
ticks-per-day
ticks-per-day
12
48
24
1
1
NIL
HORIZONTAL

SLIDER
12
258
182
291
speed
speed
0
1.5
0.6
0.1
1
NIL
HORIZONTAL

SLIDER
13
298
183
331
contact-radius
contact-radius
0.5
3
1.2
0.1
1
NIL
HORIZONTAL

SLIDER
14
338
184
371
base-beta-home
base-beta-home
0
0.3
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
13
380
183
413
base-beta-work
base-beta-work
0
0.3
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
10
420
180
453
p-asym
p-asym
0
1
0.35
0.05
1
NIL
HORIZONTAL

SLIDER
13
465
183
498
mean-incubation-days
mean-incubation-days
1
7
3.5
0.5
1
NIL
HORIZONTAL

SLIDER
15
505
185
538
mean-inf-days-sym
mean-inf-days-sym
2
14
7
1
1
NIL
HORIZONTAL

SLIDER
16
547
186
580
mean-inf-days-asym
mean-inf-days-asym
2
10
5
1
1
NIL
HORIZONTAL

SLIDER
190
60
360
93
hosp-prob-low
hosp-prob-low
0
0.2
0.03
0.01
1
NIL
HORIZONTAL

SLIDER
190
100
360
133
hosp-prob-high
hosp-prob-high
0
0.3
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
190
140
360
173
hosp-mortality
hosp-mortality
0
0.4
0.18
0.01
1
NIL
HORIZONTAL

SLIDER
185
180
355
213
test-rate-sym-per-day
test-rate-sym-per-day
0
0.7
0.25
0.05
1
NIL
HORIZONTAL

SLIDER
185
220
355
253
iso-effect
iso-effect
0
1
0.2
0.05
1
NIL
HORIZONTAL

SLIDER
190
260
360
293
compliance-rate
compliance-rate
0
1
0.8
0.05
1
NIL
HORIZONTAL

SLIDER
193
300
363
333
vax-coverage
vax-coverage
0
1
0.6
0.05
1
NIL
HORIZONTAL

SLIDER
185
340
355
373
vax-effect-sus
vax-effect-sus
0
0.9
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
190
380
360
413
vax-effect-inf
vax-effect-inf
0
0.9
0.4
0.05
1
NIL
HORIZONTAL

SLIDER
190
425
360
458
waning-per-day
waning-per-day
0
0.02
0.002
0.001
1
NIL
HORIZONTAL

SWITCH
255
20
355
53
wrap?
wrap?
1
1
-1000

MONITOR
380
20
450
65
Hour
hour-of-day
17
1
11

MONITOR
455
20
525
65
Day
day-count
17
1
11

MONITOR
530
20
600
65
E Count
count people with [state = \"E\"]
17
1
11

MONITOR
605
20
675
65
I Count
count people with [state = \"I\"]
17
1
11

MONITOR
680
20
750
65
A Count
count people with [state = \"A\"]
17
1
11

MONITOR
380
70
450
115
H Count
count people with [state = \"H\"]
17
1
11

MONITOR
455
70
525
115
R count
count people with [state = \"R\"]
17
1
11

MONITOR
530
70
600
115
D count
count people with [state = \"D\"]
17
1
11

MONITOR
605
70
750
115
New Infections Today
new-infections-today
17
1
11

PLOT
0
590
200
750
Incidence (daily)
Time
Infections
0
10
0
10
true
false
"" ""
PENS
"inc" 1 0 -7500403 true "if hour-of-day = 0 [ plot new-infections-today ]\n" "if hour-of-day = 0 [ plot new-infections-today ]\n"

PLOT
233
596
433
756
State Counts
NIL
NIL
0
10
0
10
true
false
"" ""
PENS
"E" 1 0 -7500403 true "plot count people with [state = \"E\"]" "plot count people with [state = \"E\"]"
"I" 1 0 -2674135 true "plot count people with [state = \"I\"]" "plot count people with [state = \"I\"]"
"A" 1 0 -955883 true "plot count people with [state = \"A\"]" "plot count people with [state = \"A\"]"
"H" 1 0 -6459832 true "plot count people with [state = \"H\"]" "plot count people with [state = \"H\"]"
"R" 1 0 -1184463 true "plot count people with [state = \"R\"]" "plot count people with [state = \"R\"]"
"D" 1 0 -10899396 true "plot count people with [state = \"D\"]" "plot count people with [state = \"D\"]"

SLIDER
197
470
367
503
base-beta-community
base-beta-community
0
0.1
0.02
0.005
1
NIL
HORIZONTAL

SLIDER
195
510
365
543
mean-hosp-delay-days
mean-hosp-delay-days
1
10
5
1
1
NIL
HORIZONTAL

SLIDER
199
555
369
588
mean-hosp-len-days
mean-hosp-len-days
2
21
7
1
1
NIL
HORIZONTAL
@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0
-0.2 0 0 1
0 1 1 0
0.2 0 0 1
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@

@#$#@#$#@
