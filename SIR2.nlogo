breed [ people person ]

people-own [
  state               
  hours-in-state    
  home-id            
  work-id         
  risk-level          
  compliant?         
  vaccinated?         
  immunity           
  target-px target-py 
]

patches-own [
  kind 
  group
]

globals [
  tick-counter
  hour-of-day 
  day-count

  new-infections-today
  daily-incidence-list
]

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

to move-people
  ask people with [ state != "H" and state != "D" ] [
    let spd speed
    if state = "I" and compliant? [ set spd (speed * iso-effect) ]
    facexy (target-px + random-float 0.6 - 0.3) (target-py + random-float 0.6 - 0.3)
    fd spd

    ifelse wrap? [
    ] [
      if (pxcor = max-pxcor) or (pxcor = min-pxcor) [ set heading (180 - heading) ]
      if (pycor = max-pycor) or (pycor = min-pycor) [ set heading (- heading) ]
      set heading (heading mod 360)
    ]
  ]
end

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
      if bernoulli-by-mean hours-in-state mean-hosp-delay-days [
        if random-float 1 < (ifelse-value (risk-level = 1) [hosp-prob-high] [hosp-prob-low]) [
          set state "H"
          set hours-in-state 0
          recolor
        ]
      ]
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

to-report bernoulli-by-mean [ hrs mean-days ]
  let p (1 / (mean-days * ticks-per-day))
  report (random-float 1 < p)
end

to wane-immunity
  if waning-per-day <= 0 [ stop ]
  let per-tick-wane (waning-per-day / ticks-per-day)
  ask people with [ state != "D" ] [
    set immunity max list 0 (immunity - per-tick-wane)
  ]
end

to update-telemetry
  set new-infections-today new-infections-today
end


to recolor
  if state = "S" [ set color blue    ]
  if state = "E" [ set color orange  ]
  if state = "A" [ set color cyan    ]
  if state = "I" [ set color red     ]
  if state = "H" [ set color magenta ]
  if state = "R" [ set color green   ]
  if state = "D" [ set color gray    ]
end
