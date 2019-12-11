; Social Contagion in social networks
;
; Aline Guimarães
;
;
; COMENTAR O CODIGO
; -----------------------------------------------------------------------------------
; Base code by:
; steve scott
; sscotta@gmu.edu
; George Mason University
; Computational Social Science Program
; Fairfax VA
;
; Fall 2014
; -----------------------------------------------------------------------------------
extensions [nw]
breed [ users user ]
undirected-link-breed [edges edge]

globals [ path
  average_betweenness
  average_closeness
  average_eigen
  std_betweenness
  std_closeness
  std_eigen
  max_betweenness
  max_closeness
  max_eigen
  max_friends
  average_friendsa
  std_friends
  user-list
  all-tweets
  id-tweets
]

users-own [
  closeness        ; variables for the network
  betweenness
  eigen
  friends
  degrees

  user-positioning ; 0 is bad, 1 is like heaven
  user-mood
  user-openness    ; big 5 features that can help to analyze the probability to retweet the news
  user-conscientiousness
  influencer?      ; in case it is an influencer, the variables below are useful
  tweeting-rate    ; probability of tweeting every tick

  tweet-list       ; list of generated tweets
  retweet-list     ; list of retweeted tweets
  new-post         ; tweet created at this very tick
]

to setup
  clear-all
  setup-patches
  setup-network
  layout-network
  render-plot
  stats
  setup-colors        ; procedure to setup the color of the node depending on its positioning
  setup-influencers   ; procedure to setup the influencers (size) dependig on its quantity of links
  setup-tweets        ; procedure to inicialize global variables: all-tweets = [] and id-tweets = 0

  reset-ticks
end

to setup-patches
  ask patches [
    set pcolor black
  ]
end


to setup-erdos-renyii                              ; erdos-renyi
  nw:generate-random users edges num-nodes p       ; random network w/ probability prob-link of making a link
  ask users[
    set shape "circle"
    set size 1.25
    set influencer? false
    set tweeting-rate -1
    set user-positioning random-float 1
    set user-openness random-float 1
    set user-conscientiousness random-float 1
    set user-mood random-float 1
    set retweet-list []
    set tweet-list []
  ]
nw:save-matrix (word "er.txt")
end


to setup-barabasi-albert[ num-users prob-link max-links ]  ; barabasi-albert
                                                           ; sets up network with preferential attraction
  create-users num-nodes [
    set shape "circle"
    set size 1.25
    set influencer? false
    set tweeting-rate -1
    set user-positioning random-float 1
    set user-openness random-float 1
    set user-conscientiousness random-float 1
    set user-mood random-float 1
    set retweet-list []
    set tweet-list []

    let spot one-of patches with[not any? users-here]
    ifelse (spot != nobody) [
      move-to spot
    ]
    [setxy random-pxcor random-pycor]
  ]

  ask users [
    let  my-user self
    set degrees max-n-of max-links users [count link-neighbors]
    foreach sort degrees [ [?1] ->
      let chance random-float 1.0
      if (chance < prob-link) and (?1 != my-user) [
        ask my-user [
          create-link-with ?1
        ]
      ]
    ]
    set friends count link-neighbors
  ]


nw:save-matrix (word "pa.txt")
end


to setup-small-world[ num-users num-links prob-rewire ]  ; small-world
                                                         ; build a "small world" network
  create-users num-users [
    set shape "circle"
    set size 1.25
    set influencer? false
    set user-positioning random-float 1
    set user-openness random-float 1
    set user-conscientiousness random-float 1
    set user-mood random-float 1
    set tweeting-rate -1
    set retweet-list []
    set tweet-list []
    let spot one-of patches with[not any? users-here]
    ifelse (spot != nobody) [
      move-to spot
    ]
    [setxy random-pxcor random-pycor]
  ]

  set user-list sort users
  let i 0
  foreach user-list [ [?1] ->
    let j ?1
    let k 1
    let link-users []
    while [ k <= num-links ]
    [
      let ptr (i + k + (length user-list)) mod (length user-list)
      set link-users lput (item ptr user-list) link-users
      set k (k + 1)
    ]
    foreach link-users [ [??1] ->
      set k ??1
      ask j [ create-link-with k]
    ]
    set i (i + 1)
  ]

  ;
  ; make some long jump links
  ;
  foreach sort users [ [?1] ->
    ask ?1 [
      let temp sort link-neighbors
      foreach temp [ [??1] ->
        if random-float 1.0 < prob-rewire [
          let x one-of other users with [ abs (who - [who] of myself) > 2 ]
          if is-user? x
          [
            ask link-with ??1 [ die ]
            create-link-with x
          ]
        ]
      ]
    ]
  ]

    nw:save-matrix (word "sw.txt")
end


to setup-uniform[ num-users num-links ]     ; uniform
                                            ; each user has num-links links
  create-users num-users [
    set shape "circle"
    set size 1.25
    set influencer? false
    set user-positioning random-float 1
    set user-openness random-float 1
    set user-conscientiousness random-float 1
    set user-mood random-float 1
    set tweeting-rate -1
    set retweet-list []
    set tweet-list []
    let spot one-of patches with[not any? users-here]
    ifelse (spot != nobody) [
      move-to spot
    ]
    [setxy random-pxcor random-pycor]
  ]

  set user-list sort users
  let i 0
  foreach user-list [ [?1] ->
    let j ?1
    let k 1
    let link-users []
    while [ k <= num-links ]
    [
      let ptr (i + k + (length user-list)) mod (length user-list)
      set link-users lput (item ptr user-list) link-users
      set k (k + 1)
    ]
    foreach link-users [ [??1] ->
      set k ??1
      ask j [ create-link-with k]
    ]
    set i (i + 1)
  ]
end

to setup-network
  if network-type = "erdos-renyi" [ setup-erdos-renyii ]
  if network-type = "barabasi-albert" [ setup-barabasi-albert num-nodes p M]
  if network-type = "small-world" [ setup-small-world num-nodes M p ]
  if network-type = "uniform" [ setup-uniform num-nodes M ]
end

; example of a layout generator
;
; causes users to jiggle around rather aimlessly
;
to layout-equidistant [ num-loops ]
  repeat num-loops [
    ask users [

      ;
      ; scan area in conics range units out, width degrees wide
      ;
      let scan-angles [0 30 60 90 120 150 180 210 240 270 300 330]
      let _range 10
      let width 30
      let zone-count []
      foreach scan-angles [ [?1] ->
        set heading ?1
        let num count users-on patches in-cone _range width
        set zone-count (lput num zone-count)
      ]

      ;
      ; find least populated zone(s)
      ;
      let min-zone min zone-count
      let i 0
      let ptr []
      foreach zone-count [ [?1] ->
        if ?1 = min-zone [
          set ptr lput (item i scan-angles) ptr
        ]
        set i (i + 1)
      ]

      ;
      ; now move in most vacant direction
      ;
      set heading one-of ptr
      forward 0.5

      ;; type "debug: user " type self type " has zone count " type zone-count type " moving in direction " type ptr print " "
    ]
  ]
end

to layout-centroid [ d ]
  ask users [
    let nearby users in-radius d
    if (count nearby > 1) [
      let mid-x mean [pxcor] of nearby
      let mid-y mean [pycor] of nearby
      facexy mid-x mid-y
      set heading 180 + heading
      forward 1
    ]
  ]
end

to layout-random
  ask users [
    let spot one-of patches with [not any? users-here]
    if (spot != nobody) [ move-to spot ]
  ]
end

to layout-network
  if layout-type = "random" [ layout-random ]
  if layout-type = "spring" [ layout-spring users edges 1 3 2]
  if layout-type = "circle" [ layout-circle sort turtles max-pxcor * 0.9 ]
  if layout-type = "radial" [ layout-radial users edges user 1 ]
  if layout-type = "equidistant" [ layout-equidistant 50 ]
  if layout-type = "centroid" [ repeat 20 [ layout-centroid 2 ] ]
end


to go
  print "\n============================== tick ==============================\n "
  render-plot
  generate-tweets
  retweet-tweets

  if #-users-changing-friends[      ; switch to change links
    refresh-links-follow
    if ticks > 5 [                  ; if is needed, because the nodes has to generate content before the algorithm judges if the actual node like its content
      refresh-links-unfollow
    ]
    setup-influencers               ; has to setup again because the color can change
  ]

  if #-users-changing-positioning[  ; switch to change variable positioning if the node is in a skewed enviroment
    change-users-positioning
    setup-colors                    ; has to setup again because the color can change
  ]

  tick
end

; render-plot
;
; manually generate plots to show centrality info
;
to render-plot
  ;
  ; get the centrality data
  ;
  let degree-centrality-list sort [count link-neighbors] of users

  ;
  ; plot the degree distribution
  ;
  set-current-plot "Degree Centrality"
  clear-plot
  set-current-plot-pen "degree centrality"
  set-plot-pen-mode 0
  set-plot-x-range 0 (max (list degree-centrality-list 1.0))

  let i 0
  foreach degree-centrality-list [ [?1] ->
    plot-pen-down
    plotxy i ?1
    set i (i + 1)
    plot-pen-up
  ]

  ;
  ; histogram of degree distributions
  ;
  set-current-plot "Degree Distribution"
  clear-plot
  set-current-plot-pen "degree distribution"
  set-plot-pen-mode 1
  set-plot-x-range 0 (max degree-centrality-list) + 1
  set-histogram-num-bars 10
  histogram [count link-neighbors] of users
end

to stats
 ask users [
  set closeness  nw:closeness-centrality
   set betweenness nw:betweenness-centrality
   set eigen nw:eigenvector-centrality
   set friends count link-neighbors
  ]


  set average_betweenness mean [betweenness] of users
  set average_closeness mean [closeness] of users
  set std_betweenness standard-deviation [betweenness] of users
  set std_closeness standard-deviation [closeness] of users
  set max_betweenness max [betweenness] of users
  set max_closeness max [closeness] of users
  set max_friends max [friends] of users
  set std_friends standard-deviation [friends] of users

  if [eigen] of user 0 != false [
   set average_eigen mean [eigen] of users
    set std_eigen standard-deviation [eigen] of users
    set max_eigen max [eigen] of users
  ]

end

; SETUP INFLUENCERS
;
; used in setup and go
; procedure to setup the influencers (size) dependig on its quantity of links
;
to setup-influencers
  let degreeSortedList (sort-on [(- friends)] users) ; get the ones with the biggest number of friends
  let influencersList (sublist degreeSortedList 0 #-influencers) ; get a sublist with the number of influencers (slide)

  ask users [ ; set all users
    set size 1.25
  ]

  foreach influencersList [
     influencer -> ask influencer [ ; characteristics of influencers
       set influencer? true
       set size 2.25
       set tweeting-rate random-float influencers-rate
     ]
  ]
end

; GENERATE TWEETS
;
; used in go
; procedure to generate tweets with its random characteristics - only influencers
; tweet is added on all-tweets (global) and its id is added on the users tweet-list
;
to generate-tweets
  ask users [
    if influencer? [
      if tweeting-rate > #-medium-rate [ ; medium tweeting rate - variable in the slider
        let mood user-mood
        let positioning user-positioning
        let new-tweet []
        let if-fake random-float 1 ; random parameter - could be a slider threashold
        ifelse if-fake > 0.5 [ ; tweet structure: [id id-root tweet-sharer tick mood positioning fake?]
          set new-tweet (list id-tweets id-tweets self ticks mood positioning true)
        ][
          set new-tweet (list id-tweets id-tweets self ticks mood positioning false)
        ]
        set tweet-list lput (item 0 new-tweet) tweet-list
        share-tweet new-tweet ; function that was repeated code in generate and retweet
        set tweeting-rate random-float influencers-rate   ; change at every tick because of the if
     ]
    ]
  ]
end

; RETWEET TWEETS
;
; used in go
; procedure to retweet tweets, depending of the interest and positioning
; the retweet consists in creating a new list (tweet) with equal characteristics to the one that's being retweeted
; tweet is added on all-tweets (global) and its id is added on the users tweet-list
; only tweets from the last tick can be retweeted
;
to retweet-tweets
  ; if fake, 70% maior chance - NAO ESTA SENDO UTILIZADO MAS DEVE

  foreach all-tweets [
    fTweet ->
      if  (ticks > 0) and (item 3 fTweet = (ticks - 1)) [  ; filter the tweets that were tweeted or retweeted in the last tick
        let users-retweeting [] ; setting the list of users that will retweet - has to be blank at the beginning
        let comparable-interest 0 ; creating the comparable interest

        ask (item 2 fTweet) [ ; ask the tweet sharer
          ask link-neighbors [  ; linked to the sharer
            let tweet-root-id tweets-retweets-list ; return a list with all root id
            set comparable-interest abs((item 5 fTweet) - user-positioning) ; get the diference between the tweet positioning and user positioning, if its not far, retweet
            if (comparable-interest < #-threashold-tweet-interest) and (not member? (item 1 fTweet) tweet-root-id)[ ;
              set users-retweeting lput self users-retweeting  ; users linked that want to retweet
            ]
          ]
        ]

        foreach users-retweeting [
          user-retweeting -> ask user-retweeting [  ; retweet - create a tweet with characteristics from the one it wants to retweet
            let new-retweet (list id-tweets (item 0 fTweet) self ticks (item 4 fTweet) (item 5 fTweet) (item 6 fTweet)) ; tweet structure: [id id-root tweet-sharer tick mood positioning fake?]
            set retweet-list lput (item 0 new-retweet) retweet-list
            share-tweet new-retweet

          ]
        ]
     ]
  ]
end

; SHARE TWEETS
;
; used in generate-tweets and retweet-tweets
; procedure that recieves the new tweet and shares it
; sharing consist in putting on the super list and setting the links color
; this code was equal in two procedures
;
to share-tweet [tweet]
   set id-tweets (id-tweets + 1) ; increment of the id-tweet
   set all-tweets lput tweet all-tweets ; put the new tweet on the super list

    ask my-out-links[ ; setting colors
      (ifelse ((item 0 tweet) = (item 1 tweet)) and (item 6 tweet)[ ; verify if its tweet and true
        set color 15
      ]((item 0 tweet) = (item 1 tweet))[ ; verify if its tweet and false
        set color 55
      ]((item 0 tweet) != (item 1 tweet)) and (item 6 tweet)[ ; verify if its retweet and true
        set color 17
      ]((item 0 tweet) != (item 1 tweet))[ ; verify if its retweet and false
        set color 57
      ])
      set thickness .3
      set shape "friendship"
    ]
end

; REFRESH LINKS FOLLOW
;
; used in go
; procedure that review the retweets and follow users that produce interesting content for the current user
; need setup influencers after
;
to refresh-links-follow ; apparently doesn't work
  ask users [
    let listed-users [] ; list with users that current user will follow
    let tweet-sharer nobody

    foreach retweet-list [ ; goes throught all the tweets from the user
      retweet ->
        set tweet-sharer find-tweet-sharer retweet
        if (tweet-sharer != nobody) and (not member? tweet-sharer link-neighbors) and (tweet-sharer != self)[ ; see if the sharer of that tweet isn't already their friend
          set listed-users lput tweet-sharer listed-users ; if they aren't, theyl'll be put on this list
        ]
    ]

    set listed-users remove-duplicates listed-users

    foreach listed-users [
      listed-user -> ask listed-user [ ; all the users that produce interesting content and will be followed
        print "------------ NEW FRIEND ------------"
        create-edge-with myself
      ]
    ]
  ]
end

; REFRESH LINKS UNFOLLOW
;
; used in go
; procedure that review the retweets and unfollow users that produce interesting content for that user
; need setup influencers after
;
to refresh-links-unfollow ; apparently doesn't work
  ask users [
    let list-to-compare retweet-list ; retweet list from the main user

    ask link-neighbors [
      let unfollow? true
      let tweet-sharer nobody
      foreach list-to-compare [ ; foreach all their retweet list
        tweet-listed ->
          set tweet-sharer find-tweet-sharer tweet-listed ; get the sharer of the tweet
          if tweet-sharer != nobody and self = tweet-sharer [ ; if the tweeted sharer did tweet something, they won't be unfollowed
            set unfollow? false
          ]
      ]
      if unfollow? [ ; if they weren't he author of any tweets retweeted, they'll be unfollowed
        ; print "------------ UNFOLLOW FRIEND ------------"
        ; die
      ]
    ]
  ]
end


; CHANGE USERS POSITIONING
;
; used in go
; procedure if user is neutral and its inserted in a polarized environment, they can change opinions
; need setup colors after
;
to change-users-positioning
  let right-friends []  ; starting the list
  let left-friends []
  let neutral-friends []

  ask users [
    if (user-positioning > 0.45) and (user-positioning < 0.55) [ ; setting the links/friends in the list according to their positioning
      ask link-neighbors [
        (ifelse user-positioning > 0.55 [
          set right-friends lput self right-friends
        ] user-positioning < 0.45 [
          set left-friends lput self left-friends
        ] [
          set neutral-friends lput self neutral-friends
        ])
      ]

      if (length right-friends) > (length neutral-friends) or (length left-friends) > (length neutral-friends) [ ; virify if the node isn't on a polarized environment
        ifelse(length right-friends > length left-friends)[
          set user-positioning (user-positioning + (length right-friends * #-influence-positioning)) ; add the threashold into the user-positioning variable from user - threashold * number of friends
        ][
          set user-positioning (user-positioning + (length left-friends * #-influence-positioning))
        ]
      ]
    ]
  ]
end

; SETUP COLORS
;
; used in setup and go
; procedure to setup the color of the node depending on its positioning
;
to setup-colors
  ask users [
    (ifelse user-positioning > 0.55 [
        set color blue
     ] user-positioning < 0.45 [
        set color yellow
     ][
        set color white
     ])
  ]
end

; SETUP TWEETS
;
; used in setup
; procedure to inicialize global variables: all-tweets = [] and id-tweets = 0
;
to setup-tweets
  set all-tweets []
  set id-tweets 0
end

; FIND TWEET
;
; used in find-tweet-sharer, tweets-retweets-list
; procedure that returns the tweet (list) reciving the id from the tweet
;
to-report find-tweet [id-to-search]
  foreach all-tweets [
   tweet ->
      if id-to-search = (item 0 tweet) [
        report tweet
      ]
  ]
  report []
end

; FIND TWEET SHARER
;
; used in refresh-links-follow and refresh-links-unfollow
; procedure that returns the user that tweeted or retweeted that tweet reciving the id from the tweet
;
to-report find-tweet-sharer [id-to-search]
  let tweet find-tweet id-to-search
  if not empty? tweet [
    report (item 2 tweet)
  ]
  report nobody
end

; TWEETS RETWEETS LIST
;
; used in retweet-tweets
; procedure that returns a list with all id of tweets and retweets (id-root) by that user
;
to-report tweets-retweets-list
  let tweet-root-id []
  foreach retweet-list [
      id-tweet ->  ; return the id-root because its the one compared
        set tweet-root-id lput (item 1 (find-tweet id-tweet)) tweet-root-id
    ]
  foreach tweet-list [
     tweet ->
       set tweet-root-id lput tweet tweet-root-id
   ]
  set tweet-root-id remove-duplicates tweet-root-id
  report tweet-root-id
end
@#$#@#$#@
GRAPHICS-WINDOW
215
10
633
429
-1
-1
10.0
1
10
1
1
1
0
0
0
1
-20
20
-20
20
0
0
1
ticks
30.0

BUTTON
5
83
68
116
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
139
82
202
115
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

TEXTBOX
12
10
162
76
Social Contagion in Networks
18
0.0
1

CHOOSER
7
140
199
185
network-type
network-type
"erdos-renyi" "barabasi-albert" "small-world" "uniform"
1

SLIDER
660
41
832
74
p
p
0.0
1.0
0.293
0.0001
1
NIL
HORIZONTAL

CHOOSER
9
200
196
245
layout-type
layout-type
"random" "spring" "circle" "radial" "centroid" "equidistant"
0

PLOT
869
10
1069
160
Degree Centrality
Cumulative Frequency
Degree
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"degree centrality" 1.0 0 -16777216 true "" ""

PLOT
870
170
1070
320
Degree Distribution
Degree
No. of Nodes
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"degree distribution" 1.0 1 -16777216 true "" ""

MONITOR
871
328
928
373
N
count users
0
1
11

MONITOR
933
328
990
373
edges
count links
0
1
11

SLIDER
659
102
831
135
M
M
0
50
4.0
1
1
NIL
HORIZONTAL

BUTTON
62
271
128
304
layout
layout-network
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
661
21
850
49
P = probability of link (ER, BA, WS)
11
0.0
1

TEXTBOX
664
82
814
100
M = max links (BA)
11
0.0
1

MONITOR
871
390
948
435
no friends
count users with [friends = 0]
17
1
11

MONITOR
949
390
1006
435
max
max [friends] of users
17
1
11

BUTTON
72
82
135
115
go
go
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
661
184
833
217
num-nodes
num-nodes
0
1000
20.0
5
1
NIL
HORIZONTAL

SLIDER
661
242
833
275
#-influencers
#-influencers
0
250
8.0
1
1
NIL
HORIZONTAL

SLIDER
659
315
831
348
influencers-rate
influencers-rate
0
1
0.7
0.05
1
NIL
HORIZONTAL

TEXTBOX
666
145
816
163
N = influencers+users
11
0.0
1

TEXTBOX
665
165
815
193
total of nodes\n
11
0.0
1

TEXTBOX
664
224
814
242
nodes that produce content 
11
0.0
1

TEXTBOX
663
286
819
314
constant to calculate influencers rate\n
11
0.0
1

MONITOR
995
328
1090
373
normal users
num-nodes - #-influencers
17
1
11

MONITOR
1008
389
1090
434
influencers
#-influencers
17
1
11

SLIDER
659
387
831
420
#-medium-rate
#-medium-rate
0
1
0.4
0.05
1
NIL
HORIZONTAL

TEXTBOX
659
365
809
383
minimum rate to tweet
11
0.0
1

SWITCH
1099
145
1320
178
#-users-changing-friends
#-users-changing-friends
0
1
-1000

SLIDER
1096
64
1324
97
#-threashold-tweet-interest
#-threashold-tweet-interest
0
1
0.2
0.05
1
NIL
HORIZONTAL

TEXTBOX
1096
10
1327
66
threashold to compare to retweet, so node can retweet a tweet or not, according to interest
11
0.0
1

TEXTBOX
1098
112
1319
154
users change friends according to content shared
11
0.0
1

SWITCH
1089
234
1337
267
#-users-changing-positioning
#-users-changing-positioning
0
1
-1000

TEXTBOX
1099
194
1325
236
users that are inserted in a strong enviroment, can change positioning
11
0.0
1

MONITOR
1104
389
1179
434
All Tweets
length all-tweets
17
1
11

SLIDER
1109
330
1311
363
#-influence-positioning
#-influence-positioning
0
1
0.002
0.001
1
NIL
HORIZONTAL

TEXTBOX
1106
282
1316
338
how much it increases for each friend when a node is nserted in a skewed environment
11
0.0
1

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
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
Polygon -7500403 true true 135 285 195 285 270 90 30 90 105 285
Polygon -7500403 true true 270 90 225 15 180 90
Polygon -7500403 true true 30 90 75 15 120 90
Circle -1 true false 183 138 24
Circle -1 true false 93 138 24

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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

friendship
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
