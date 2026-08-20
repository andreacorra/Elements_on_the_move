;;This model take the hourly data for the deer that has been padded and splot so each turtles has one year of data
;Here, all the deer start March 11th


extensions [gis table csv ]

globals [
  GPS-tracks
  current-point
  current-deer
  real-deers-list
  row-nr
  nr-of-deer
  Hour
  Day
  Month
  time-deer
  winter-N-raster
  spring-N-raster
  summer-N-raster
  land-use-raster
 ]


breed[ deers deer]


turtles-own [
  ID
  body-n
  consumed-n
  n-to-be-excreted
  excretion-rate
  leader?
  my-leader
  potential-patch
]

patches-own [
  seasonally-available-n
  patch-visited?
  total-n-deposited
  total-n-consumed
  net-n-changed
  land?
  spring-N-value
  winter-N-value
  summer-N-value
  land-use-value
]


;;;;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  reset-ticks
  file-close-all

  set  winter-N-raster gis:load-dataset "winter_NDVI.asc"
  set  spring-N-raster gis:load-dataset "spring_NDVI.asc"
  set  summer-N-raster gis:load-dataset "latesum_NDVI.asc"
  set  land-use-raster gis:load-dataset "landuse.asc"

  if Model = "Migrant_and_Resident_300" [
    set GPS-tracks  gis:load-dataset "All_Deer.shp"
    ;; gis:load-coordinate-system "All_Deer.prj" line removed as coordinates are now local planar
    set row-nr 0
    set nr-of-deer 30 ] ;Note - this value should be replcaed with the number of GPS tracks for the simulation


  if Model = "Resident_150" [
    set GPS-tracks  gis:load-dataset "Resident_Deer.shp"
    ;; gis:load-coordinate-system "Resident_Deer.prj" line removed as coordinates are now local planar
    set row-nr 0
    set nr-of-deer 17 ] ;Note - this value should be replcaed with the number of GPS tracks for the simulation


  if Model = "Resident_300"[
    set GPS-tracks gis:load-dataset "Resident_Deer.shp"
    ;; gis:load-coordinate-system "Resident_Deer.prj" line removed as coordinates are now local planar
    set row-nr 0
    set nr-of-deer 17 ] ;Note - this value should be replcaed with the number of GPS tracks for the simulation


  if Model = "Migrant_150" [
    set GPS-tracks gis:load-dataset "Resident_Deer.shp"
    ;; gis:load-coordinate-system "Resident_Deer.prj" line removed as coordinates are now local planar
    set row-nr 0
    set nr-of-deer 13 ] ;Note - this value should be replcaed with the number of GPS tracks for the simulation


  let width ceiling (gis:width-of winter-N-raster  / 2)
  let height ceiling (gis:height-of winter-N-raster  / 2)
  resize-world (-1 * width ) width (-1 * height ) height

  set-patch-size 1

  gis:set-world-envelope gis:envelope-of winter-N-raster
  gis:apply-raster winter-N-raster winter-N-value
  gis:apply-raster spring-N-raster spring-N-value
  gis:apply-raster summer-N-raster summer-N-value
  gis:apply-raster land-use-raster land-use-value

  set-up-patches
  create-actual-agents


  set Hour gis:property-value time-Deer "HOUR"
  set Day gis:property-value time-Deer "DAY"
  set Month gis:property-value time-Deer "MONTH"

end


;;;;;;; PROCEDURES TO HELP IN LOADING DATA




to set-up-patches
  ask patches [
    if (   land-use-value < 1) [
      set land? false  ]
    if (   land-use-value >= 1) [
      set land?  true
      set seasonally-available-n winter-N-value
      set pcolor scale-color green  land-use-value 0 10
      set patch-visited? false
    ]]
end



to create-actual-agents
 set  current-deer n-values nr-of-deer [ ?0 -> ?0 ]
 set real-deers-list (list (item row-nr gis:feature-list-of GPS-tracks) )
 set time-Deer item row-nr gis:feature-list-of GPS-tracks
 set  current-deer remove 0  current-deer
; add the rest of deer at the current time step into the list
 foreach  current-deer [ ?1 -> set real-deers-list  lput (item (row-nr + 1) gis:feature-list-of GPS-tracks) real-deers-list
  set row-nr row-nr + 1  ]
set row-nr nr-of-deer
 (foreach real-deers-list  n-values nr-of-deer [ ?0 -> ?0 ]  [ ?0 ->
  set current-point (?0)
     let coords gis:location-of gis:centroid-of current-point
    create-deers 1 [
     set ID gis:property-value current-point "New_AID"
       setxy item 0 coords item 1 coords
      set leader? true
      set label ID
       set  consumed-n 7.123
   ;   ask patches in-radius 30 [ set list-of-ids lput [ID] of myself  list-of-ids ]
     set shape "moose"
    set label who
    set color red
    set size 20

      ; adding deer herds around the real deer
 if Model = "Migrate_and_Resident_300" [ hatch 9 [
        set leader? false ]]

 if Model = "Resident_150" [ hatch 7 [
        set leader? false ]]

 if Model = "Resident_300" [ hatch 16 [
          set leader? false ]]

 if Model = "Migration_150" [ hatch 10 [
        set leader? false ]]

  ]])

;;just getting it up to the right number
 if Model = "Resident_150" [
  ask n-of 14 turtles with [leader? = true] [ hatch 1 [
        set leader? false ]]]


 if Model = "Resident_300" [
  ask n-of 11 turtles with [leader? = true] [ hatch 1 [
        set leader? false ]]]


 if Model = "Migration_150" [
  ask n-of 7 turtles with [leader? = true] [ hatch 1 [
        set leader? false ]]]




  ask turtles with [leader? = false] [
        let possible-leaders turtles with [leader? = true and ID = [ID] of myself ]
        set my-leader one-of possible-leaders
        let movable-patches ([ patches in-radius 20 ] of my-leader )
        set potential-patch  one-of movable-patches
        move-to potential-patch
        set label [who] of my-leader
      set color blue ]

  ask turtles [
    set body-n random-normal 4703  2 ;winter mass
    set n-to-be-excreted random-normal  85  2
    set excretion-rate n-to-be-excreted / 12
  ]

end



;;;;;;;  GO ;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  read-GPS
  set Hour gis:property-value time-Deer "HOUR"
  set Day gis:property-value time-Deer "DAY"
  set Month gis:property-value time-Deer "MONTH"

  ask turtles[
    start-day
    consume-n
    defecate
  ]

  ask patches [
    if (any? turtles-here) [ set  patch-visited? true ]  ]
 ask patches with [ patch-visited? = true  and seasonally-available-n = 0 and land? = false] [ set pcolor red]

  change-seasons
  tick

end
;;;;;;;  MODEL PROCESSES IN GO ;;;;;;;;;;;;;;;;;;;;;;;;;

to read-GPS
  set current-deer n-values nr-of-deer [ ?0 -> ?0 ]
  set real-deers-list  (list (item row-nr gis:feature-list-of GPS-tracks) )
  set time-Deer item row-nr gis:feature-list-of GPS-tracks
  set  current-deer remove 0  current-deer
  foreach  current-deer [ ?0 -> set real-deers-list  lput (item (row-nr + 1) gis:feature-list-of GPS-tracks) real-deers-list
    set row-nr row-nr + 1 ]
  set row-nr row-nr + 1
  foreach  real-deers-list  [ ?0 ->
    set current-point (?0)
    let coords gis:location-of gis:centroid-of current-point
    ask deers with [ID = gis:property-value current-point "New_AID" and leader? = true] [
      setxy item 0 coords item 1 coords
  ]]

  ask turtles with [leader? = false] [
    let movable-patches ([ patches in-radius 20 with [land? = true]] of my-leader  )
    set potential-patch  one-of movable-patches
    move-to potential-patch]
end

to consume-n
  if Month = 5 or Month = 6 or Month = 7 or Month = 8 or Month = 9 or Month = 10 [
  if  Hour < 6 or Hour > 17 [
   ;this is summer amount of tood
    ifelse [seasonally-available-n] of patch-here >= 10.713 [  ;shoudl be 5.4*2 because they only eat for 12 ticks
      set seasonally-available-n seasonally-available-n - 10.713
      set  consumed-n  consumed-n + 10.713
      set total-n-consumed total-n-consumed + 10.713
      set net-n-changed net-n-changed - 10.713
      ]
   [ let left-n seasonally-available-n
      set seasonally-available-n seasonally-available-n - left-n
      set  consumed-n  consumed-n + left-n
      set total-n-consumed total-n-consumed + left-n
      set net-n-changed net-n-changed - left-n
  ]] ]

  if Month = 1 or Month = 2 or Month = 3 or Month = 4 or Month = 11 or Month = 12 [
   if  Hour < 6 or Hour > 17 [
      ifelse [seasonally-available-n] of patch-here >= 7.123 [ ; this is winter amount of food they only eat for 12 ticks
      set seasonally-available-n seasonally-available-n - 7.123 ;shoudl be 4.95*2
      set consumed-n consumed-n + 7.123
      set total-n-consumed total-n-consumed + 7.123
      set net-n-changed net-n-changed - 7.123 ]
  [ let left-n seasonally-available-n
      set seasonally-available-n seasonally-available-n - left-n
      set consumed-n consumed-n + left-n
      set total-n-consumed total-n-consumed + left-n
      set net-n-changed net-n-changed - left-n ]
    ]  ]

end

to defecate
  if ticks mod 2 = 0 [
    let excretion excretion-rate
    ask patch-here [
      set total-n-deposited total-n-deposited + excretion
      set net-n-changed  net-n-changed + excretion
  ]]
end


to start-day
  if Hour = 0 [
   if Month = 5 or Month = 6 or Month = 7 or Month = 8 or Month = 9 or Month = 10 [  ;Summer metabolistm starting May 1 ending Nov 1
    set body-n body-n + (consumed-n * .21) ;
    set body-n body-n - 24.219 ;
    let waste-n consumed-n * 0.79 + 24.219  ;
    set n-to-be-excreted waste-n ;move what should be urinated to be urinated tomorrow
    set excretion-rate n-to-be-excreted / 12
    set consumed-n 0  ]

 if Month = 1 or Month = 2 or Month = 3 or Month = 4 or Month = 11 or Month = 12 [  ;witer metabolistm
    set body-n body-n + (consumed-n * .17) ;
    set body-n body-n - 17.308  ;
    let waste-n (consumed-n * 0.83) + 17.308  ;
    set n-to-be-excreted waste-n ;move what should be urinated to be urinated tomorrow
    set excretion-rate n-to-be-excreted / 12
    set consumed-n 0  ]
  ]
end

;;;;;;;  Seasonal Dynamics  ;;;;;;;;;;;;;;;;;;;;;;;;;
to change-seasons
  if Month = 5 and Day = 1 and Hour = 0 ;[This starts May 1
  [ ask patches [
    set  seasonally-available-n spring-N-value  ]]
   if Month = 8 and Day = 1 and Hour = 0; This starts august 1
  [ ask patches
    [set  seasonally-available-n summer-N-value  ]]
  if Month = 11 and Day = 1 and Hour = 0 ; This starts november 1
  [ ask patches
    [set  seasonally-available-n winter-N-value  ]]
end
@#$#@#$#@
GRAPHICS-WINDOW
232
61
2937
1223
-1
-1
1.0
1
10
1
1
1
0
1
1
1
-1348
1348
-576
576
1
1
1
ticks
30.0

BUTTON
0
235
98
268
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

MONITOR
129
154
186
199
NIL
Hour
17
1
11

MONITOR
66
154
123
199
NIL
Day
17
1
11

MONITOR
4
155
61
200
NIL
Month
17
1
11

MONITOR
11
67
98
112
NIL
count deers
17
1
11

CHOOSER
9
10
226
55
Model
Model
"Migrant_and_Resident_300" "Resident_150" "Resident_300" "Migrant_150"
1

BUTTON
4
281
85
314
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
1

@#$#@#$#@
## WHAT IS IT?

This model simulates how red deer (Cervus elaphus) contribute to nitrogen movement across a mountainous landscape through their seasonal migration and daily foraging behavior. It was developed to explore the ecological consequences of migration, especially the redistribution of nutrients such as nitrogen via consumption and excretion.

Set in the Central Italian Alps, the model combines real GPS data from red deer with spatial data on vegetation productivity (NDVI) and land cover to track how individual deer interact with their environment over time. The model differentiates between real (leader) deer, based on actual GPS tracks, and simulated (follower) deer that move in relation to these leaders. Agents consume seasonally available nitrogen and deposit waste, altering local nutrient dynamics at the patch level.

This spatially explicit model helps quantify how the disruption of migration may influence ecosystem functions such as nutrient cycling and soil fertility.

## HOW IT WORKS

The model uses an agent-based approach where each deer-agent represents one year of movement data. Real deer tracks (leaders) are imported from a shapefile with hourly GPS points and assigned unique IDs. These leaders are accompanied by simulated deer (followers), which move in relation to their assigned leader.

The model landscape consists of patches (10x10 meter grid cells) that contain nitrogen availability values for three seasons — winter, spring, and late summer — derived from NDVI-based raster layers. Land cover information is also imported to distinguish between land and non-habitat (e.g., water bodies). At each time step (one hour), deer move, consume nitrogen if it is a foraging hour, and defecate at regular intervals.

Key processes include:
Agent setup: Real GPS tracks are loaded, and leader and follower deer are created. Each follower is assigned to a leader and placed randomly within a defined radius.
Nitrogen dynamics: Patches track the total nitrogen consumed and deposited by deer, updating net nitrogen levels.
Seasonal transitions: The model updates nitrogen availability maps based on the simulated month and day to reflect seasonal changes in plant growth and forage quality.
Body mass and metabolism: Deer metabolism varies seasonally and affects how much nitrogen is retained, lost, or excreted.

Outputs include spatial patterns of nitrogen change, individual deer nutrient budgets, and a record of patch visitation, which can highlight critical foraging or nutrient deposition areas.

## HOW TO USE IT
Choose a Movement Model: In the interface, select from one of the available scenarios (e.g., "Migrate_and_Resident_300", "Resident_150", etc.). Each model varies in the number of deer agents and their movement patterns.

Run Setup:
Press the setup button to load the required spatial data:
GPS tracks of deer movement
Seasonal NDVI rasters for winter, spring, and summer
A land use raster that distinguishes between land and non-land patches
The setup process creates agents, assigns leader/follower roles, and initializes nitrogen values on the landscape.

Start the Simulation:
Press go to start the model.
Time advances hourly. Each hour, deer move to a new patch based on their GPS (or follower) behavior, consume nitrogen if it is a foraging hour, and excrete at regular intervals.
The model tracks nitrogen consumption, deposition, and net changes on each patch.

View and Analyze Outputs:
Patch colors represent land use or stress conditions (e.g., red patches may indicate areas with no remaining nitrogen or unsuitable habitat).
Labels and colors distinguish leader deer (typically red) from followers (blue).
Use monitors, plots, or export functions (if added) to observe individual-level or landscape-level nitrogen metrics.

Adapt the Model (Optional):
You can replace the included shapefiles with your own GPS data and NDVI rasters to simulate other species or ecosystems.
Modify metabolic parameters, consumption rates, and movement behavior to suit different animal species or ecological questions.

This model serves as a flexible platform for studying how animal movement influences nutrient redistribution across landscapes, with applications in conservation, landscape ecology, and ecosystem management.
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

moose
false
0
Polygon -7500403 true true 196 228 198 297 180 297 178 244 166 213 136 213 106 213 79 227 73 259 50 257 49 229 38 197 26 168 26 137 46 120 101 122 147 102 181 111 217 121 256 136 294 151 286 169 256 169 241 198 211 188
Polygon -7500403 true true 74 258 87 299 63 297 49 256
Polygon -7500403 true true 25 135 15 186 10 200 23 217 25 188 35 141
Polygon -7500403 true true 270 150 253 100 231 94 213 100 208 135
Polygon -7500403 true true 225 120 204 66 207 29 185 56 178 27 171 59 150 45 165 90
Polygon -7500403 true true 225 120 249 61 241 31 265 56 272 27 280 59 300 45 285 90

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
NetLogo 6.4.0
@#$#@#$#@
set population 200
setup
repeat 200 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Alps" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <postRun>csv:to-file "Alps_End_patches_oneyear_8.6.2024.csv" [ (list pxcor pycor seasonally-available-n Total-N-Deposited Total-N-Consumed  Net-N-Changed Yearly-Net-N-Changed   land-use-value) ] of patches
csv:to-file "Alps_End_deer_oneyear_8.6.2024.csv" [ (list body-n ) ] of turtles</postRun>
    <timeLimit steps="8759"/>
    <metric>Hour</metric>
    <metric>Day</metric>
    <metric>Month</metric>
    <metric>mean [body-n] of turtles</metric>
    <metric>min [body-n] of turtles</metric>
    <metric>max [body-n] of turtles</metric>
    <metric>count turtles</metric>
    <metric>mean [daily-n] of turtles</metric>
    <metric>max [daily-n] of turtles</metric>
    <metric>min [daily-n] of turtles</metric>
    <metric>mean [seasonally-available-n] of patches with [land? = true]</metric>
    <metric>mean [ate-today] of turtles</metric>
    <metric>min [ate-today] of turtles</metric>
  </experiment>
  <experiment name="Migrate_Deer_10" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <postRun>csv:to-file "Migrate_Patches_10.csv" [ (list pxcor pycor  seasonally-available-n patch-visited?  total-n-deposited total-n-consumed net-n-changed  land?) ] of patches</postRun>
    <timeLimit steps="8759"/>
    <metric>Hour</metric>
    <metric>Day</metric>
    <metric>Month</metric>
    <metric>mean [body-n] of turtles</metric>
    <metric>min [body-n] of turtles</metric>
    <metric>max [body-n] of turtles</metric>
    <metric>mean [daily-n] of turtles</metric>
    <metric>max [daily-n] of turtles</metric>
    <metric>min [daily-n] of turtles</metric>
    <metric>[who] of turtles with-min [daily-n]</metric>
    <enumeratedValueSet variable="Migrate">
      <value value="&quot;Migrate&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Resident_Large_Deer_7" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <postRun>csv:to-file "Resident_Large_7.csv" [ (list pxcor pycor  seasonally-available-n patch-visited?  total-n-deposited total-n-consumed net-n-changed  land?) ] of patches</postRun>
    <timeLimit steps="8759"/>
    <metric>Hour</metric>
    <metric>Day</metric>
    <metric>Month</metric>
    <metric>count deers</metric>
    <metric>mean [body-n] of turtles</metric>
    <metric>min [body-n] of turtles</metric>
    <metric>max [body-n] of turtles</metric>
    <metric>mean [daily-n] of turtles</metric>
    <metric>max [daily-n] of turtles</metric>
    <metric>min [daily-n] of turtles</metric>
    <metric>[who] of turtles with-min [daily-n]</metric>
    <enumeratedValueSet variable="Migrate">
      <value value="&quot;Resident_Large&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Resident_Small_204Deer" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <postRun>csv:to-file "Alps_Resident_Small.csv" [ (list pxcor pycor  seasonally-available-n patch-visited?  total-n-deposited total-n-consumed net-n-changed  land?) ] of patches</postRun>
    <timeLimit steps="8760"/>
    <metric>Hour</metric>
    <metric>Day</metric>
    <metric>Month</metric>
    <metric>mean [body-n] of turtles</metric>
    <metric>min [body-n] of turtles</metric>
    <metric>max [body-n] of turtles</metric>
    <metric>mean [daily-n] of turtles</metric>
    <metric>max [daily-n] of turtles</metric>
    <metric>min [daily-n] of turtles</metric>
    <metric>[who] of turtles with-min [daily-n]</metric>
    <enumeratedValueSet variable="Migrate">
      <value value="&quot;Resident_Small&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Resident_150" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <postRun>csv:to-file "Resident_150_10.csv" [ (list pxcor pycor  seasonally-available-n patch-visited?  total-n-deposited total-n-consumed net-n-changed  land?) ] of patches</postRun>
    <timeLimit steps="8759"/>
    <metric>Hour</metric>
    <metric>Day</metric>
    <metric>Month</metric>
    <metric>count deers</metric>
    <metric>mean [body-n] of turtles</metric>
    <metric>min [body-n] of turtles</metric>
    <metric>max [body-n] of turtles</metric>
    <metric>mean [daily-n] of turtles</metric>
    <metric>max [daily-n] of turtles</metric>
    <metric>min [daily-n] of turtles</metric>
    <metric>[who] of turtles with-min [daily-n]</metric>
    <enumeratedValueSet variable="Model">
      <value value="&quot;Resident_150&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Resident_300_" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <postRun>csv:to-file "Resident_300_10.csv" [ (list pxcor pycor  seasonally-available-n patch-visited?  total-n-deposited total-n-consumed net-n-changed  land?) ] of patches</postRun>
    <timeLimit steps="8759"/>
    <metric>Hour</metric>
    <metric>Day</metric>
    <metric>Month</metric>
    <metric>count deers</metric>
    <metric>mean [body-n] of turtles</metric>
    <metric>min [body-n] of turtles</metric>
    <metric>max [body-n] of turtles</metric>
    <metric>mean [daily-n] of turtles</metric>
    <metric>max [daily-n] of turtles</metric>
    <metric>min [daily-n] of turtles</metric>
    <metric>[who] of turtles with-min [daily-n]</metric>
    <enumeratedValueSet variable="Model">
      <value value="&quot;Resident_300&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Migration_150_4" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <postRun>csv:to-file "Migration_150_4.csv" [ (list pxcor pycor  seasonally-available-n patch-visited?  total-n-deposited total-n-consumed net-n-changed  land?) ] of patches</postRun>
    <timeLimit steps="8759"/>
    <metric>Hour</metric>
    <metric>Day</metric>
    <metric>Month</metric>
    <metric>count deers</metric>
    <metric>mean [body-n] of turtles</metric>
    <metric>min [body-n] of turtles</metric>
    <metric>max [body-n] of turtles</metric>
    <metric>mean [daily-n] of turtles</metric>
    <metric>max [daily-n] of turtles</metric>
    <metric>min [daily-n] of turtles</metric>
    <metric>[who] of turtles with-min [daily-n]</metric>
    <enumeratedValueSet variable="Model">
      <value value="&quot;Migration_150&quot;"/>
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
