extensions [matrix gis csv]

;variables turtles-own
globals [seed ;random seed
pv-share ;percentage households who installed PV
pv-no pv-yes ;[0,1] values to calculate both utility for and against PV
households-data ;list of hh data collected, imported from file
hh_fields ;variable related to GIS positioning household
use-hh ;variable related to GIS positioning household
pv-cost-per-m2 pv-peak-power sunshine-hour performance-ratio pv-lifetime grid-electricity-costs ;external constants for PV utility calculations
avoided-cost-per-kwh pv-co2-per-kwh pv_avg_co2 ;variables calculated from external constants for PV utility calculations per household
var-weights ;matrix of all weight sets from stakeholder workshop
Vj ;visibility value per household
wijk-dataset bouw-dataset geemente-dataset buurten-dataset ;imported household data
factor ;GIS units per NetLogo unit
n-turtles ;total number of turtles
pv-init ;transfer variable for setting initial PV share
]

;variables turtles-own
turtles-own [past-utility current-utility ;transfer variables to compare utilities
esystem ;energy system - list item 1 equal to one if PV installed, 0 if not
pv-saving pv-balance pv-emissions pv-comfort pv-pp ; [0,1] values to calculate utilities and connected variables for PV installation and no PV
hh_att ;list of all household attributes from data
hh_inc ;household income
avg_inc ;average household income
income-class ; household income class, [1,2,3]
probability_inc ;probability to pass income barrier, dependent on household income
hh_area hh_area_ratio avg-hh-size roof-size ;household area in sqm as well as normalised and normalization ratio and roof size
n-of-neighbors ;household's size of social network
pv-cof ;value comfort PV
pv-output cashflows-pv cumulative-pv total-pv-revenue pp_pv pv-money-saving total-pv-cost ;variables for calculation economic utility
pv_co2 ;CO2 emission saving for environmental utility
w_eco w_env w_soc w_cof ;weights for utility functions

pv-eco-util pv-eco-util-prep pv-eco-util-bar pv-env-util pv-soc-util pv-cof-util  pv-inc-util ;report variables for each utility
vis_test ;[0,1] technology visible to household or not
inc_test ;[0,1] household passed income test or not
thres_prob; threshold probabilityagainst which household income is compared MF

pv-yes? ;transfer variable for calculating pv-share
pv-eco-no pv-eco-yes pv-env-no pv-env-yes pv-soc-no pv-soc-yes pv-cof-no pv-cof-yes pv-inc-no pv-inc-yes;utility per category and install PV or not install PV
disaggregate ;list to export data on households attributes during the decision making process
t1 B1 C1 ;transfer variables calculation economic utility

;variables for financial information parts
cost_time ;transfer variable for amount of time invested in information search
total-pv-cost-fin; transfer variable for total PV cost
tot_rev ; total revenue per PV installation
prob_info_eco ;[0,1] depending on whether household gets information installer
unc_level_normalised ;transfer variables for inaccuracy of economic utility calculation
unc_level;inaccuracy of economic utility calculation

imp_attitude-yes attitude-yes control-yes imp_control-yes imp_social-yes social-yes ;transfer variables for utilities and their weights for Schwarz and Ernst's operationalization TPB
imp_attitude-no attitude-no control-no imp_control-no imp_social-no social-no ;transfer variables for utilities and their weights for Schwarz and Ernst's operationalization TPB
utility-RR ;sum utilities for RR operationalization TPB
total-pv-cost-fin-bar;
]

;prepare introduction datasets
breed [households household]
breed [ wijk-labels wijk-label]
breed [ buurt-labels buurt-label]
breed [ geemente-labels geemente-label ]

; functions to run simulation and set up interface views
to startup
  ca
  load-data
end

to load-data
  ca

  ; Load all datasets
  set wijk-dataset gis:load-dataset "DataProjected/Dalfsen_wijk_p.shp"         ;street code area
  set bouw-dataset gis:load-dataset "DataProjected/Dalfsen_huizen_p.shp"       ;households point shape file
  set geemente-dataset gis:load-dataset "DataProjected/Dalfsen_gem_p.shp"      ; Dalfsen geemente polygon shape file
  set buurten-dataset gis:load-dataset "DataProjected/Dalfsen_buurt_p.shp"     ; city section polygon shape file
  ;Set the world envelope to the union of all of our dataset's envelopes
  gis:set-world-envelope (gis:envelope-union-of (gis:envelope-of bouw-dataset)
                                                (gis:envelope-of wijk-dataset)
                                                (gis:envelope-of geemente-dataset)
                                                (gis:envelope-of buurten-dataset)
                                                )


  ;scale the GIS geographic unit to netlogo unit for radius social network
  let world-envelope gis:envelope-of geemente-dataset
  let gis-width (item 1 world-envelope - item 0 world-envelope)
  let gis-height (item 3 world-envelope - item 2 world-envelope)


  set factor max list (gis-width / world-width);factor = GIS units per NetLogo unit
                       (gis-height / world-height)

  ask turtles [die] ;; remove previously loaded turtle/hh data


  set hh_fields gis:property-names bouw-dataset
  let feature-list (gis:feature-list-of bouw-dataset)


  foreach feature-list [ ?1 ->
    let hh gis:centroid-of ?1    ;location of the  households


    let prop-list []
    let this-feature ?1
    foreach hh_fields [ ??1 -> ;; iterate through all of the data that corresponsds to each household and make a list that will be handed off to agents
      let prop-field gis:property-value this-feature ??1
      set prop-list lput prop-field prop-list
    ]

    let location gis:location-of hh
    ifelse empty? location [
    ][
    create-turtles 1 [ ;; initialize an agent to represent a hh in the simulation
      setxy item 0 location item 1 location
      set shape "house"
      set color blue
      set size 1.5
      set hh_att prop-list
      ]
    ]
  ]
set use-hh 1 ;; hh are loaded so output can be saved

set n-turtles count turtles

display-wijk
display-buurt
display-geemente
end

to display-wijk
  ask wijk-labels [ die ]
  gis:set-drawing-color yellow
  gis:draw wijk-dataset 1

end

to display-buurt
  ask buurt-labels [ die ]
  gis:set-drawing-color white
  gis:draw buurten-dataset 1

  foreach gis:feature-list-of buurten-dataset

  [ ?1 -> gis:set-drawing-color scale-color green (gis:property-value ?1 "AANTAL_HH") 1400 5    ; to shade the polygone based on hh number

      gis:fill ?1 2.0 ]

end

; Drawing polygon data from a shapefile
to display-geemente
  ask geemente-labels [ die ]
  gis:set-drawing-color white
  gis:draw geemente-dataset 1

end

to setup
  clear-links
  clear-all-plots
  no-display
  set seed InputRandomSeed ;set random seed through inpout
  random-seed seed

  file-close-all
  ;file-open csv_file_name ; file to write data for disaggregation analysis
  ;csv:to-file csv_file_name (list "InputRandomSeed" "TPB_operationalization" "Financial-Information?" "MF-Income?" "Visibility?" "Info-Costs?" "Info-Costs-Revenue?" "Info-Costs-Income?" "Information-threshold?" "information_distribution" "information_threshold" "Income" "initial-pv-share" "probability_financial_information" "cost_time" "pv-yes?" "SE_importance_attitude" "SE_importance_control" "w_eco" "w_env" "w_cof" "w_soc" "pv-eco" "pv-env" "pv-cof" "pv-soc" "pv-inc")
 ; file-print csv:to-row
ask turtles [
;set pv-no and pv-yes for all utility functions
  set pv-no 0
  set pv-yes 1
  set pv-pp [0 1]           ; payback period
  set pv-emissions [0 1]    ; CO2 emission
  set pv-saving [0 1]       ;related to the money saving
  set pv-balance [0 1]      ; related to the balance of generation and avoided cost
  set pv-comfort [0 1]      ; related to the comfort


  ifelse initial-pv-share > random-float 1 [set pv-init 1 ]
     [set pv-init 0]
  set esystem (list pv-init 0 0 0 0) ; esystems: [pv floor wall roof window] -- initially some of the hh have installed the technologies, here we deal only with pv

  set avg_inc 5.5300 ;Average income of all hh (N-bar) euro = 55300 / 10000 - the income divided by 10000 to reduce the calculation load/or range
  ifelse income = "heterogenous"
  [set hh_inc (item 23 hh_att) / 10000] ; household income in 10,000 EUR]
  [set hh_inc 5.53]

; define households income group for the small world link
set income-class  item 38 hh_att ; if income is <= 30,000 set the income-class =  1.0, if income > 30,000 and <45,000 2, and if income >= 45000 3
    if hh_inc >= 3.0 and hh_inc < 4.5 [
    set income-class 2]

    if hh_inc >= 4.5[
    set income-class 3]

    if hh_inc < 3.0 [
    set income-class 1]

;define variables for PV emission savings/installation costs
  set hh_area (item 17 hh_att)  ; household area in m2 -
  set roof-size hh_area * 0.5 ; the roof size assumed to be half of the house area - this is particularly true for houses with two floors
  set avg-hh-size 162 ; an average hh size from the surveyed hh in WoON 2009 in m2
  set hh_area_ratio hh_area / avg-hh-size ; the ratio of each hh as compared to the average - used to calculate the gas saving


;define weights per household
ifelse weight_distribution = "heterogenous"[ ;choice: use weights with slider or set weights using survey, 11 sets of weights
; set weights based on survey Tariku (2014), 11 participants -> 11 different weight sets, each agent gets one of these randomly assigned as initial weight

 ;input using survey
  set var-weights matrix:from-row-list [[0.286 0.286 0.071 0.356][0.294 0.294 0.235 0.176][0.143 0.286 0.214 0.357][0.308 0.308 0.231 0.154][0.231 0.385 0.077 0.308][0.267 0.334 0.134 0.267][0.214 0.357 0.143 0.286][0.357 0.286 0.071 0.286][0.223 0.334 0.445 0][0.250 0.417 0.167 0.167][0.223 0.334 0.445 0]] ; [w_eco w_env w_soc w_cof]
  let random_w random 11
  let hh_weight matrix:get-row var-weights random_w

  set w_eco item 0 hh_weight ; weight for the economic utilities, payback period and potentially also income
  set w_env item 1 hh_weight ; weight for the environmental utility
  set w_soc item 2 hh_weight ; weight for the social utility
  set w_cof item 3 hh_weight ; weight for the comfort utility
]
 ; input using the slider
[
  let weight_total (weight_eco + weight_env + weight_cof + weight_soc)
  set w_eco  ifelse-value (weight_total != 0) [weight_eco / weight_total] [ 0 ]     ; weight for the payback period
  set w_env  ifelse-value (weight_total != 0) [weight_env / weight_total] [ 0 ]     ; weight for the environmental utility
  set w_soc  ifelse-value (weight_total != 0) [weight_soc / weight_total] [ 0 ]     ; weight for the social utility
  set w_cof  ifelse-value (weight_total != 0) [weight_cof / weight_total] [ 0 ]     ; weight for the comfort utility

  ]

  ;Annual average CO2 emission reduction over the 20 year (in 1000 tonne) - values calculated based on th WoON2009 household survey and some assumption on CO2 saving per unit of installation
  set pv_avg_co2  0.6759

  ;;define variables for calculating balances, avoided cost, generation cost, co2 saving, money saving

  set pv-cost-per-m2 2000 ; price in euro/m2 -- peak power = 1 kwp/m2 and PV price 2000 euro/kwp
  set pv-peak-power 1 ; this is max power in kw/m2
  set sunshine-hour 1000 ; the total sunshine hour in the netherlands
  set performance-ratio 0.75 ; this is the overall perfomance efficiency of the pv system
  set pv-lifetime 20 ; assumed life time of PV in years
  set grid-electricity-costs 0.19 ; this is the electricity price in euro/kwh in year 2012 based on CBS data


  ;avoided cost by installing
  set avoided-cost-per-kwh grid-electricity-costs   ;the avoided cost by generating electricity from PV - it is equal to the grid electricity price

  ;average avoided co2
  set pv-co2-per-kwh 0.56                           ; average avoided co2 in kg/kwh


  ;electricity generated per year for each household and technology
  set pv-output pv-peak-power * sunshine-hour * performance-ratio * roof-size    ; electricity generated in kWh/y

  ;total investment cost in euro
  set total-pv-cost pv-cost-per-m2 * roof-size                                   ; pv investment cost in euro

  ;the CO2 emission reduction per household over 20 years in 1000 tonne - to reduce the calc load of exponent
  set pv_co2  (pv-output * pv-co2-per-kwh) * 20 / 1000000

  ;calculate payback for PV
  set cashflows-pv [ ]
  set cumulative-pv [ ]
  let years1 0
  set total-pv-revenue 0.0

  while [years1 < pv-lifetime] [
    let dr 1 / (1 + interest_rate) ^ years1  ; discount rate - yearly revenue discount
    set cashflows-pv lput (pv-output * (grid-electricity-costs + pv_SDE_premium) * dr) cashflows-pv
        set years1 years1 + 1

    foreach cashflows-pv [ ?1 ->
      set total-pv-revenue total-pv-revenue + ?1
      if total-pv-revenue < total-pv-cost [
         set cumulative-pv lput total-pv-revenue cumulative-pv
      ]
    ]
  ]

set tot_rev sum cashflows-pv
set pv-money-saving tot_rev - total-pv-cost
;define the social network
;;links based on income class and geographical proximity
repeat close_links
   [
      let y other turtles in-radius (10000 / factor )  ; setting the radius of social networking within 10 kilo meter (10000 meter).
                                                       ; The factor is derived earlier to sacle Netlogo unit to real world unit
      let targets y with [not link-neighbor? myself]
      let target one-of targets with [income-class = [income-class] of myself]
      if (target != nobody)
      [
      create-link-with target
      ]
    ]
;;  random links
 let random_n random-float 1
  if random_n <= random_links
  [
  repeat round (random_links * (count turtles)) [
    ask one-of turtles [
      ask one-of my-links [die]
      let targets other turtles with [not link-neighbor? myself]

     create-link-with one-of targets
     let x min-one-of targets in-radius (10000 / factor) [distance myself]
      if (x != nobody) [
      create-link-with x ]
    ]
  ]
]
]
 ask turtles with [count link-neighbors = 0] [create-link-with one-of other turtles] ; lone turtles create a random link

reset-ticks
end

;functions

to-report gr-one [x] ;tool to address specific items in a list
  if x > 1 [set x 1]
  report x
end

;functions for calculating utilities

; comfort utility - like the aesthetics = 1, else = -1
to-report pv-util-cof [x]

  let pv-dis-cof random 2
  ifelse pv-dis-cof = 1[
    set pv-cof 1
  ]
  [
    set pv-cof -1
  ]
  set pv-cof-util (pv-cof * item x pv-comfort)
  report pv-cof-util
end

; economic payback utility
to-report pv-util-eco-time [x]
ifelse Uncertainty?
  ;calculation of economic utility if Uncertainty? == True
[set t1 length cumulative-pv
   set B1 total-pv-cost - item (t1 - 2) cumulative-pv
   set C1 item (t1 - 1) cumulative-pv  - item (t1 - 2) cumulative-pv

   set pp_pv (precision (t1 + (B1 / C1)) 2) ;payback period of a PV
   set pv-eco-util-prep (21 - (pp_pv) * item x pv-pp) / 21 ; calculation economic utility

    ;calculation of uncertainaty level for different information distributions
        if information_distribution = "uniform" [set unc_level random-float (pv-eco-util-prep * 2)]
        if information_distribution = "normal" [set unc_level random-normal (pv-eco-util-prep) 0.1]
        if information_distribution = "poisson" [set unc_level (random-poisson (pv-eco-util-prep * 100)) / 100]
        if information_distribution = "empirical" [let random_5 random-float 1
if random_5 <= 0.18  ;  1-2 ; assume 50% time used for financial information --> see Rai and MacAndrews, Rai, Reeves and Margolis
[set unc_level 1 / 13]
if random_5 > 18 and random_5 <= 0.42  ;3-4
[set unc_level 2 / 13]
if random_5 > 0.42 and random_5 <= 0.67;  5-6
[set unc_level 3 / 13]
if random_5 > 0.67 and random_5 <= 0.69;  7-8
[set unc_level 4 / 13]
if random_5 > 0.69 and random_5 <= 0.73 ;9-10
[set unc_level 5 / 13]
if random_5 > 0.73 and random_5 <= 0.86 ;11-12
[set unc_level 6 / 13]
if random_5 > 0.86 and random_5 <= 0.89 ;17-18
[set unc_level 9 / 13]
if random_5 > 0.89 and random_5 <= 0.96 ;23-24
[set unc_level 12 / 13]
if random_5 > 0.96 and random_5 <= 1 ;25 +
[set unc_level 13 / 13]
set unc_level ((unc_level * pv-eco-util-prep * 2))] ;diff sizes here

        set unc_level_normalised unc_level - pv-eco-util-prep
        ifelse TPB_operationalization = "RR" [set pv-eco-util pv-eco-util-prep - unc_level_normalised]; in case of the RR model the economic utility also feeds into the PBC barrier
        [set pv-eco-util unc_level]; set updated economic utility for MF and SE modules

   ]
 ;calculation of economic utility if Info-Costs? == True
[  ifelse Info-Costs?

  [ let random_hyp4 random-float 1
    ifelse random_hyp4 < probability_financial_information ; implementation probability barrier, full information for some agents
      [set cost_time 0
        set prob_info_eco  true]
        ;calculation of economic utility for all other agents and different information distributions
      [if information_distribution = "uniform" [set cost_time random-float 2]
        if information_distribution = "normal" [set cost_time random-normal 1 0.1]
        if information_distribution = "poisson" [set cost_time ((random-poisson 100) / 100)]
        if information_distribution = "empirical" [let random_5 random-float 2
          if random_5 <= 0.18  ;  1-2 ; assume 50% time used for financial information --> see Rai and MacAndrews, Rai, Reeves and Margolis
          [set cost_time 1 / 13]
          if random_5 > 18 and random_5 <= 0.42  ;3-4
          [set cost_time 2 / 13]
          if random_5 > 0.42 and random_5 <= 0.67;  5-6
          [set cost_time 3 / 13]
          if random_5 > 0.67 and random_5 <= 0.69;  7-8
          [set cost_time 4 / 13]
          if random_5 > 0.69 and random_5 <= 0.73 ;9-10
          [set cost_time 5 / 13]
          if random_5 > 0.73 and random_5 <= 0.86 ;11-12
          [set cost_time 6 / 13]
          if random_5 > 0.86 and random_5 <= 0.89 ;17-18
          [set cost_time 9 / 13]
          if random_5 > 0.89 and random_5 <= 0.96 ;23-24
          [set cost_time 12 / 13]
          if random_5 > 0.96 and random_5 <= 1 ;25 +
          [set cost_time 13 / 13]]

        ]
      ifelse TPB_operationalization = "RR"[
        ; again separate calculation of total PV costs for RR, since it is included in the PBC barrier
              ifelse Info-Costs-Revenue?
        [set total-pv-cost-fin total-pv-cost + influence_cost_time *  cost_time * tot_rev / (12 * 21)];including potential monthly revenue of solar cells in payback  calculation
      [set total-pv-cost-fin total-pv-cost]
      ifelse Info-Costs-Income?
      [set total-pv-cost-fin total-pv-cost + influence_cost_time * ((tot_rev / 21) + (hh_inc * 10000)) / 12 * cost_time];including hh's monthly income in payback calculation
      [set total-pv-cost-fin total-pv-cost + influence_cost_time * cost_time * tot_rev / (12 * 21) ]
      ][
         ; calculation economic utility for SE and MF
      ifelse Info-Costs-Revenue?
      [set total-pv-cost-fin total-pv-cost +  influence_cost_time * cost_time * tot_rev / (12 * 21)];including potential monthly revenue of solar cells in payback  calculation

      [set total-pv-cost-fin total-pv-cost]
      ifelse Info-Costs-Income?
      [set total-pv-cost-fin total-pv-cost + influence_cost_time * ((tot_rev / 21) + (hh_inc * 10000)) / 12 * cost_time];including hh's monthly income in payback calculation
      [set total-pv-cost-fin total-pv-cost + influence_cost_time * cost_time * tot_rev / (12 * 21)]; wo 0.3
      ]
      ;calculate economic utility based on updated total PV costs
  set t1 length cumulative-pv
    set B1 total-pv-cost-fin - item (t1 - 2) cumulative-pv
    set C1 item (t1 - 1) cumulative-pv  - item (t1 - 2) cumulative-pv

    set pp_pv (precision (t1 + (B1 / C1)) 2) ;payback period of a PV

   set pv-eco-util (21 - (pp_pv) * item x pv-pp) / 21

  ]

;calculation of economic utility if Uncertainty? == False and Info-Costs == False
[  set t1 length cumulative-pv
   set B1 total-pv-cost - item (t1 - 2) cumulative-pv
   set C1 item (t1 - 1) cumulative-pv  - item (t1 - 2) cumulative-pv

   set pp_pv (precision (t1 + (B1 / C1)) 2) ;payback period of a PV

    set pv-eco-util (21 - pp_pv * item x pv-pp) / 21

]]
  report pv-eco-util
end

;to-report pv-util-eco-bar [x]
;
;  ifelse Info-Costs?
;  [
;  ifelse Info-Costs-Revenue?
;      [set total-pv-cost-fin-bar total-pv-cost - influence_cost_time *  cost_time * tot_rev / (12 * 21)]
;      [set total-pv-cost-fin-bar total-pv-cost]
;      ifelse Info-Costs-Income?
;      [set total-pv-cost-fin-bar total-pv-cost - influence_cost_time * ((tot_rev / 21) + (hh_inc * 10000)) / 12 * cost_time]
;      [set total-pv-cost-fin-bar total-pv-cost - influence_cost_time *  cost_time * tot_rev / (12 * 21)]
;  ][set total-pv-cost-fin total-pv-cost-fin]
;
;  set t1 length cumulative-pv
;    set B1 total-pv-cost-fin-bar - item (t1 - 2) cumulative-pv
;    set C1 item (t1 - 1) cumulative-pv  - item (t1 - 2) cumulative-pv
;
;    set pp_pv (precision (t1 + (B1 / C1)) 2) ;payback period of a PV
;
;   set pv-eco-util-bar (21 - (pp_pv) * item x pv-pp) / 21
;
;  if Uncertainty? = TRUE [set pv-eco-util-bar pv-eco-util-bar - unc_level_normalised]
;
;  report pv-eco-util-bar
;end


;environmental utility
to-report pv-util-env [x]
  set pv-env-util ((exp(pv_co2 * item x pv-emissions - pv_avg_co2)) / (1 + (exp(pv_co2 * item x pv-emissions - pv_avg_co2))))
  report pv-env-util
end

;social utility
to-report pv-util-soc [x]
  set pv-soc-util ifelse-value (n-of-neighbors != 0) [(count link-neighbors with [item 0 esystem = x]) / n-of-neighbors] [ 0 ] ; ifelse used to avoid division by zero
report pv-soc-util
end

;economic income (utility /factor - dependent on TPB operationalization)
to-report pv-util-inc [x]
  let normal 600 *(553)^ (-1)      ;normalization component to use sigmoid function for probability - align mean value probability with average income
  set pv-inc-util 1 * ((1 + exp((-1) *(normal * hh_inc) + 6 )))^ (-1) ; sigmoid function, input household income and "normal", which puts the household income in relation to the average income
  report pv-inc-util
end

;Functions to calculate barriers
to-report test-visibility-pv
  ifelse Visibility?[
     let con 0.33        ; technology specific parameter reflecting the effect of market size or confidence in the market
     let VT random-float 1
     ifelse Sparking-Events? [
       set Vj max(list Vj min(list 1 (Ajadv + Ajsoc + (pv-share) ^ con)))         ; visibility at t >  0
       ]
       [
       set Vj max(list Vj min(list 1 (Ajadv + (pv-share) ^ con)))         ; visibility at t >  0
       ]
    ifelse VT < Vj[
      set vis_test 1
    ]
    [
      set vis_test 0
    ]
  ]
  [set vis_test 1
  ]
  report vis_test
end

;probabilistic income barrier for MF
to-report test-income-pv
  ifelse MF-Income?[
    let normal 600 *(553)^ (-1)  ;normalization component to use sigmoid function for probability - align mean value probability with average income

    set probability_inc 1 * ((1 + exp((-1) *(normal * hh_inc) + 6 )))^ (-1) ; sigmoid function, input household income and "normal", which puts the household income in relation to the average income
      ifelse MF-Income-Barrier?
    [set thres_prob MF_income_barrier]
    [set thres_prob random-float 1]
    ifelse probability_inc > thres_prob [
      set inc_test 1
    ][
      set inc_test 0
    ]
  ]
  [set inc_test 1
  ]
  report inc_test
end

;Functions for choose-esystems
;choosing function for MF
to choose-pv-MF
   if item 0 esystem != 1 [
  let visibility-test test-visibility-pv; check for inclusion of visibility
  let income-test test-income-pv; check for inclusion of income
  if visibility-test = 1 and income-test = 1 [

    set pv-eco-no pv-util-eco-time pv-no
    set pv-eco-yes pv-util-eco-time pv-yes

    set pv-env-no pv-util-env pv-no
    set pv-env-yes pv-util-env pv-yes

    set pv-soc-no pv-util-soc pv-no
    set pv-soc-yes pv-util-soc pv-yes

    set pv-cof-no pv-util-cof pv-no
    set pv-cof-yes pv-util-cof pv-yes
; calculate multi-attribute utility function pro and conse PV installation
    let utility-no (w_eco * pv-eco-no + w_env * pv-env-no + w_cof * pv-cof-no + w_soc * pv-soc-no)
    let utility-yes (w_eco * pv-eco-yes + w_env * pv-env-yes + w_cof * pv-cof-yes + w_soc * pv-soc-yes)
    ifelse Information-Threshold? ; check if information cutoff is implemented
   [
     ifelse information_threshold > cost_time
        [if prob_info_eco != true [set esystem replace-item 0 esystem utility-no]
         if prob_info_eco = true [let utility (list (utility-yes) (utility-no))
              set esystem replace-item 0 esystem (position (max utility) utility)]]
        [let utility (list (utility-yes) (utility-no))
              set esystem replace-item 0 esystem (position (max utility) utility)]
   ]
    [let utility (list (utility-yes) (utility-no))
          set esystem replace-item 0 esystem (position (max utility) utility)
    ]
  ]
   ]
end

;choosing function for RR
to choose-pv-RR
   if item 0 esystem != 1 [

    set pv-eco-no pv-util-eco-time pv-no
    set pv-eco-yes pv-util-eco-time pv-yes

    set pv-eco-no pv-util-eco-time pv-no
    set pv-eco-yes pv-util-eco-time pv-yes

    set pv-env-no pv-util-env pv-no
    set pv-env-yes pv-util-env pv-yes

    set pv-soc-no pv-util-soc pv-no
    set pv-soc-yes pv-util-soc pv-yes

    set pv-cof-no pv-util-cof pv-no
    set pv-cof-yes pv-util-cof pv-yes

    set pv-inc-no pv-util-inc pv-no
    set pv-inc-yes pv-util-inc pv-yes
    ;PBC barrier RR
    if pv-eco-yes < pv-inc-yes [
    ; multi-attribute utility function RR
    set utility-RR (1 / 4 ) * (w_eco * pv-eco-yes + w_env * pv-env-yes + w_cof * pv-cof-yes + w_soc * pv-soc-yes)

ifelse Information-Threshold?; check if information cutoff is implemented
   [
     ifelse information_threshold > cost_time
     [if prob_info_eco != true [set esystem replace-item 0 esystem 0]
      if prob_info_eco = true [ifelse utility-RR > RR_sia
         [ set esystem replace-item 0 esystem 1]
         [ set esystem replace-item 0 esystem 0]
         ]
     ]
     [ifelse utility-RR > RR_sia
         [ set esystem replace-item 0 esystem 1]
         [ set esystem replace-item 0 esystem 0]]
   ]
     [

    ifelse utility-RR > RR_sia
         [ set esystem replace-item 0 esystem 1]
         [ set esystem replace-item 0 esystem 0]
    ]
   ]
   ]
end
;choosing function for SE
to choose-pv-SE

    if item 0 esystem != 1 [
    set pv-eco-no pv-util-eco-time pv-no
    set pv-eco-yes pv-util-eco-time pv-yes

    set pv-env-no pv-util-env pv-no
    set pv-env-yes pv-util-env pv-yes

    set pv-soc-no pv-util-soc pv-no
    set pv-soc-yes pv-util-soc pv-yes

    set pv-cof-no pv-util-cof pv-no
    set pv-cof-yes pv-util-cof pv-yes

    set pv-inc-no pv-util-inc pv-no
    set pv-inc-yes pv-util-inc pv-yes

   ;multi-attribute utility function SE, including external technology related parameters
set imp_attitude-yes SE_importance_attitude
set attitude-yes (w_eco * pv-eco-yes + w_env * pv-env-yes + w_cof * pv-cof-yes)
set control-yes (w_eco * pv-inc-no)
set imp_control-yes SE_importance_control
set imp_social-yes w_soc
set social-yes pv-soc-yes

set imp_attitude-no SE_importance_attitude
set attitude-no (w_eco * pv-eco-no + w_env * pv-env-no + w_cof * pv-cof-no)
set control-no (w_eco * pv-inc-no)
set imp_control-no SE_importance_control
set imp_social-no w_soc
set social-no pv-soc-no

let utility-no ((imp_attitude-no * attitude-no + control-no * imp_control-no) * (1 - imp_social-no) + social-no * imp_social-no)
let utility-yes ((imp_attitude-yes * attitude-yes + control-yes * imp_control-yes) * (1 - imp_social-yes) + social-yes * imp_social-yes)

ifelse Information-Threshold?; check if information cutoff is implemented
   [
     ifelse information_threshold > cost_time
     [if prob_info_eco != true [set esystem replace-item 0 esystem utility-no]
      if prob_info_eco = true [let utility (list (utility-yes) (utility-no))
              set esystem replace-item 0 esystem (position (max utility) utility)]]
        [let utility (list (utility-yes) (utility-no))
              set esystem replace-item 0 esystem (position (max utility) utility)]
   ]
    [let utility (list (utility-yes) (utility-no))
          set esystem replace-item 0 esystem (position (max utility) utility)
    ]
  ]
end

;simulation setup
to go
if ticks = 30 [stop]

ask turtles [

  set n-of-neighbors count link-neighbors

 if TPB_operationalization = "MF" [choose-pv-MF]
 if TPB_operationalization = "RR" [choose-pv-RR]
 if TPB_operationalization = "SE" [choose-pv-SE]

  ifelse item 0 esystem = 1
  [set pv-yes? true][set pv-yes? false]

  if ticks = 29 [

  ifelse item 0 esystem = 1
   [set disaggregate (list InputRandomSeed TPB_operationalization Financial-Information? MF-Income? Visibility? Info-Costs? Info-Costs-Revenue? Info-Costs-Income? Information-Threshold? information_distribution information_threshold Income initial-pv-share probability_financial_information cost_time pv-yes? SE_importance_attitude SE_importance_control w_eco w_env w_cof w_soc pv-eco-no pv-env-no pv-cof-no pv-soc-no pv-inc-no)
  ]
  [set disaggregate (list InputRandomSeed TPB_operationalization Financial-Information? MF-Income? Visibility? Info-Costs? Info-Costs-Revenue? Info-Costs-Income? Information-Threshold? information_distribution information_threshold Income initial-pv-share probability_financial_information cost_time pv-yes? SE_importance_attitude SE_importance_control w_eco w_env w_cof w_soc pv-eco-yes pv-env-yes pv-cof-yes pv-soc-yes pv-inc-yes)

  ];csv:to-file csv_file_name disaggregate;file-print csv:to-row disaggregate; save disaggregated outcomes to file

    ]


  ]

set pv-share ifelse-value (n-turtles != 0) [count turtles with [pv-yes? = true] / n-turtles] [ 0 ]

tick

end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
448
-1
-1
13.0
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
0
0
1
ticks
30.0

INPUTBOX
16
176
171
236
InputRandomSeed
-1.343890055E9
1
0
Number

SLIDER
16
290
188
323
interest_rate
interest_rate
0
0.1
0.0
0.005
1
NIL
HORIZONTAL

SLIDER
16
328
188
361
pv_SDE_premium
pv_SDE_premium
0
0.3
0.0
0.05
1
NIL
HORIZONTAL

CHOOSER
14
514
158
559
weight_distribution
weight_distribution
"heterogenous" "homogenous"
0

SLIDER
174
487
317
520
weight_eco
weight_eco
0
1
0.251
.001
1
NIL
HORIZONTAL

SLIDER
175
525
315
558
weight_env
weight_env
0
1
0.3
0.001
1
NIL
HORIZONTAL

SLIDER
330
487
470
520
weight_cof
weight_cof
0
1
0.181
0.001
1
NIL
HORIZONTAL

SLIDER
331
527
470
560
weight_soc
weight_soc
0
1
0.161
0.001
1
NIL
HORIZONTAL

SWITCH
483
485
631
518
Visibility?
Visibility?
0
1
-1000

SWITCH
484
527
631
560
Sparking-Events?
Sparking-Events?
1
1
-1000

SWITCH
668
74
847
107
MF-Income?
MF-Income?
0
1
-1000

BUTTON
14
17
102
50
load data
load-data
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
14
59
80
92
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
0

BUTTON
111
18
177
51
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

SLIDER
15
378
187
411
close_links
close_links
0
5
5.0
1
1
NIL
HORIZONTAL

SLIDER
15
417
187
450
random_links
random_links
0
0.1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
16
251
188
284
initial-pv-share
initial-pv-share
0
1
0.09
.01
1
NIL
HORIZONTAL

SWITCH
928
278
1099
311
Financial-Information?
Financial-Information?
0
1
-1000

CHOOSER
645
460
783
505
Ajadv
Ajadv
0 0.02
1

CHOOSER
646
516
784
561
Ajsoc
Ajsoc
0 0.02
1

SWITCH
929
51
1098
84
Info-Costs?
Info-Costs?
0
1
-1000

SWITCH
929
136
1098
169
Info-Costs-Revenue?
Info-Costs-Revenue?
1
1
-1000

SLIDER
927
316
1100
349
probability_financial_information
probability_financial_information
0
1
1.0
0.05
1
NIL
HORIZONTAL

INPUTBOX
16
109
170
169
csv_file_name
/Users/Desktop/1803_se_mf_over_cost_04.csv
1
0
String

SWITCH
929
174
1098
207
Info-Costs-Income?
Info-Costs-Income?
0
1
-1000

CHOOSER
15
460
153
505
income
income
"heterogenous" "homogenous"
0

SLIDER
928
407
1101
440
information_threshold
information_threshold
0
1
0.0
0.01
1
NIL
HORIZONTAL

CHOOSER
928
225
1099
270
information_distribution
information_distribution
"uniform" "normal" "poisson" "empirical"
3

SWITCH
928
370
1100
403
Information-Threshold?
Information-Threshold?
1
1
-1000

SWITCH
928
12
1098
45
Uncertainty?
Uncertainty?
1
1
-1000

CHOOSER
667
13
846
58
TPB_operationalization
TPB_operationalization
"MF" "RR" "SE"
2

SLIDER
669
201
849
234
SE_importance_control
SE_importance_control
0
1
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
669
240
849
273
SE_importance_attitude
SE_importance_attitude
0
1
0.97
0.01
1
NIL
HORIZONTAL

SLIDER
669
327
850
360
RR_sia
RR_sia
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
669
288
849
321
RR_sensitivity_barrier
RR_sensitivity_barrier
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
669
152
848
185
MF_income_barrier
MF_income_barrier
0
1
0.01
0.01
1
NIL
HORIZONTAL

SWITCH
668
113
848
146
MF-Income-Barrier?
MF-Income-Barrier?
0
1
-1000

SLIDER
929
99
1098
132
influence_cost_time
influence_cost_time
0
1
1.0
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
One theory - many formalisations testing different code implementations of the Theory of Planned Behaviour in energy agent-based models

Authors: Muelder, Hannah; Filatova, Tatiana
Contact: h.m.mulder-1@alumnus.utwente.nl

Instructions: Choose the variables for the run, including the csv file to be written with results of the model, then press "load data", then "setup" and lastly "go".Alternatively, use Behavior Space to define a set of different simulation runs. 

Data: The data is currently not available due to privacy reasons

Variables:

Globals

- InputRandomSeed: Random seed for all stochastic variables in the model - variables summarised in the ODD description.
- initial_pv_share: The percentage of households who have already installed PV at the start of the simulation.
- interest_rate: interest rate on the PV investment
- pv_SDE_premium: Subsidy by the government to be used for the financing the PV panels.

Procedural

- Visibility?: Switch on/off of the visibility barrier of the technology, dependent on market share and advertisement.
	- Sparking-Events?: Switch on/off for making the visibility barrier dependent on the social network as well.
	- Ajadv: The strength of influence of advertisement on the visibility.
	- Ajsoc: The strength of influence of the social network on the visibility.

- information_distribution: The probability distribution of the costs/inaccuracy of information over all households for "Uncertainty?" and "Info-Costs?"
- Information-Threshold?: Switch on/off, whether or not households who did not invest a certain amount of time in searching for information are not continuing the decision making process.
- information_threshold: The value for the threshold, mentioned above. It should be between 0 and 1.
- probability_financial_information: The percentage of households that are informed on the finances by the installer.
- Uncertainty?: Implementation of the information on financial aspects as inaccuracy of the calculation of the economic utility.
- Info-Costs?: Implementation of the information on financial aspects as an increase of the initial investment costs dependent on amount of time for information search and the
 probability for information on economic already provided by the installers
	- Info-Costs-Revenue?: Switch on/off to include the potential revenue of PV in the calculation of the costs of the time that is spent on information search.
	- Info-Costs-Income?: Switch on/off to include the household's income in the calculation of the costs of the time that is spent on information search.
	- influence_costs_time: The percentage of monthly revenue and/or monthly income that is used to calculate the costs of information of financial aspects of PV

- TPB_operationalization: A choice between our, Muelder et al.'s, Schwarz and Ernst or Robinson and Rai's operationalization of the Theory of Planned Behaviour. The specific differences are presented in the ODD description of the model
	- MF-Income?:  Includes Income as a measure of PBC as a probabilistic barrier, giving households with higher income a higher chance to consider PV then households with lower income.
	- MF-Income-Barrier?: Includes income as measure but instead of a probabilistic behavioour this one is attached to a threshold, namely MF_income_barrier.
	- MF_income_barrier: Threshold for the income barrier.
	- SE_importance_control: The importance of the control parameter in the utility function for Schwarz and Ernst operationalization of the Theory of Planned Behaviour. Value between 0 and 1.
	- SE_importance_attitude: The importance of the attitude parameter in the utility function for Schwarz and Ernst operationalization of the Theory of Planned Behaviour. Value between 0 and 1.
	- RR_sia: The threshold value against which a household's utility for the PV installation is compared. If the threshold is smaller than the utility, PV is installed.
	- RR_sensitivity_barrier: Can be included if one wants to test the PBC barrier, comparing payback to income. The code part needs to be uncommented in order to do somm though.

Characteristics agent population

- weight_distribution: The weight distribution for the households can be set to homogenous or heterogenous.
- close_links: The amount of links an agent's social network can have, that are set depending on an agent's income class.
- random_links: The probability for an agent to have a link based on the geographical distance between two agents.

Agent's attributes

- weight_eco: If the weight distribution is homogenous, the economic utility can be specified. The value can be between 0 and 1.
- weight_env: The environmental utility, a value between 0 and 1.
- weight_soc: The social utility, a value between 0 and 1.
- weight_cof: The comfort utility. Either equal to 1 or -1.
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
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="run" repetitions="1" runMetricsEveryStep="true">
    <setup>load-data
setup</setup>
    <go>go</go>
    <metric>pv-share</metric>
    <metric>count turtles with [pv-yes? = true]</metric>
    <metric>sum [pv_co2] of turtles with [pv-yes? = true]</metric>
    <metric>sum [pv-output] of turtles with [pv-yes? = true]</metric>
    <metric>sum [pv-money-saving] of turtles with [pv-yes? = true]</metric>
    <enumeratedValueSet variable="TPB_operationalization">
      <value value="&quot;MF&quot;"/>
      <value value="&quot;SE&quot;"/>
      <value value="&quot;RR&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RR_sia">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE_importance_attitude">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE_importance_control">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income">
      <value value="&quot;heterogenous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Visibility?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Financial-Information?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sparking-Events?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InputRandomSeed">
      <value value="-1895275590"/>
      <value value="-1685352683"/>
      <value value="445967620"/>
      <value value="825466499"/>
      <value value="1118274093"/>
      <value value="-1714229232"/>
      <value value="-1206706735"/>
      <value value="-1054615730"/>
      <value value="799126972"/>
      <value value="-1997981434"/>
      <value value="2117550788"/>
      <value value="-1160633981"/>
      <value value="-743811881"/>
      <value value="-1475737499"/>
      <value value="915907501"/>
      <value value="787879594"/>
      <value value="1529269517"/>
      <value value="718846246"/>
      <value value="-474134528"/>
      <value value="1396654759"/>
      <value value="-2089794397"/>
      <value value="1896224233"/>
      <value value="-149384996"/>
      <value value="-970083000"/>
      <value value="-124920090"/>
      <value value="2058435316"/>
      <value value="-771285926"/>
      <value value="-716310264"/>
      <value value="1281090411"/>
      <value value="-1250590988"/>
      <value value="638452447"/>
      <value value="-645252747"/>
      <value value="1142746496"/>
      <value value="325474521"/>
      <value value="-452846882"/>
      <value value="1634686056"/>
      <value value="-775632534"/>
      <value value="-427146965"/>
      <value value="-793465801"/>
      <value value="802484765"/>
      <value value="1004723554"/>
      <value value="-374264897"/>
      <value value="1516294754"/>
      <value value="913520472"/>
      <value value="-290462945"/>
      <value value="843478507"/>
      <value value="-148504784"/>
      <value value="1194863070"/>
      <value value="-485909916"/>
      <value value="-1343890055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="close_links">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random_links">
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight_distribution">
      <value value="&quot;heterogenous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pv_SDE_premium">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial_pv_share">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability_financial_information">
      <value value="0.447"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ajadv">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ajsoc">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Info-Costs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Info-Costs-Revenue?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Info-Costs-Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Information-Threshold?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information_distribution">
      <value value="&quot;normal&quot;"/>
      <value value="&quot;poisson&quot;"/>
      <value value="&quot;empirical&quot;"/>
      <value value="&quot;uniform&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uncertainty_threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Uncertainty?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence_cost_time">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="csv_file_name">
      <value value="&quot;run1.csv&quot;"/>
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
