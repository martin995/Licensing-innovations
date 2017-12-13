;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DEFININDO TURTLES
;Podem ser firmas, que inovam ou imitam
;Ou patentes, que são criadas por firmas inovadoras

breed[firms firm]
breed[patents patent]
breed[licenses license]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DEFININDO O QUE TURTLES POSSUEM

firms-own [
  q           ; produto da firma
  A           ; produtividade da firma
  k           ; estoque de capital da firma
  profit      ; lucro bruto da firma
  rin         ; investimento inovativo da firma
  rim         ; investimento imitativo da firma
  A_in        ; Produtividade adquirida graças a inovacao
  A_im        ; Produtividade adquirida graças a imitacao
  MS          ; market share
  X           ;performance da firma
  u_n         ;desvio padrao do rin
  error-in    ;variavel aleatoria possivel sd
  u_m         ;desvio opadrao do rim
  error-im    ;variavel aleatoria possivel sd
  time        ;tempo
  innovate?   ;dummy se firma é inovadora
  mylist
  mylist1
  min_dist_k  ;minima distancia de k
  license?    ;se ativo a firma, deseja adquirir uma licença.


]

patents-own[
  lifetime       ;tempo de vida da patente
  productivity   ;produtividade marginal da patente
  MarketPower    ; poder de mercado e concentração da firma inovadora
  capital        ; capital da firma inovadora
  exclusive?     ;identifica se a patente está em fase de exclusividade
  creator        ;identifica a firma criadora
  licensing?     ;identifica se a patente é licenciada
  permisse?      ;identifica se a licença é permitida a alguma turtle

]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DEFININDO GLOBALS

globals [
  N ;numero total de firmas
  price; preco de mercado
  Qt;quantidade total ofertada
  Kt;quantidade total capital

  ;indicadores
  Hk;indice de concentracao
  HQ; indice de concentracao
  A_e;produtiviade da entrante
  ave_a; produtividade media
  max_a; maxima produtividade
  min_a;minima produtividad
  freeA;maxima produtividade disponivel
  freeL; Produtividade disponivel para licenciamento
  max_profit; maximo lucro
  ave_profit;lucro medio
  ave_k;capital medio
  max_k;maximo capital
  min_k;minimo capital
  ave_X
  sum_q;soma produto
  meanRim;politica media
  meanRin;politica media
  nbImit;numero de entrantes imitadoras
  nbInnov;numero de entrantes inovadoras
  invd;investimento desejado
  invp;investimento possivel
  state;estado da patente
  patent-life;tempo da patente
  patent-ON;lista de inovacoes com patentes
  patent-OFF;lista de inovacoes sem patentes
  patent-Lic;lista de inovacoes patenteadas com licenciamento
  pl          ;preco de licencimento
  k_licenses;lista de  capitais de inovadoras licenciadoras
  CS; excedente do consumidor
  FS; excedente do produtor
  SocW; bem estar social
  max_a_disponivel ;Maior produditividade disponível para imitar
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP DA SIMULAÇÃO


to setup
  clear-all

  setting-initial-state

  reset-ticks
end

to setting-initial-state

  let condi 0

  while [condi = 0] [

     let mi (0.5 * (A0 + alpha + 0.15))
     let sd  0.0589
     let zeta ln (1 + (sd ^ 2)/ (mi ^ 2))
     let M ln (mi) - zeta / 2
     let S sqrt zeta


    set A_e  exp(random-normal M S)  ;definindo produtividade inicial
    let u (random-normal 0 0.014)  ;definindo função erro

    if 1.2 * A_e - c > 0.06 + u [   ;condição de entrada

      create-firms nFounder [   ;criando nFounder firms

        setxy random-pxcor * .8 random-pycor * .8
        set color gray
        set size 2
        set shape "circle"
        set label-color white

        set rim 0.002
        set rin 0.005
        set k 25        ;definindo Capital Inicial (o $, não a banda)
        set A A_e
        set profit 1.2 * A - c - rin - rim

        set innovate? true

        ;Funções erro
        set error-in (random-normal 0 0.002)
        ifelse error-in > 0 [set u_n error-in][set u_n 0]

        set X 0  ;desempenho inicial da firma
        set time 0
        set label time
      ]

      set condi 1  ; Isso aqui pode definir fora da função create porque é uma variável global

    ]

  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GO DA SIMULAÇÃO

to go

  ; A função Go chama 4 sub-funções
  ; (1) update das firmas e patentes presentes
  ; (2) nova configuração do mercado
  ; (3) estatísticas
  ; (4) contagem e parada de ticks

  market
  statistics
  update
  ent

  if (any? firms) [ask firms [
    set time time + 1
    set label time
  ]]
  if (any? patents) [ask patents [
    set lifetime lifetime + 1
    set label lifetime
  ]]


  tick
  if (ticks = number-of-periods) [stop]


end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to market

  ; A sub-função market chama 5 rotinas

  production ; calcula produção de cada firma
  concentration ; calcula a concentração dos mercados
  pricing ;calcula o novo preço de equilíbrio
  market-share


end

to production

  ask firms [

   set q A * k

  ]

end

to concentration

  set Qt sum[q] of firms ;Soma total da produção das firmas
  set Kt sum[k] of firms ;Soma total do capital das firmas

  set HK (Kt) ^ 2 / sum [k ^ 2] of firms
  set HQ (Qt) ^ 2 / sum [q ^ 2] of firms


end

to pricing

  if (Qt < 53.33) [set price 1.20]
  if (Qt > 53.33) [set price 64 / (Qt ^ ETA) ]


end

to market-share

  ask firms [

    set MS (q / Qt)

    set profit (price * A) - c - rin - rim

    if (any? links with[color = pink])[set profit profit - pl]
    let myA A
    if (any? links with [color = red])and(any? other firms with [A = myA] )[set profit profit + pl]

    set X ( 0.75 * X  + 0.25 * profit)  ; indicador de performance.

  ]





end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UPDATE

to update

  update-patents
  list-patents



  ask firms [
    from-im-to-in ; Transforma imitadores em inovadores
    rnd-investments
    acq-prod
    kill-firms

    ifelse (innovate? = true) [set color gray][set color blue]

  ]

  if (patents-die = true) [kill-patents]


end

to from-im-to-in


  ask firms [
   ; show (X - ave_profit)
    if (innovate? = false) and (X < ave_profit) and (random-float 1. < 0.5) [set innovate? true set X X + (ave_X - X)]
    if (innovate? = true) and (X < ave_profit) and (random-float 1. < 0.5) [set innovate? false set X X + ( ave_X - X)]

  ]

end

to rnd-investments

  set error-im (random-normal 0 0.0004)
  set error-in (random-normal 0 0.002)

  ifelse (innovate? = false) [policy-im][policy-in] ; Chama as políticas de inovação ou imitação


end

to policy-im ;Função política dos imitadores

  ifelse (X >= ave_profit)
  [set rin 0]
  [
    ifelse (error-im < 0) [set u_m 0][set u_m error-im]
    set meanRim mean [rim] of firms
    set rim ( 1 - beta ) * rim + ( beta * meanRim ) + u_m
    set rin 0
  ]

end

to policy-in ;Função política dos inovadores

  if (X < ave_profit)
  [
    set meanRin mean[rin] of firms
    set meanRim mean [rim] of firms

    ifelse (error-in < 0) [set u_n 0][set u_n error-in]
    set rin ( 1 - beta ) * rin + ( beta * meanRin ) + u_n

    ifelse (error-im < 0) [set u_m 0][set u_m error-im]
    set rim ( 1 - beta ) * rim + ( beta * meanRim ) + u_m

  ]

end

to acq-prod

  set max_a max[A] of firms ;Produtividade máxima da indústria
  set ave_a mean[A] of firms ;Produtividade média da indústria
  set min_a min[A] of firms ;Produtividade mínima da indústria
  set mylist n-values(length k_licenses)[k]
  set mylist1 (map - mylist k_licenses)
  ifelse (empty? mylist1) or (mylist1 = [0])[set min_dist_k 1][set min_dist_k sqrt((min(mylist1)) ^ 2 )]
  ifelse (empty? patent-OFF) [set max_a_disponivel 0 ] [set max_a_disponivel max patent-OFF]

 ifelse innovate? = true
[
  let draw1 random-float 1.
  let draw2 random-float 1.
  let draw4 random-float 1.

  let mi (0.5 * (A0 + (alpha ^ time)) + 0.5 * A)
  let sd  0.0589
  let zeta ln (1 + (sd ^ 2)/ (mi ^ 2))
  let M ln (mi) - zeta / 2
  let S sqrt zeta

  ifelse (draw1 <= rin * k * 0.25) [set A_in exp(random-normal M S)][set A_in 0] ;função inovar
  ifelse (draw2 <= rim * k * 2.5) [set A_im max_a_disponivel ][set A_im 0] ;função imitar
 ifelse (empty? patent-Lic) [set freeL 0] [ ifelse(draw4 <= (1 - MS) ) and (draw4 <= 1 / min_dist_k)[set license? true set freeL max patent-Lic][set freeL 0]];função adquirir licensa



]
[
  let draw2 random-float 1.
  let draw4 random-float 1.
  set A_in 0
  ifelse (draw2 <= rim * k * 2.5) [set A_im max_a_disponivel ][set A_im 0] ;função imitar
  ifelse (empty? patent-Lic) [set freeL 0] [ ifelse(draw4 <= (1 - MS))and (draw4 <= 1 / min_dist_k)[ set freeL max patent-Lic ][set freeL 0]];função adquirir licensa

]

  let firmA A_in
  let k_2 k
  let myfirm who
  let Mp MS

  if (A_in > 0) [

    hatch-patents 1 [       ; Criando nova patente
      set shape "square"
      set color red
      set size 2
      set lifetime 1
      set label lifetime
      set exclusive? true
      set licensing? false
      set permisse? false
      set productivity firmA
      set capital k_2
      set MarketPower Mp
      set creator myfirm
      setxy random-pxcor * .8 random-pycor * .8

      create-link-with firm myfirm [set color red]

    ]
    licensing

  ]
  set pl ( freeL - max_a_disponivel )

  set A max (list A A_in A_im freeL)
  let myA A

  if (any? patents) [if (A = A_im) and ( A = [productivity]of patents) [create-link-with one-of patents with [productivity = myA] [set color blue]]]
  if (any? patents with [permisse? = true])[if (A = freeL) [create-link-with one-of patents with [productivity = myA] [set color pink]]]


  let rho c / (price * A)
  let m1 (3 - (2 * MS)) / (3 - (3 * MS))
  let m2 ((1.2 + price) * A ) / ( 2 * c)
  let markup min list  m1 (0.999 * m2)
  set invd   1 + DEPREC  - markup * rho   ; investimento desejavel
  ifelse profit <= 0 [ set invp ( DEPREC + profit ) ][ set invp ( DEPREC + ((1 + b) * profit ) )]

  let investK k * ((max list  (min list invd invp) 0.) + 0.97)

  set k investK

end

to licensing

  ask patents [
  let draw3 random-float 1.

  if (productivity > 0) and(draw3 <= ( 1 - MarketPower) ^ (lifetime))[ ;decisão de licenciar

   set permisse? true
   set shape "face happy"
   ]
  ]
end

to kill-firms

  if (k < minimal-capital) or ( X < -0.051 ) [ die]


end

to update-patents

  ask patents [

    if (lifetime > exclusivity-time) [
      set exclusive? false
      set color green

    ]

    let myprod productivity

    if (any? link-neighbors with [A > myprod]) [

      ask my-links with [[A] of other-end > myprod] [die]

    ]


  ]
end


to kill-patents

  ask patents [


    ifelse (any? my-links) and (lifetime > (exclusivity-time * 2 )) [][die]

  ]

end

to list-patents

  set patent-ON [productivity] of patents with [(exclusive? = true) and (permisse? = false)]
  set patent-OFF [productivity] of patents with [(exclusive? = false) and (permisse? = true)]
  set patent-Lic [productivity] of patents with [(exclusive? = true) and (permisse? = true)]
  set k_licenses [capital] of patents with [(exclusive? = true) and (permisse? = true)]



  set freeA max_a_disponivel

end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to statistics


  set ave_profit mean[profit] of firms
  set min_k min[k]of firms
  set max_k max[k]of firms
  set ave_k mean[k] of firms
  set ave_X mean[X] of firms
  set sum_q sum[q] of firms
  set CS (1.2 - price) * 53.3 + Dem * ( ln (Qt * price) - ln 53.3)
  set FS sum [ profit ] of firms
  set SocW FS + CS

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to ent

  ask firms [
     let mi (0.5 * (A0 + (alpha ^ time)) + 0.5 * ave_a)
     let sd  0.0589
     let zeta ln (1 + (sd ^ 2)/ (mi ^ 2))
     let M ln (mi) - zeta / 2
     let S sqrt zeta


    set A_e  exp(random-normal M S)  ;definindo produtividade inicial
    let u (random-normal 0 0.014)  ;definindo função erro

    if price * A_e - c > 0.06 + u [
    set nbImit random-poisson (2.5 * 0.2); numero de entrantes imitadoras
    set nbInnov random-poisson (0.25 * 0.2)  ;numero de entrantes inovadoras

      new-firms

    ]

  ]

end

to new-firms

  hatch-firms nbInnov [
    setxy random-pxcor * .8 random-pycor * .8
    set color gray
    set size 2
    set shape "circle"
    set label-color white

    ifelse (error-in < 0) [set u_n 0][set u_n error-in]
    ifelse (error-im < 0) [set u_m 0][set u_m error-im]

    set rim ( beta * meanRim ) + u_m
    set rin ( beta * meanRin ) + u_n
    set k max list (random-normal 25 7.5) 10       ;definindo Capital Inicial (o $, não a banda)
    set A A_e    ;definindo produtividade inicial
    set innovate? true
    set license? false
    set X ave_X

    set time 0
    set label time

  ]

  hatch-firms nbImit [
    setxy random-pxcor * .8 random-pycor * .8
    set color blue
    set size 2
    set shape "circle"
    set label-color white

    ifelse (error-im < 0) [set u_m 0][set u_m error-im]

    set rim ( beta * meanRim ) + u_m
    set rin 0
    set k max list (random-normal 25 7.5) 10        ;definindo Capital Inicial (o $, não a banda)
    set A A_e       ;definindo produtividade inicial
    set innovate? false
    set license? false
    set X ave_X

    set time 0
    set label time


]

end
@#$#@#$#@
GRAPHICS-WINDOW
14
10
259
241
16
16
6.061
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
Quarters
30.0

BUTTON
16
227
80
260
Setup
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
87
227
150
260
Go
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

BUTTON
157
227
220
260
Go 1
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
17
346
222
379
minimal-capital
minimal-capital
0
100
10
1
1
NIL
HORIZONTAL

SLIDER
16
304
221
337
number-of-periods
number-of-periods
0
500
161
1
1
NIL
HORIZONTAL

SLIDER
17
388
223
421
b
b
0
5
2
1
1
NIL
HORIZONTAL

SLIDER
18
427
225
460
exclusivity-time
exclusivity-time
0
30
12
1
1
quarters
HORIZONTAL

INPUTBOX
229
12
283
72
alpha
0.015
1
0
Number

INPUTBOX
287
11
338
71
ETA
1
1
0
Number

INPUTBOX
230
75
284
135
beta
0.167
1
0
Number

INPUTBOX
288
75
338
135
c
0.16
1
0
Number

SLIDER
19
467
226
500
nFounder
nFounder
0
10
2
1
1
NIL
HORIZONTAL

INPUTBOX
343
10
407
70
DEPREC
0.03
1
0
Number

INPUTBOX
344
75
409
135
Dem
64
1
0
Number

INPUTBOX
230
139
288
199
A0
0.135
1
0
Number

PLOT
438
10
638
160
Lucro
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ave_profit"

PLOT
440
166
640
316
Capital das Firmas
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ave_k"

PLOT
440
323
640
473
Concentração do Capital
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Hk" 1.0 0 -5825686 true "" "plot Hk"
"Hq" 1.0 0 -13840069 true "" "plot Hq"

PLOT
647
11
847
161
Produtividade
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ave_a"

PLOT
648
167
848
317
Firmas
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count firms"

TEXTBOX
253
228
403
326
Quadrados vermelhos são patentes em período de exclusividade. Quadrados verdes são patentes disponíveis ao mercado. Bolinhas Cinzas são inovadoras e Azuis são imitadoras
11
0.0
1

SWITCH
296
156
415
189
patents-die
patents-die
1
1
-1000

MONITOR
253
336
310
381
Anos
ticks / 4
0
1
11

PLOT
651
324
851
474
Price
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot price"

SLIDER
16
268
221
301
license-time
license-time
0
12
4
1
1
NIL
HORIZONTAL

PLOT
856
10
1056
160
Excedente do Consumidor
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot CS"

PLOT
857
169
1057
319
Excedente do Produtor
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot FS"

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
NetLogo 5.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="licenciamento" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="161"/>
    <metric>count firms</metric>
    <metric>mean [hk] of firms</metric>
    <metric>mean[CS] of firms</metric>
    <metric>mean[FS] of firms</metric>
    <metric>mean[Rim] of firms</metric>
    <metric>mean[Rin] of firms</metric>
    <metric>mean[A] of firms</metric>
    <metric>mean[price]of firms</metric>
    <metric>max[A] of firms</metric>
    <metric>count patents</metric>
    <metric>count patents with[shape = "face happy"]</metric>
    <metric>count links with[color = pink]</metric>
    <enumeratedValueSet variable="ETA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A0">
      <value value="0.135"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exclusivity-time">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.167"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal-capital">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patents-die">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEPREC">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-periods">
      <value value="161"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Dem">
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="license-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFounder">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="licenciamento2" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="161"/>
    <metric>count firms</metric>
    <metric>mean [hk] of firms</metric>
    <metric>mean[CS] of firms</metric>
    <metric>mean[FS] of firms</metric>
    <metric>mean[Rim] of firms</metric>
    <metric>mean[Rin] of firms</metric>
    <metric>mean[A] of firms</metric>
    <metric>mean[price]of firms</metric>
    <metric>max[A] of firms</metric>
    <metric>count patents</metric>
    <metric>count patents with[shape = "face happy"]</metric>
    <metric>count links with[color = pink]</metric>
    <enumeratedValueSet variable="ETA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="A0">
      <value value="0.135"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exclusivity-time">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.167"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal-capital">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patents-die">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DEPREC">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-periods">
      <value value="161"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Dem">
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="license-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nFounder">
      <value value="2"/>
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
