; ######################################################################################################################################################################
; #                                           MANET NETWORK - PROJECT 3 COMPLEX SYSTEMS I ANNO L.M. INFORMATICA UNIBO                                                  #
; #                                                                                                                                                                    #
; #                                                                                                                                                                    #
; #      Developer :  Andrea Sghedoni                                                                                                                                  #
; #      Mail      :  andrea.sghedoni4@studio.unibo.it                                                                                                                 #
; #      Matr.     :  0000736038                                                                                                                                       #
; #                                                                                                                                                                    #                                                                                                                                                                                                                                     
; ######################################################################################################################################################################

; Il codice di debug è stato lasciato e commentato appositamente, cosicchè l'utente, qualora dovesse aver bisogno di avere un trace di alcune situazioni, può decommentare 
; le porzioni di codice interessate.


; estensione utile per alcune valutazioni da effettuare sulla rete
extensions [nw]
; piastrelle di forma quadrata che compongono l'area di movimento dei device
breed [plates plate]
; nome personalizzato delle turtle che rappresentano i device mobili
breed [devices device]
; collegamenti di servizio tra i device e i range di propagazione
undirected-link-breed [servicelinks servicelink]
; circonferenza che indica il range di propagazione del segnale, personale ad ogni device
breed [halos halo]
; connessioni tra device
undirected-link-breed [connections connection]

; variabili globali del sistema
globals [
  component-size          ; numero di device nel componente corrente che si sta controllando
  giant-component-size    ; numero di device del Giant Component
  giant-start-node        ; nodo individuato come la radice del GiantComponent
  tstotal                 ; lista che raccoglie i timestamp delle connessioni cancellate, utilizzata nel plot del Timestamp distribution
]

; attributi personali dei devices
devices-own [
  ID                      ; ID univoco ad ogni dispositivo
  location                ; locazione attuale del dispositivo
  range                   ; range di propagazione del segnale
  myneighbors             ; vicini a cui il device è connesso
  degree                  ; degree del dispositivo
  explored?               ; booleano che indica se il nodo è stato esplorato o meno (serve per determinare il Giant Component)
]

; attributi personali alle connessioni
connections-own [
  A                       ; ID del nodo ad un estremo
  B                       ; ID del nodo all'altro estremo
  timestamp               ; contatore che indica da quanto tempo è attiva quella determinata connessione
]

to setup
  ; pulisci tutto
  clear-all    
  ; i dispositivi sono rappresentati dallo shape creato in libreria
  set-default-shape devices "device3"
  ; una pistrella è rappresentata da un quadrato
  set-default-shape plates "square"
  ; il range è rappresentato da un ring
  set-default-shape halos "ring"
  ; setto i turtles e i link che l'estensione deve considerare per le valutazioni della rete
  nw:set-context devices connections
  
  set tstotal []
  ; creazione della griglia di piastrelle
  ask patches [ 
    sprout-plates 1 [ 
      set color black   ;; dark gray
      set size 1.2
    ] 
  ]
  ; crea quanti device indicati dallo slider in GUI
  create-devices number-of-devices [
    ; seleziona una locazione e lo posiziona 
    set location one-of plates
    move-to location
    ; se il flag RandomRange è off i devices hanno tutti il range indicato dallo slider, altrimenti viene settato uno random da 1 a MaxRange (scelto nello slider)
    ifelse (RandomRange)
      [set range 1 + random MaxRange]
      [set range 1 + MaxRange]
    set color blue
    set myneighbors []
  ] 
  
  ; settaggio degli ID univoci ai device
  let ID_start  0
  ask devices [
    set ID ID_start
    set ID_start ID_start + 1
  ]
     
  ; nascondi link di servizio
  ask servicelinks [hide-link]
  ; crea il range di propagazione per ogni dispositivo
  ask devices [make-halo range]
  ; nasconde le piastrelle cosi le connessioni sono mostrare con linee continue e non tratteggiate
  ask plates [hide-turtle]
  ; mostra gli Id dei device
  show_id_devices
  ; procedure per la colorazione del Giant Component
  find-all-components
  color-giant-component
  ; pulisci output precedenti
  clear-output
  ; reset ticks
  reset-ticks
end

; funzione cuore del progetto, viene richiamata in modo continuativo o step by step dall'utente tramite interfaccia
; muove i device, aggiorna le connessioni, trova il Giant Component
to go
  ; controllo che i device si debbano muovere o meno
  if speed > 0 [
    ; muovo i device nelle nuove locazioni
    ask devices [
      move-to futureLocation speed
    ]  
    ; controllo che le connessioni siano ancora attive: se questo non lo sono vengono cancellate
    ; decrementando i degree dei nodi coinvolti
    ask connections [ 
      if (is-active? = false) 
        [deleteconnection self]
    ]
     
    ; determino i vicini aggiornati dei device
    ask devices [set myneighbors findmyneighbors] 
    ; reset della variabile giant-component-size per il calcolo del Giant Component
    set giant-component-size 0
    ; procedure per la colorazione del Giant Component
    find-all-components
    color-giant-component
    ; differenzio la colorazione delle connessioni tra quelle coinvolte nel Giant Component e quelle no
    ask connections [set color [color] of end1]  
    ; aggiorno i timestamp delle connessioni attive, incrementandolo
    ask connections [set timestamp timestamp + 1]
    ; DEBUG
    ; show (word "[NUMERO DI CONNESSIONI: " count connections "]" ) 
    ; controllo che ogni dispositivo rispetti il vincolo del numero massimo di connessioni, se questo viene superato si mostra un messaggio di errore
    ask devices [ if (degree > MaxDegree) [show(word "[NODE : " ID "] ERRORE SUPERATO IL DEGREE MASSIMO : "MaxDegree "  -- DEGREE: "degree)]]
  ]
  tick
end

;[DEVICE CONTEXT] ritorna l'AgentSet di tutti i devices che sono connessi con il device preso in considerazione
to-report findmyneighbors
  ; come prima scrematura per trovare nuove connessioni possibili cerco tutti i device che sono dentro al raggio di propagazione del device
  ; e viceversa
  let p devices in-radius range
  let inContact p with [range >= abs distance myself and abs distance myself > 0]    
  ; tolgo dalle nuove connessioni candidate quelle già presenti
  if(myneighbors != nobody)
    [set inContact inContact with [not member? myself  myneighbors]]
  let appoggio ID
  ; per ognuna di queste cerco di crearle
  ask inContact [
    ; DEBUG
    ; show (word "[NODE " [ID] of myself "] CERCO DI EFFETTUARE UNA CONNESSIONE NUOVA CON IL NODE " ID )
    ; le variabili x e y contengono gli ID dei nodi che cercano di connettersi
    let x [ID] of myself
    let y ID 
                
    ; [CASE 1] : SE DEGREE-NODO-Y < MAXDEGREE AND DEGREE-NODO-X < MAXDEGREE --> AGGIUNGO LA CONNESSIONE SENZA DOVERNE TOGLIERE A NESSUNO DEI DUE ESTREMI
    if ((existconnection x = false) and (degree < MaxDegree) and ([degree] of myself < MaxDegree))  
      [setconnection x y]
                  
    ; [CASE 2] : SE DEGREE-NODO-X = MAXDEGREE AND DEGREE-NODO-Y < MAXDEGREE --> CERCO UNA CONNESSIONE PIU VECCHIA SOLTANTO PER IL NODO X, SE ESISTE LA CANCELLO ED AGGIUNGO LA NUOVA TRA X - Y
    if ((existconnection x = false) and (MaxDegree = [degree] of myself) and (MaxDegree > degree)) [
      let connenction-to-replace search-connection-to-replace x
                  
      ;DEBUG REPLACEMENT
      ;if (is-connection? connenction-to-replace)
      ;  [ask connenction-to-replace [show (word "[NODE " x "] PER FAR POSTO ALLA CONNESSIONE CON IL NODO " y " TOLGO LA CONNESSIONE TRA " A " - " B " CON TIMESTAMP:"timestamp" / TIMESTAMP MASSIMO: " max [timestamp] of connections with [A = x or B = x])]]
                   
      if (is-connection? connenction-to-replace) [
        deleteconnection connenction-to-replace
        setconnection x y 
      ]
    ]
                  
    ; [CASE 3] : SE DEGREE-NODO-X < MAXDEGREE AND DEGREE-NODO-Y = MAXDEGREE --> CERCO UNA CONNESSIONE PIU VECCHIA SOLTANTO PER IL NODO Y, SE ESISTE LA CANCELLO ED AGGIUNGO LA NUOVA TRA X - Y
    if ((existconnection x = false) and (MaxDegree > [degree] of myself) and (MaxDegree = degree)) [
      let connenction-to-replace search-connection-to-replace y
      
      ;DEBUG REPLACEMENT
      ;if (is-connection? connenction-to-replace)
      ;  [ask connenction-to-replace [show (word "[NODE " y "] PER FAR POSTO ALLA CONNESSIONE CON IL NODO " x " TOLGO LA CONNESSIONE TRA " A " - " B " CON TIMESTAMP:"timestamp" / TIMESTAMP MASSIMO: " max [timestamp] of connections with [A = y or B = y])]]
                    
      if (is-connection? connenction-to-replace)[
        deleteconnection connenction-to-replace
        setconnection x y 
      ]
    ]    
    
    ; [CASE 4] : SE DEGREE-NODO-Y = MAXDEGREE AND DEGREE NODO X = MAXDEGREE --> CERCO UNA CONNESSIONE PIU VECCHIA PER ENTRAMBI GLI ESTREMI, SE ESISTONO LE CANCELLO ED AGGIUNGO LA NUOVA TRA X - Y
    if ((existconnection x = false) and (MaxDegree = degree) and ([degree] of myself = MaxDegree)) [
      let connenction-to-replaceY search-connection-to-replace y
      let connenction-to-replaceX search-connection-to-replace x                   
                                
      ;DEBUG REPLACEMENT
      ;if ((is-connection? connenction-to-replaceY) and (is-connection? connenction-to-replaceX))
      ;  [ask connenction-to-replaceY [show (word "[NODE " y "] PER FAR POSTO ALLA CONNESSIONE CON IL NODO " x " TOLGO LA CONNESSIONE TRA " A " - " B " CON TIMESTAMP:"timestamp" / TIMESTAMP MASSIMO: " max [timestamp] of connections with [A = y or B = y])]
      ;  ask connenction-to-replaceX [show (word "[NODE " x "] PER FAR POSTO ALLA CONNESSIONE CON IL NODO " y " TOLGO LA CONNESSIONE TRA " A " - " B " CON TIMESTAMP:"timestamp" / TIMESTAMP MASSIMO: " max [timestamp] of connections with [A = x or B = x])]]
                                   
      if ((is-connection? connenction-to-replaceY) and (is-connection? connenction-to-replaceX)) [ 
        deleteconnection connenction-to-replaceX 
        deleteconnection connenction-to-replaceY 
        setconnection x y 
      ]
    ]                   
  ]
  ; ritorna i nuovi vicini aggiornati del nodo
  let ret getneighbors ID
  report ret
end 
 
; [DEVICE CONTEXT] funzione che determina se un device ha una connessione attiva con un altro dispositivo, dato l'ID di quest'ultimo
to-report existconnection [id0]
  ; determino gli estremi della connessione
  let nodeA ID
  let nodeB id0
  ; filtro le connessioni per trovare quella desiderata, se esiste.
  let filterconnection one-of connections with[((A = nodeA) and (B = nodeB)) or ((B = nodeA) and (A = nodeB))]
  ; se esiste torno true, false altrimenti
  ifelse (filterconnection != nobody)
    [report true]
    [report false]
end

; funzione che ritorna un Agentset contenete tutti i device vicini che hanno una connessione attiva con un particolare dispositivo
to-report getneighbors [id0]
  ; filtro le connessione in modo da avere solo quelle del nodo interessato
  let connectionsid0 connections with[((A = id0) or (B = id0))]  
  let listID []
  let neighborsdevices []
  ; se ce ne sono metto tutti gli ID dei device vicini e connessi nella lista listaid
  if (count connectionsid0 > 0) [ 
    ask connectionsid0 [ 
      ifelse (A = id0) 
        [set listID lput B listID]
        [set listID lput A listID]
    ]
  ]
  ; costruisco l'agentset dei vicini a partire dai loro ID e lo ritorno
  set neighborsdevices devices with [member? ID listID]
  report neighborsdevices   
end

; [CONNECTION CONTEXT] funzione che determina, tramite il ritorno di un booleano, se la connessione è ancora attiva o no 
to-report is-active?
  let active? checkrange A B
  ; DEBUG
  ;ifelse (booleano)
  ;  [show (word "La connessione tra il Node: " A " ed il Node: " B " è mantenuta")]
  ;  [show (word "La connessione tra il Node: " A " ed il Node: " B " è cancellata")]
  report active?
end

; funzione che verifica se due nodi sono l'uno nel range di propagazione dell'altro
to-report checkrange [id1 id2]
  ; risalgo ai nodi con gli ID 
  let nodeA one-of devices with [ID = id1]
  let nodeB one-of devices with [ID = id2]
  let distanceAB -1
  ask nodeA [set distanceAB distance nodeB]
  ; controllo la distanza e torno true o false a seconda della distanza tra i due nodi
  ifelse ((distanceAB <= [range] of nodeA) and (distanceAB <= [range] of nodeB))
    [report true]
    [report false]
end

; funzione che cancella una connessione data in input ed aggiorna i degree dei nodi coinvolti, decrementandoli per la connessione persa
to deleteconnection [c]
  ; risalgo ai nodi tramite gli ID posseduti dalla connessione c
  let nodeA  one-of devices with [ID = [A] of c]
  let nodeB  one-of devices with [ID = [B] of c]
  ; decremento i degree
  ask nodeA [set degree degree - 1]
  ask nodeB [set degree degree - 1]
  ; salvo il timestamp per le statistiche
  ask c [set tstotal lput timestamp tstotal]
  ; cancello la connessione
  ask c [die]
end

; funzione che crea una connessione tra i device con gli ID id1 ed id2, inoltre incrementa i degree per i due nodi coinvolti
to setconnection [id1 id2]
  ; risalgo ai nodi
  let nodeA  one-of devices with [ID = id1]
  let nodeB  one-of devices with [ID = id2] 
  
  ; creo la connessione
  if ((is-device? nodeA) and (is-device? nodeB)) [
    ask nodeA [ 
      create-connection-with nodeB [
        set A id1
        set B id2
        set timestamp 0
      ]
    ]
    ; aggiorno i degree, incrementandoli
    ask nodeA [set degree degree + 1]
    ask nodeB [set degree degree + 1]
  ]
end

; funzione che dato un ID in input, cerca una connessione da sostituire secondo la strategia che l'utente ha selezionato nel menù a tendina
; della GUI:
; - strategia RANDOM : seleziona una connessione random attiva di un particolare device, avente il timestamp > 0 [ovvero che non sia una connessione nuova appena creata]
; - strategia OLDEST : seleziona una connessione attiva di un particolare device, avente il timestamp > 0 ed il timestamp massimo tra tutte le connessioni di quel nodo
to-report search-connection-to-replace [id0]
  let candidate one-of connections with [A = id0 or B = id0] with [timestamp > 0]
  if (ReplacementStrategy = "Oldest")
    [set candidate one-of connections with [A = id0 or B = id0] with [timestamp > 0] with-max [timestamp]]
  report candidate                 
end

; [DEVICE CONTEXT] crea il range di propagazione del segnale
to make-halo [radius]

  hatch-halos 1 [ 
    let r radius * 2
    set size r
    set color lput 100 extract-rgb 105 
    __set-line-thickness 0.05
    ; creazione di un link tra la circonferenza e il device
    ; in modo da renderli collegati anche durante gli spostamenti
    create-servicelink-with myself [ 
      tie
      hide-link 
    ] 
  ]
end

; [DEVICE CONTEXT] determina la locazione futura in cui si muoverà il device, scelta con una direzione casuale in base alla velocità passata in input
to-report futureLocation [s]
 ; individuo la locazione attuale
 let currentX [pxcor] of location
 let currentY [pycor] of location
 
 ; individuo un plates per la locazione futura e lo ritorno
 let future-loc one-of plates with [distancexy currentX currentY <= s]
 set location future-loc
 report future-loc
end

; mostra in una label accanto ai device l'ID univoco assegnatogli
to show_id_devices
  ask devices [ 
    set label-color white 
    set label ID 
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funzioni relative alle statistiche da riportare all'utente, nell'interfaccia grafica ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; funzione che viene richiamata dal componente Monitor GUI, indica se il grafo è totalmente connesso oppure quanti device fanno parte del Giant Component
to-report functionplotfullconnected
  let return ""
  ifelse (giant-component-size = count devices)
    [set return "Connected!"]
    [set return (word "Not Connected - Giant Components :" giant-component-size " devices")]
  report return
end

; funzione che viene richiamata dal componente Monitor GUI, ritorna l'Edge Density della MANET
to-report functionmonitorED
  let m count connections
  let n count devices
  let ed ((2 * m) / (n * (n - 1)))       ;; FORMULA p = 2M / (N(N-1))
  let return (word "Edge Density: " ed)
  report return
end

; funzione che viene richiamata da un componente Monitor GUI e mostra il valore di Clustering Coefficient medio della rete, concetto di "friends of mine are friends between them"
to-report getMeanCC
  let return word "MEAN of CC: " mean [ nw:clustering-coefficient ] of devices
  report return
end

; funzione che ritorna il numero di connessioni presenti nella rete
to-report n_connections
  report count connections
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funzioni relative al calcolo del GiantComponent ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; funzione che cerca tutte le componenti presenti nella rete
to find-all-components
  ask devices [set explored? false]                                            
  ; LOOP finchè tutti i nodi non sono stati esplorati
  loop [
    ; prendi un nodo che non è stato esplorato
    let start one-of devices with [not explored?]
    ; se tutti sono stati esplorati STOP
    if (start = nobody) 
      [stop]
    ; reset della dimensione del componente ogni volta che considero un nodo inesplorato
    set component-size 0
    ; inizio l'esplorazione per la ricerca di una nuova componente
    ask start [explore (black + 2)]
    ; finita la procedura di visita su quel nodo se si ha trovato un potenziale Giant Component si aggiorna la dimensione
    ; ed la radice della componente
    if component-size > giant-component-size [
      set giant-component-size component-size
      set giant-start-node start
    ]
  ]
end

; funzione che cerca e trova tutti i nodi facenti parte di una stessa componente
to explore [new-color]  ;; node procedure
  if explored? 
    [stop]
  set explored? true
  set component-size component-size + 1
  set color new-color
  ask connection-neighbors[explore new-color]
end

; funzione che colora il Giant Component di rosso
to color-giant-component
  ask devices[set explored? false]               
  ask giant-start-node[explore red]              
end
@#$#@#$#@
GRAPHICS-WINDOW
511
25
996
531
16
16
14.4
1
14
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

BUTTON
0
10
67
43
SETUP
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
3
59
140
92
number-of-devices
number-of-devices
2
40
22
1
1
NIL
HORIZONTAL

SLIDER
3
95
141
128
speed
speed
1
5
5
1
1
NIL
HORIZONTAL

BUTTON
81
10
145
43
GO
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
161
10
269
43
GO ONE STEP
go\n
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
3
132
141
165
MaxRange
MaxRange
1
23
23
1
1
NIL
HORIZONTAL

SWITCH
152
133
288
166
RandomRange
RandomRange
1
1
-1000

SLIDER
2
169
141
202
MaxDegree
MaxDegree
0
count devices - 1
7
1
1
NIL
HORIZONTAL

PLOT
2
260
232
477
Trend Giant Component
Time(Ticks)
fraction in giant component
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"size" 1.0 0 -11033397 true "" "plotxy (ticks)\n       (giant-component-size / count devices)"

MONITOR
235
260
502
305
 Connectivity
functionplotfullconnected
17
1
11

MONITOR
235
308
502
353
Edge Density
functionmonitorED
17
1
11

PLOT
1010
26
1281
250
Degree Distribution
degree 
# of nodes
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "; recupero il degree massimo presente tra  i nodi della rete\nlet max-x max [count connection-neighbors] of devices\n; creo una lista con tutti i degree di tutti i device presenti\nlet listdegree [count connection-neighbors] of devices\n; recupero il valore massimo di y da plottare\nlet max-y count devices with [count connection-neighbors = item 0 modes listdegree]\nplot-pen-reset  \n; setto i range dell'asse x e y\nset-plot-x-range 0 (max-x + 1)   \nset-plot-y-range 0 (max-y)   \n; plot dell'istogramma\nhistogram [count connection-neighbors] of devices"

CHOOSER
3
207
143
252
ReplacementStrategy
ReplacementStrategy
"Random" "Oldest"
1

MONITOR
235
356
502
401
Mean of Clustering Coefficient 
getMeanCC
17
1
11

TEXTBOX
297
96
501
253
NOTE: Selezionare il MaxDegree solo dopo aver premuto il pulsante di Setup, perchè esso non può essere stabilito a priori senza sapere il numero di nodi scelto \n*********************************\nRandomRange - Off --> tutti i nodi hanno lo stesso range indicato nello slider MaxRange\nRandomRange - On --> ogni nodo ha un range scelto in modo randomico tra 1 ed il valore indicato nello slider MaxRange\n
11
0.0
0

MONITOR
235
404
502
449
Number of connections
n_connections
17
1
11

TEXTBOX
296
10
507
59
I link che mostrano le connessioni attive nella rete sono da intendersi come collegamenti logici e non fisici
11
0.0
0

PLOT
1010
259
1285
501
Timestamp Distributions of connections
timestamp
# of connections
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "let max-timestamp -1\nlet n_conn -1\nlet appoggio []\nif (count connections > 0)\n  [\n   ask connections[set appoggio lput timestamp tstotal]\n   set max-timestamp max appoggio\n   set n_conn length appoggio\n  ]\nif ((max-timestamp > -1)and (n_conn > -1))[\n  plot-pen-reset  \n  ;let list-ts [timestamp] of connections\n  let max-x max-timestamp + 1\n  ;let max-y length filter [? = item 0 modes appoggio] appoggio\n  set-plot-x-range 1 (max-x)\n  ;set-plot-y-range 0 (max-y)\n  ;show (word \"tstotal: \" appoggio)\n  histogram appoggio\n]"

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

device3
true
0
Rectangle -7500403 false true 22 75 280 225
Rectangle -7500403 true true 40 94 235 206
Circle -7500403 true true 243 138 24

devicemodello1
true
0
Rectangle -7500403 false true 75 30 225 285
Rectangle -7500403 false true 90 75 210 225
Circle -7500403 true true 135 240 30
Rectangle -7500403 true true 120 45 180 60

devicemodello2
true
2
Rectangle -7500403 true false 75 30 225 285
Rectangle -16777216 true false 90 60 209 251
Rectangle -16777216 true false 111 42 185 55
Circle -16777216 true false 139 256 22

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

hex
false
0
Polygon -7500403 true true 0 150 75 30 225 30 300 150 225 270 75 270

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

ring
true
0
Circle -7500403 false true 2 2 295

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
NetLogo 5.2.0
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

@#$#@#$#@
0
@#$#@#$#@
