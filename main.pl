:- dynamic known/3.

% Predicados necesarios
multivalued(soporte).
multivalued(rango_temperatura).
multivalued(riego).
multivalued(rango_horas_luz_solar).
multivalued(area_malformacion).
multivalued(caracteristicas_manchas_hojas).
multivalued(color_hojas).
multivalued(caracteristicas_flores).
multivalued(plantar_cuento_con).
multivalued(experiencia_primerizo).
multivalued(caracteristicas_transplantacion).
multivalued(tipo_suelo).

ask(A, V):- 
  known(si, A, V),
  !.  
ask(A, V):- 
  known(_, A, V),
  !, 
  fail. 
ask(A, V):- 
  \+ multivalued(A),  
  known(si, A, V2),  
  V \== V2,  
  !, 
  fail.
ask(A, V):-
  format('~w es ~w? (si/no): ', [A, V]),
  read(S),
  asserta(known(S, A, V)),
  S == si.

booleanask(A, V, Question):- 
  known(si, A, V),
  !.  
booleanask(A, V, Question):- 
  known(si, A, _),
  !, 
  fail. 
booleanask(A, V, Question):-
  nl, write(Question), nl,
  write('[si,no] >>> '),
  read(S),
  check_boolval(S, A, V, Question),
  save_booleanask(S, A, V).

check_boolval(X, A, V, Question) :- 
  (X == si ; X == no), 
  !. 
check_boolval(X, A, V, Question) :- 
  write(X), write(' no es una opcion disponible. Intenta nuevamente.'), nl,  
  booleanask(A, V, Question).

save_booleanask(S, A, V) :-
  S == V,
  asserta(known(si, A, S)).
save_booleanask(S, A, V) :-
  S \== V,
  asserta(known(si, A, S)),
  !,
  fail.

menuask(A, V, MenuList, Question, Options) :- 
  known(si, A, V),  % succeed if true 
  !.  % stop looking  
menuask(A, V, MenuList, Question, Options) :- 
  known(_, A, V),  % fail if false 
  !, 
  fail. 
menuask(A, V, MenuList, Question, Options) :- 
  multivalued(A),  
  known(si, A, V2),
  V \== V2,
  !,
  fail.
menuask(A, V, MenuList, Question, Options) :- 
  multivalued(A),  
  known(si, A, V2),
  V == V2,
  !.
menuask(A, V, MenuList, Question, Options) :- 
  nl, write(Question), nl,
  show_options(Options),
  write(MenuList),
  write(' >>> '),
  read(X),  
  check_val(X, A, V, MenuList, Question, Options),  
  save_menuask(X, A, V).

show_options([]).
show_options([Opcion | Resto]) :-
  write(' '),
  write(' '),
  writeln(Opcion),
  show_options(Resto).

save_menuask(X, A, V) :-
  X == V,
  asserta(known(si, A, X)),
  !.
save_menuask(X, A, V) :-
  X \== V,
  asserta(known(si, A, X)),
  !,
  fail.

check_val(X, A, V, MenuList, Question, Options) :- 
  member(X, MenuList), 
  !. 
check_val(X, A, V, MenuList, Question, Options) :- 
  write(X), write(' no es una opcion disponible. Intenta nuevamente.'), nl,  
  menuask(A, V, MenuList, Question, Options).

% Variables necesarias
% Variables necesarias tomadas en riego y luz solar
sequedad_tierra(X) :- booleanask(sequedad_tierra, X,
  'Observa que la tierra alrededor presenta signos de sequedad en este momento?').
acumulacion_despues_riego(X) :- booleanask(acumulacion_despues_riego, X,
  'El agua tiende a acumularse despues del riego?').
suelo_humedo(X) :- booleanask(suelo_humedo, X,
  'Has notado que, cuando riegas, el suelo tiende a sentirse muy humedo al tacto?').
soporte(X) :- menuask(soporte, X, [suelo, maceta], 
  'En que tipo de soporte esta la planta?',
  [
    '- En maceta',
    '- En suelo directamente'
  ]).
rango_temperatura(X) :-
  menuask(rango_temperatura, X, [1, 2, 3, 4, 5, 6],
  'En este momento, cual es el rango de temperatura promedio actual?', [
    '1: Muy frio (menos de 0 grados)',
    '2: Frio (entre 0 y 15 grados)',
    '3: Templado (entre 15 y 25 grados)',
    '4: Calido (entre 25 y 35 grados)',
    '5: Muy caliente (entre 35 y 40 grados)',
    '6: Extremadamente caliente (mas de 40 grados)'
  ]).
riego(X) :-
  menuask(riego, X, [1, 2, 3, 4, 5, 6, 7],
  'En la semana actual (que va del lunes a domingo), cuantas veces has regado la planta?', [
    '1. No he regado en absoluto (0 veces)',
    '2. 1 vez',
    '3. 2 veces',
    '4. 3 veces',
    '5. 4 veces',
    '6. 5 veces',
    '7. Mas de 5 veces'
  ]).
rango_horas_luz_solar(X) :-
  menuask(rango_horas_luz_solar, X, [1, 2, 3, 4, 5, 6],
  'Cuantas horas de sol directo esta recibiendo tu planta?', [
    '1. Menos de 2 horas',
    '2. 2 a 4 horas',
    '3. 4 a 6 horas',
    '4. 6 a 8 horas',
    '5. 8 a 10 horas',
    '6. Mas de 10 horas'
  ]).

% Variables necesarias tomadas en plagas y enfermedades
malformaciones(X) :- booleanask(malformaciones, X, 'Las hojas o el fruto estan deformadas o presentan malformaciones?').
area_malformacion(X) :- menuask(area_malformacion, X, [
    hojas,
    ramas,
    flores,
    frutos
  ], 'Que parte exactamente del manzano presenta algun sintoma o malformacion?', [
    '- Hojas',
    '- Ramas',
    '- Flores',
    '- Frutos'
  ]).
manchas_hojas(X) :- booleanask(manchas_hojas, X, 'Las hojas presentan manchas?').
caracteristicas_manchas_hojas(X) :- menuask(caracteristicas_manchas_hojas, X, [1, 2, 3],
  'Como lucen las manchas?', [
    '1. Son manchas negras',
    '2. Son manchas marrones y circulares',
    '3. No aplica'
  ]).
polvo_hojas(X) :- booleanask(polvo_hojas, X, 'Las hojas presentan polvillo blanco?').
hojas_secas(X) :- booleanask(hojas_secas, X, 'Las hojas estan secas o acartonadas?').
enrollamiento_hojas(X) :- booleanask(enrollamiento_hojas, X, 'Las hojas tienen enrollamiento?').
color_hojas(X) :- menuask(color_hojas, X, [
  marron,
  amarillo,
  ninguno
], 'Cual es el color predominante de las hojas del manzano?', [
  '- Marron',
  '- Amarillo',
  '- Ninguno de los anteriores'
]).
hojas_caen_prematuramente(X) :- booleanask(hojas_caen_prematuramente, X, 'Las hojas tienden a caer prematuramente?').
manchas_ramas(X) :- booleanask(manchas_ramas, X, 'Las ramas tienen manchas visibles?').
manchas_corteza(X) :- booleanask(manchas_corteza, X, 'Las manchas estan en la corteza?').
ramas_secas(X) :- booleanask(ramas_secas, X, 'Las ramas estan secas o parecen muertas?').
sustancia_blanca_en_ramas(X) :- booleanask(sustancia_blanca_en_ramas, X, 'Hay una sustancia blanca algodonosa en las ramas?').
caracteristicas_flores(X) :- menuask(caracteristicas_flores, X, [1, 2, 3], 
  'Como lucen las flores en este momento?', [
    '1. Ennegrecidas o marchitas en su totalidad',
    '2. Estan simplemente secas',
    '3. No aplica'
  ]).
manchas_fruto(X) :- booleanask(manchas_fruto, X, 'El fruto tiene manchas visibles?').
manchas_marrones_fruto(X) :- booleanask(manchas_marrones_fruto, X, 'Las manchas son marrones?').
podrido_fruto(X) :- booleanask(podrido_fruto, X, 'La fruta se reblandece o se pudre?').
grietas_fruto(X) :- booleanask(grietas_fruto, X, 'Hay grietas o costras en el fruto?').
manchas_larvas_fruto(X) :- booleanask(manchas_larvas_fruto, X, 
  'Las manchas estan acompañadas de larvas en el interior del fruto').
dano_pulpa(X) :- booleanask(dano_pulpa, X, 'La pulpa del fruto esta daniada?').
seco_fruto(X) :- booleanask(seco_fruto, X, 'El fruto esta seco, arrugado o ennegrecido?').
fruto_cae_prematuramente(X) :- booleanask(fruto_cae_prematuramente, X, 
  'El fruto cae prematuramente?').

% Variables necesarias tomadas en el flujo de plantar
plantar_cuento_con(X) :- menuask(plantar_cuento_con, X, [1, 2, 3, 4], 
  'En este momento, con que cuentas exactamente?', [
    '1.- Semilla',
    '2.- Planta comprada en vivero',
    '3.- Planta lista para pasar a tierra',
    '4.- Nada. Quiero empezar'
  ]).
experiencia_primerizo(X) :- menuask(experiencia_primerizo, X, [1, 2], 
  'Como deseas que sea tu experiencia con este primer cultivo?', [
    '1. Quiero dedicarme fuertemente',
    '2. No quiero batallar'
  ]).
caracteristicas_transplantacion(X) :- menuask(caracteristicas_transplantacion, X, [1, 2],
  'Vas a hacer una transplantacion (pasar de una maceta a terreno fijo)?', [
    '1. Si, viene en una maceta',
    '2. No, la planta no venia con maceta'
  ]).
planta_comprada_vivero(X) :- booleanask(planta_comprada_vivero, X, 
  'La planta de la maceta fue comprada en un vivero').
tipo_suelo(X) :- menuask(tipo_suelo, X, [1, 2, 3, 4], 
  'Como es el tipo de suelo?', [
    '1. Tiene porosidad',
    '2. Es arcilloso',
    '3. Es arenoso',
    '4. No aplica'
  ]).
semilla_supermercado(X) :- booleanask(semilla_supermercado, X, 'Obtuviste las semillas en manzanas de supermercado').
maceta_capacidad_correcta(X) :- booleanask(maceta_capacidad_correcta, X, 'Tienes una maceta de, al menos, 50 litros de capacidad').

% Flujos 
% Flujo plantar
flujo_plantar :-
  write('Para recomendarte los mejores consejos sobre como plantar, necesito preguntar algunas cosas:'), nl, nl,
  solucion_conseguir_semilla(X),
  solucion_estatus_semilla(Y),
  solucion_inicio_riego(Z),
  solucion_tipo_suelo(A),
  solucion_estatus_semilla_supermercado(C),
  solucion_semilla_maceta(D),
  solucion_instrucciones_plantar(B),
  nl, nl, write('A continuacion, la respuesta del flujo:'), nl, nl,
  mensaje_solucion_conseguir_semilla,
  mensaje_solucion_estatus_semilla,
  mensaje_solucion_inicio_riego,
  mensaje_solucion_tipo_suelo,
  mensaje_solucion_estatus_semilla_supermercado,
  mensaje_solucion_semilla_maceta,
  mensaje_solucion_instrucciones_plantar,
  nl, write('Es todo por este flujo'), nl, nl.

mensaje_solucion_conseguir_semilla :-
  solucion_conseguir_semilla(1),
  write('* CON RESPECTO A INFORMACION DE COMO CONSEGUIR LA PLANTA: '), nl,
  write('Compra una planta de manzano en un vivero cercano, ya que suelen venir injertadas y son más fáciles de cuidar. Busca viveros en paginas web o redes sociales.'), nl, nl.
mensaje_solucion_conseguir_semilla :-
  solucion_conseguir_semilla(2),
  write('* CON RESPECTO A INFORMACION DE COMO CONSEGUIR LA PLANTA: '), nl,
  write('Debido a tus requerimientos, se propone conseguir semillas de manzano, ya sea comprandolas o extrayendolas directamente de las manzanas. Sin embargo, lo mas recomendable es adquirir plantas de vivero, ya que estas suelen estar injertadas, lo que garantiza mejores caracteristicas y un crecimiento mas adecuado.'), nl, nl.
mensaje_solucion_conseguir_semilla :-
  !.

mensaje_solucion_estatus_semilla :-
  solucion_estatus_semilla(1),
  write('* CON RESPECTO A INFORMACION DE ESTATUS DE PLANTA: '), nl,
  write('Esta planta es injertada, lo que la hace mas resistente a plagas y permite un crecimiento mas rapido.'), nl, nl.
mensaje_solucion_estatus_semilla :-
  solucion_estatus_semilla(2),
  write('* CON RESPECTO A INFORMACION DE ESTATUS DE PLANTA: '), nl,
  write('Ten en cuenta que, si plantas desde semilla, hay menos posibilidades de exito y tardaras mas de un año en poder trasladarla a tierra. Ademas, se recomienda empezar el proceso utilizando una maceta.'), nl, nl.
mensaje_solucion_estatus_semilla :-
  solucion_estatus_semilla(3),
  write('* CON RESPECTO A INFORMACION DE ESTATUS DE PLANTA: '), nl,
  write('Si la planta no proviene de un vivero, no se puede garantizar un crecimiento rapido. Sin embargo, es una mejor opcion que iniciar desde semillas.'), nl, nl.
mensaje_solucion_estatus_semilla :-
  write('* CON RESPECTO A INFORMACION DE ESTATUS DE PLANTA: '), nl,
  write('Sin informacion.'), nl, nl.

mensaje_solucion_estatus_semilla_supermercado :-
  solucion_estatus_semilla_supermercado(1),
  write('* CON RESPECTO A INFORMACION DE SEMILLAS PROVENIENTES DE FRUTOS DE SUPERMERCADO: '), nl,
  write('Ten en cuenta que las semillas de una manzana de supermercado pueden no crecer ni dar frutos, aunque es posible plantarlas.'), nl, nl.
mensaje_solucion_estatus_semilla_supermercado :-
  !.

mensaje_solucion_inicio_riego :-
  solucion_inicio_riego(1),
  write('* CON RESPECTO A INFORMACION INICIAL DE RIEGO: '), nl,
  write('Puedes plantar, pero asegurate de regar con frecuencia, ya que la manzana es sensible a la sequia y necesita mucha agua durante todo su crecimiento. La humedad debe mantenerse entre un 70% y 80% para evitar la deshidratacion. Para mas informacion de riego, dirigete a la opcion del menu principal: 3. Gestion de riego y luz solar.'), nl, nl.
mensaje_solucion_inicio_riego :-
  solucion_inicio_riego(2),
  write('* CON RESPECTO A INFORMACION INICIAL DE RIEGO: '), nl,
  write('Puedes plantar, pero asegurate de no regar en exceso, ya que la manzana no tolera el exceso de humedad. Aun asi, la humedad debe mantenerse entre un 70% y 80% para evitar la deshidratacion. Para mas informacion de riego, dirigete a la opcion del menu principal: 3. Gestion de riego y luz solar.'), nl, nl.
mensaje_solucion_inicio_riego :-
  write('* CON RESPECTO A INFORMACION INICIAL DE RIEGO: '), nl,
  write('Sin informacion. Para mas informacion de riego, dirigete a la opcion del menu principal: 3. Gestion de riego y luz solar.'), nl, nl.

mensaje_solucion_tipo_suelo :-
  solucion_tipo_suelo(1),
  write('* CON RESPECTO A INFORMACION DE TIPO DE SUELO: '), nl,
  write('No se recomienda plantar en este tipo de suelo porque no permite que el agua y el aire circulen bien.'), nl, nl.
mensaje_solucion_tipo_suelo :-
  solucion_tipo_suelo(2),
  write('* CON RESPECTO A INFORMACION DE TIPO DE SUELO: '), nl,
  write('No se recomienda plantar en este tipo de suelo, ya que suele ser poco rico en nutrientes y no retiene bien la humedad.'), nl, nl.
mensaje_solucion_tipo_suelo :-
  !.

mensaje_solucion_instrucciones_plantar :-
  solucion_instrucciones_plantar(3),
  write('* CON RESPECTO A INFORMACION DE COMO PLANTAR: '), nl,
  write('1.- Coloca las semillas en un papel ligeramente húmedo y ponlas en el refrigerador durante un mes.'), nl,
  write('2.- Después de un mes, cada semilla debe tener un pequeño brote.'), nl,
  write('3.- Para plantar el brote, usa un palito de madera pequeño para hacer un hoyo de al menos 10 cm en una maceta con tierra (suficiente para cubrir la semilla del brote).'), nl,
  write('4.- Coloca la semilla en el hoyo y cúbrela con tierra, dejando el brote fuera.'), nl,
  write('La planta puede tardar unos 5 meses en crecer. Se recomienda plantar cada semilla con brote en su propia maceta. Una vez que la planta crezca, puede trasladarse a un terreno fijo para continuar su cultivo.'), nl, nl.
mensaje_solucion_instrucciones_plantar :-
  solucion_instrucciones_plantar(1),
  write('* CON RESPECTO A INFORMACION DE COMO PLANTAR: '), nl,
  write('La planta tiene las caracteristicas adecuadas para crecer bien en este lugar. Antes de comenzar, asegúrate de que el área donde plantaras tenga suficiente espacio. Se recomienda que esté a una distancia de 5 a 8 metros de otras plantas:'), nl,
  write('1.- Marca con una varilla el lugar donde irá el árbol y usa una azada para remover la hierba.'), nl,
  write('2.- Con una pala, termina de retirar la hierba.'), nl,
  write('3.- Usa un pico para comenzar a cavar el hoyo. Continúa con la pala. El hoyo debe tener aproximadamente 50 cm de profundidad (esto puede variar según la profundidad de las raíces de la planta).'), nl,
  write('4.- Saca la planta de la bolsa y déjala en agua durante 15 minutos (preferentemente en una cubeta).'), nl,
  write('5.- Mientras la planta está en el agua, puedes colocar un poco de abono en el hoyo, como una mezcla de turba y humus.'), nl,
  write('6.- Después de 15 minutos, saca la planta del agua e introdúcela en el hoyo.'), nl,
  write('7.- Cubre las raíces con tierra y un poco de abono. Nota: Asegúrate de no cubrir el injerto del tronco. Pisa suavemente la tierra para asegurar la profundidad correcta.'), nl,
  write('8.- Una vez cubierto el hoyo, vierte el agua que usaste para remojar la planta en el área del hoyo.'), nl,
  write('9.- Si es necesario, coloca una estaca junto al tronco para evitar que el viento lo derribe. Usa un mazo para enterrar la estaca y amarra el tronco a ella con una cinta.'), nl, nl.
mensaje_solucion_instrucciones_plantar :-
  solucion_instrucciones_plantar(2),
  write('* CON RESPECTO A INFORMACION DE COMO PLANTAR: '), nl,
  write('No esperes resultados perfectos debido a las caracteristicas de la planta. Antes de comenzar, asegúrate de que el área donde plantaras tenga suficiente espacio. Se recomienda que esté a una distancia de 5 a 8 metros de otras plantas:'), nl,
  write('1.- Marca con una varilla el lugar donde irá el árbol y usa una azada para remover la hierba.'), nl,
  write('2.- Con una pala, termina de retirar la hierba.'), nl,
  write('3.- Usa un pico para comenzar a cavar el hoyo. Continúa con la pala. El hoyo debe tener aproximadamente 50 cm de profundidad (esto puede variar según la profundidad de las raíces de la planta).'), nl,
  write('4.- Saca la planta de la bolsa y déjala en agua durante 15 minutos (preferentemente en una cubeta).'), nl,
  write('5.- Mientras la planta está en el agua, puedes colocar un poco de abono en el hoyo, como una mezcla de turba y humus.'), nl,
  write('6.- Después de 15 minutos, saca la planta del agua e introdúcela en el hoyo.'), nl,
  write('7.- Cubre las raíces con tierra y un poco de abono. Nota: Asegúrate de no cubrir el injerto del tronco. Pisa suavemente la tierra para asegurar la profundidad correcta.'), nl,
  write('8.- Una vez cubierto el hoyo, vierte el agua que usaste para remojar la planta en el área del hoyo.'), nl,
  write('9.- Si es necesario, coloca una estaca junto al tronco para evitar que el viento lo derribe. Usa un mazo para enterrar la estaca y amarra el tronco a ella con una cinta.'), nl, nl.
mensaje_solucion_instrucciones_plantar :-
  write('* CON RESPECTO A INFORMACION DE COMO PLANTAR: '), nl,
  write('Sin informacion.'), nl, nl.

mensaje_solucion_semilla_maceta :-
  solucion_semilla_maceta(1),
  write('* CON RESPECTO A INFORMACION ADICIONAL DE MACETA: '), nl,
  write('La recomendacion general para manzanos son macetas de 50 litros de capacidad como minimo'), nl, nl.
mensaje_solucion_semilla_maceta :-
  !.


solucion_conseguir_semilla(1) :-
  plantar_cuento_con(4),
  experiencia_primerizo(2).
solucion_conseguir_semilla(2) :-
  plantar_cuento_con(4),
  experiencia_primerizo(1).
solucion_conseguir_semilla(0) :-
  !.

solucion_estatus_semilla(1) :-
  plantar_cuento_con(2).
solucion_estatus_semilla(1) :-
  plantar_cuento_con(3),
  caracteristicas_transplantacion(1),
  planta_comprada_vivero(si).
solucion_estatus_semilla(1) :-
  plantar_cuento_con(3),
  caracteristicas_transplantacion(2),
  planta_comprada_vivero(si).
solucion_estatus_semilla(1) :-
  solucion_conseguir_semilla(1).
solucion_estatus_semilla(2) :-
  plantar_cuento_con(1).
solucion_estatus_semilla(2) :-
  solucion_conseguir_semilla(2).
solucion_estatus_semilla(3) :-
  plantar_cuento_con(3),
  caracteristicas_transplantacion(1),
  planta_comprada_vivero(no).
solucion_estatus_semilla(0) :-
  !.

solucion_inicio_riego(1) :-
  (rango_temperatura(4) ; rango_temperatura(5) ; rango_temperatura(6)). % caluroso
solucion_inicio_riego(2) :-
  (rango_temperatura(1) ; rango_temperatura(2)). % frio
solucion_inicio_riego(0) :-
  !.

solucion_tipo_suelo(1) :-
  (solucion_inicio_riego(1) ; solucion_inicio_riego(2) ; rango_temperatura(3)), % temperatura agradable
  tipo_suelo(2).
solucion_tipo_suelo(2) :-
  (solucion_inicio_riego(1) ; solucion_inicio_riego(2) ; rango_temperatura(3)), % temperatura agradable
  tipo_suelo(3).
solucion_tipo_suelo(0) :-
  !.

solucion_instrucciones_plantar(3) :-
  solucion_estatus_semilla(2).
solucion_instrucciones_plantar(1) :-
  tipo_suelo(1).
solucion_instrucciones_plantar(2) :-
  \+ tipo_suelo(1).
solucion_instrucciones_plantar(0) :-
  !.

% Util para informacion con respecto a semilla supermercado
solucion_estatus_semilla_supermercado(0) :-
  \+ (plantar_cuento_con(1) ; solucion_estatus_semilla(2)),
  !.
solucion_estatus_semilla_supermercado(1) :-
  (plantar_cuento_con(1) ; solucion_estatus_semilla(2)),
  semilla_supermercado(si).
solucion_estatus_semilla_supermercado(0) :-
  !.

solucion_semilla_maceta(0) :-
  \+ (plantar_cuento_con(1) ; solucion_estatus_semilla(2)),
  !.
solucion_semilla_maceta(1) :-
  (plantar_cuento_con(1) ; solucion_estatus_semilla(2)),
  maceta_capacidad_correcta(no).
solucion_semilla_maceta(0) :-
  !.


% Flujo Riego/luz solar
flujo_riego_luz_solar :-
  write('Para recomendarte los mejores consejos en riego y luz solar, necesito preguntar algunas cosas:'), nl, nl,
  solucion_riego(X),
  solucion_encharcamiento(Y),
  solucion_luz_solar(Z),
  nl, nl, write('A continuacion, la respuesta del flujo:'), nl, nl,
  write('* CON RESPECTO A RIEGO: '), nl,
  mensaje_solucion_riego, nl,
  write('* CON RESPECTO A ENCHARCAMIENTO: '), nl,
  mensaje_solucion_encharcamiento, nl,
  write('* CON RESPECTO A LUZ SOLAR: '), nl,
  mensaje_solucion_luz_solar,
  nl, write('Es todo por este flujo'), nl, nl.

mensaje_solucion_riego :-
  solucion_riego(1),
  write('Riegala en este momento. Debido a tu clima, no basta con regarla 2 veces a la semana. Cada que observes sequedad en la tierra, es necesario regarla.'), nl.
mensaje_solucion_riego :-
  solucion_riego(2),
  write('No hace falta regarla en este momento. Sin embargo, debido a tu clima, cada que observes sequedad en la tierra es necesario regarla.'), nl.
mensaje_solucion_riego :-
  solucion_riego(3),
  write('En tiempo de frio, se recomienda un manzano se riegue, por lo menos, 2 veces a la semana. Por lo tanto, elige 2 dias de la semana para regarlo.'), nl.
mensaje_solucion_riego :-
  solucion_riego(4),
  write('En tiempo de frio, se recomienda un manzano se riegue, por lo menos, 2 veces a la semana. Por lo tanto, riega una vez mas en lo que queda de la semana.'), nl.
mensaje_solucion_riego :-
  solucion_riego(5),
  write('En tiempo de frio, se recomienda un manzano se riegue, por lo menos, 2 veces a la semana. Por lo tanto, ya no es necesario que riegues mas el manzano.'), nl.
mensaje_solucion_riego :-
  solucion_riego(6),
  write('En tiempo de frio, se recomienda un manzano se riegue, por lo menos, 2 veces a la semana. Sin embargo, has estado regado mucho el manzano. No la riegues mas en esta semana.'), nl.
mensaje_solucion_riego :-
  solucion_riego(_),
  write('Sin informacion'), nl.
mensaje_solucion_encharcamiento :-
  solucion_encharcamiento(1),
  write('Cuando se tiene un manzano, el suelo no debe encharcarse. Un suelo con mal drenaje puede conducir a pudricion de la raiz y muerte de la planta. Considera las siguientes opciones: '), nl,
  write('- Mejorar el drenaje (por ejemplo, agregar arena o perlita)'), nl,
  write('- Crear un sistema de drenaje (tuberias o zanjas)'), nl,
  write('- Cambiar la ubicacion del manzano a un area mas elevada'), nl,
  write('- Usar un deshidratador de suelo (si esta disponible)'), nl.
mensaje_solucion_encharcamiento :-
  solucion_encharcamiento(_),
  write('Sin informacion'), nl.
mensaje_solucion_luz_solar :- 
  solucion_luz_solar(1),
  write('Está todo correcto.'), nl.
mensaje_solucion_luz_solar :- 
  solucion_luz_solar(2),
  write('Tu planta esta recibiendo un exceso de luz solar. Aunque no es tan problematico, reconsidera reubicar tu planta en un sitio donde reciba aproximadamente 8 horas de luz solar.'), nl.
mensaje_solucion_luz_solar :- 
  solucion_luz_solar(3),
  write('Tu planta esta recibiendo muy poca luz solar. Esto es un problema grave, reconsidera reubicar tu planta en un sitio donde reciba aproximadamente 8 horas de luz solar.'), nl.
mensaje_solucion_luz_solar :- 
  solucion_luz_solar(4),
  write('Los manzanos requieren como minimo 8 horas de sol. Manten el riego constante si tienes mucha luz solar.'), nl.
mensaje_solucion_luz_solar :- 
  solucion_luz_solar(5),
  write('Tu planta esta recibiendo muy poca luz solar. Esto es un problema grave, sin embargo, debido a que tu planta esta fija no sera posible una reubicacion. No esperes resultados perfectos.'), nl.
mensaje_solucion_luz_solar :-
  solucion_luz_solar(_),
  write('Sin informacion'), nl.


solucion_encharcamiento(1) :-
  soporte(suelo),
  acumulacion_despues_riego(si),
  suelo_humedo(si).
solucion_encharcamiento(0) :-
  !.
solucion_riego(1) :-
  \+ (rango_temperatura(1) ; rango_temperatura(2)),
  sequedad_tierra(si).  
solucion_riego(2) :-
  \+ (rango_temperatura(1) ; rango_temperatura(2)),
  sequedad_tierra(no).
solucion_riego(3) :-
  (rango_temperatura(1) ; rango_temperatura(2)),
  riego(1).
solucion_riego(4) :-
  (rango_temperatura(1) ; rango_temperatura(2)),
  riego(2).
solucion_riego(5) :-
  (rango_temperatura(1) ; rango_temperatura(2)),
  riego(3).
solucion_riego(6) :-
  (rango_temperatura(1) ; rango_temperatura(2)),
  \+ (riego(1) ; riego(2) ; riego(3)).
solucion_riego(0) :-
  !,
  fail.
solucion_luz_solar(1) :-
  (rango_horas_luz_solar(4) ; rango_horas_luz_solar(5)),
  (soporte(suelo) ; soporte(maceta)).
solucion_luz_solar(2) :-
  rango_horas_luz_solar(6),
  soporte(maceta).
solucion_luz_solar(3) :-
  \+ (rango_horas_luz_solar(4) ; rango_horas_luz_solar(5) ; rango_horas_luz_solar(6)),
  soporte(maceta).  
solucion_luz_solar(4) :-
  rango_horas_luz_solar(6),
  soporte(suelo).
solucion_luz_solar(5) :-
  \+ (rango_horas_luz_solar(4) ; rango_horas_luz_solar(5) ; rango_horas_luz_solar(6)),
  soporte(suelo).
solucion_luz_solar(0) :-
  !,
  fail.

% Flujo plagas y enfermedades
flujo_plagas_enfermedades :-
  write('Para recomendarte los mejores consejos sobre el control de plagas y enfermedades, necesito preguntar algunas cosas:'), nl, nl,
  solucion_plaga_enfermedad(X),
  nl, nl, write('A continuacion, la respuesta del flujo:'), nl, nl,
  write('* CON RESPECTO A PLAGA/ENFERMEDAD: '), nl,
  mensaje_solucion_plaga_enfermedad,
  nl, write('Es todo por este flujo'), nl, nl.

mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(moteado),
  write('Es probable que tu manzano tenga la enfermedad moteado (Venturia inaequalis).'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Aplicar fungicidas preventivos como mancozeb o captan.'), nl,
  write('- Podar ramas infectadas y destruirlas.'), nl,
  write('- Usar variedades resistentes, como Liberty o Enterprise.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- Comun en manzanos susceptibles como Golden Delicious.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 1 temporada'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(antracnosis),
  write('Es probable que tu manzano tenga la enfermedad antracnosis (Colletotrichum coccodes).'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Aplicar fungicidas como azoxistrobina o mancozeb.'), nl,
  write('- Mejorar el drenaje y evitar el exceso de riego.'), nl,
  write('- Podar ramas infectadas.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- Afecta principalmente a manzanos en climas humedos.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 1-2 meses.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(oidio),
  write('Es probable que tu manzano tenga la enfermedad oidio (Podosphaera leucotricha).'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Aplicar fungicidas como azufre micronizado o miclobutanil.'), nl,
  write('- Podar y eliminar brotes infectados.'), nl,
  write('- Mejorar la ventilación del árbol.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- General, aunque afecta mas en climas calidos y secos.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 2-4 semanas.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(fuego_bacteriano),
  write('Es probable que tu manzano tenga la enfermedad fuego bacteriano (Erwinia amylovora).'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Podar partes infectadas y desinfectar herramientas.'), nl,
  write('- Usar bactericidas como oxitetraciclina.'), nl,
  write('- Evitar heridas en el árbol durante la temporada de crecimiento.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- Afecta a todas las variedades, pero algunas como Granny Smith son mas resistentes.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 1-2 temporadas, dependiendo de la gravedad.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(psila),
  write('Es probable que tu manzano tenga la plaga psila (Cacopsylla pyri).'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Aplicar aceite mineral en yemas durante el invierno.'), nl,
  write('- Usar insecticidas especificos como abamectina o piretroides en primavera.'), nl,
  write('- Introducir control biologico con depredadores naturales como Anthocoris nemoralis.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- Ataca principalmente a manzanos y perales.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 1-2 meses si se controla a tiempo.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(carpocapsa),
  write('Es probable que tu manzano tenga la plaga Carpocapsa (Cydia).'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Colocar trampas de feromonas para monitorear y capturar adultos.'), nl,
  write('- Aplicar insecticidas específicos como spinosad o lufenuron.'), nl,
  write('- Usar biocontrol con virus de la granulosis (Cydia pomonella granulovirus).'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- Principalmente afecta a variedades de manzanos dulces.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 1-2 ciclos de aplicacion (2-4 semanas por ciclo).'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(mosca_fruta),
  write('Es probable que tu manzano tenga la plaga mosca de la fruta (Ceratitis capitata).'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Uso de trampas con atrayentes (proteinas hidrolizadas o feromonas).'), nl,
  write('- Aplicacion de insecticidas como spinosad o lambda-cialotrina.'), nl,
  write('- Recolectar y destruir frutos infectados para evitar reinfestaciones.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- No solo afecta a varias variedades del manzano, sino tambien a otras frutas.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 3-4 semanas.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(piojo_san_jose),
  write('Es probable que tu manzano tenga la plaga piojo de san jose (Quadrasipidotus perniciosus).'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Aplicar aceite mineral en invierno.'), nl,
  write('- Usar insecticidas como clorpirifos o acefato en primavera.'), nl,
  write('- Introducir enemigos naturales como Encarsia perniciosi.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- General, afecta a varios tipos de frutales, incluido el manzano.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 1-2 meses con tratamientos adecuados.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(arana_roja),
  write('Es probable que tu manzano tenga la plaga araña roja (Panonychus ulmi).'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Aplicar acaricidas especificos como abamectina o fenpiroximato.'), nl,
  write('- Mejorar el riego y la humedad para reducir la proliferacion.'), nl,
  write('- Introducir control biologico con acaros depredadores como Phytoseiulus persimilis.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- General, afecta a todos los manzanos, especialmente en climas secos.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 2-3 semanas.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(piojo_harinoso),
  write('Es probable que tu manzano tenga la plaga piojo harinoso (Planococcus citri)'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Aplicar aceite mineral y jabon potasico.'), nl,
  write('- Usar insecticidas sistemicos como imidacloprid.'), nl,
  write('- Introducir control biologico con Cryptolaemus montrouzieri (escarabajo depredador).'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- General, aunque las variedades mas vigorosas suelen resistir mejor.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 1-2 meses.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(pulgon_lanigero),
  write('Es probable que tu manzano tenga la plaga pulgon lanigero (Eriosoma lanigerum)'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Podar ramas afectadas y quemarlas.'), nl, 
  write('- Aplicar aceite mineral y jabon potasico.'), nl, 
  write('- Usar enemigos naturales como Aphelinus mali.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- Ataca principalmente a variedades de manzanos de zonas templadas.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 1-2 meses.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(palomilla_marron),
  write('Es probable que tu manzano tenga la plaga palomilla marron de la manzana (Epiphyas postvittana)'), nl, nl,
  write('* CON RESPECTO A TRATAMIENTO:'), nl,
  write('- Monitorear con trampas de feromonas.'), nl, 
  write('- Aplicar insecticidas específicos como spinosad.'), nl, 
  write('- Recolectar hojas y frutos dañados.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- General, pero más común en climas templados.'), nl, nl,
  write('* CON RESPECTO A RECUPERACION:'), nl,
  write('- 3-4 semanas.'), nl.
mensaje_solucion_plaga_enfermedad :-
  solucion_plaga_enfermedad(_),
  write('Probablemente no hay plagas o enfermedades en tu planta'), nl.
solucion_plaga_enfermedad(moteado) :-
  malformaciones(si),
  area_malformacion(hojas),
  manchas_hojas(si),
  caracteristicas_manchas_hojas(1).
solucion_plaga_enfermedad(antracnosis) :-
  malformaciones(si),
  area_malformacion(hojas),
  manchas_hojas(si),
  caracteristicas_manchas_hojas(2).
solucion_plaga_enfermedad(oidio) :-
  malformaciones(si),
  area_malformacion(hojas),
  manchas_hojas(no),
  polvo_hojas(si).
solucion_plaga_enfermedad(psila) :-
  malformaciones(si),
  area_malformacion(hojas),
  manchas_hojas(no),
  polvo_hojas(no),
  hojas_secas(si).
solucion_plaga_enfermedad(psila) :-
  malformaciones(si),
  area_malformacion(hojas),
  manchas_hojas(no),
  polvo_hojas(no),
  hojas_secas(si).
solucion_plaga_enfermedad(palomilla_marron) :-
  malformaciones(si),
  area_malformacion(hojas),
  manchas_hojas(no),
  polvo_hojas(no),
  hojas_secas(no),
  enrollamiento_hojas(si).
solucion_plaga_enfermedad(arana_roja) :-
  malformaciones(si),
  area_malformacion(hojas),
  manchas_hojas(no),
  polvo_hojas(no),
  hojas_secas(no),
  enrollamiento_hojas(no),
  color_hojas(marron),
  hojas_caen_prematuramente(si).
solucion_plaga_enfermedad(piojo_harinoso) :-
  malformaciones(si),
  area_malformacion(hojas),
  manchas_hojas(no),
  polvo_hojas(no),
  hojas_secas(no),
  enrollamiento_hojas(no),
  color_hojas(amarillo).
solucion_plaga_enfermedad(antracnosis) :-
  malformaciones(si),
  area_malformacion(ramas),
  manchas_ramas(si),
  manchas_corteza(si).
solucion_plaga_enfermedad(piojo_san_jose) :-
  malformaciones(si),
  area_malformacion(ramas),
  manchas_ramas(no),
  ramas_secas(si).
solucion_plaga_enfermedad(pulgon_lanigero) :-
  malformaciones(si),
  area_malformacion(ramas),
  manchas_ramas(no),
  ramas_secas(no),
  sustancia_blanca_en_ramas(si).
solucion_plaga_enfermedad(fuego_bacteriano) :-
  malformaciones(si),
  area_malformacion(flores),
  caracteristicas_flores(1).
solucion_plaga_enfermedad(psila) :-
  malformaciones(si),
  area_malformacion(flores),
  caracteristicas_flores(2).
solucion_plaga_enfermedad(mosca_fruta) :-
  malformaciones(si),
  area_malformacion(frutos),
  manchas_fruto(si),
  manchas_marrones_fruto(si),
  podrido_fruto(si).
solucion_plaga_enfermedad(moteado) :-
  malformaciones(si),
  area_malformacion(frutos),
  manchas_fruto(si),
  manchas_marrones_fruto(si),
  podrido_fruto(no),
  grietas_fruto(si).
solucion_plaga_enfermedad(mosca_fruta) :-
  malformaciones(si),
  area_malformacion(frutos),
  manchas_fruto(si),
  manchas_marrones_fruto(no),
  manchas_larvas_fruto(si).
solucion_plaga_enfermedad(carpocapsa) :-
  malformaciones(si),
  area_malformacion(frutos),
  manchas_fruto(no),
  dano_pulpa(si).
solucion_plaga_enfermedad(fuego_bacteriano) :-
  malformaciones(si),
  area_malformacion(frutos),
  manchas_fruto(no),
  dano_pulpa(no),
  seco_fruto(si).
solucion_plaga_enfermedad(palomilla_marron) :-
  malformaciones(si),
  area_malformacion(frutos),
  manchas_fruto(no),
  dano_pulpa(no),
  seco_fruto(no),
  fruto_cae_prematuramente(si),
  hojas_caen_prematuramente(si).
solucion_plaga_enfermedad(moteado) :-
  malformaciones(si),
  area_malformacion(frutos),
  manchas_fruto(no),
  dano_pulpa(no),
  seco_fruto(no),
  fruto_cae_prematuramente(si),
  hojas_caen_prematuramente(no).
solucion_plaga_enfermedad(0) :-
  !.


% Base de conocimiento

% --- Interfaz principal ---
inicio :-
  nl,
  write('Bienvenido al sistema experto para principiantes en manzanos!!'), nl,
  write('--- Acciones ---'), nl,
  write('Introduce un numero para:'), nl,
  write('1. Plantacion'), nl,
  write('2. Control de plagas y enfermedades'), nl,
  write('3. Gestion de riego y luz solar'), nl,
  write('4. Cuidado del suelo'), nl,
  write('5. Limpiar memoria actual del sistema'), nl,
  write('6. Salir'), nl,
  write('>>> '),
  leer_opcion.

% --- Leer la opción del usuario ---
leer_opcion :-
  read(Opcion),
  manejar_opcion(Opcion).

% --- Manejar la opción del usuario ---
manejar_opcion(1) :-
  nl, write('Seleccionaste: Plantacion'), nl,
  flujo_plantar,
  inicio.

manejar_opcion(2) :-
  nl, write('Seleccionaste: Control de plagas y enfermedades'), nl,
  flujo_plagas_enfermedades,
  inicio.

manejar_opcion(3) :-
  nl, write('Seleccionaste: Gestion de riego y luz solar'), nl,
  flujo_riego_luz_solar,
  inicio.

manejar_opcion(4) :-
  nl, write('Seleccionaste: Cuidado del suelo'), nl,
  inicio.

manejar_opcion(5) :-
  retractall(known(_, _, _)),
  nl, write('Borrando memoria... Hecho'), nl,
  inicio.

manejar_opcion(6) :-
  nl, write('Saliendo... Adios!'), nl.

manejar_opcion(_) :-
  nl, write('Error, intenta de nuevo.'), nl,
  inicio.