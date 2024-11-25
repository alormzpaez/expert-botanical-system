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
multivalued(tipo_fertilizante).
multivalued(ph_suelo).
multivalued(deteccion_ph_suelo).
multivalued(tratamientos_suelo_alcalino).
multivalued(fertilizante_acido).
multivalued(tratamientos_suelo_acido).
multivalued(profundidad_correcta_suelo).

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
    '1: Muy frio',
    '2: Frio',
    '3: Templado',
    '4: Calido',
    '5: Muy caliente',
    '6: Extremadamente caliente'
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
  'Las manchas estan acompaniadas de larvas en el interior del fruto').
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
    '1. Equilibrado',
    '2. Es arcilloso',
    '3. Es arenoso',
    '4. No aplica'
  ]).
semilla_supermercado(X) :- booleanask(semilla_supermercado, X, 'Obtuviste las semillas en manzanas de supermercado').
maceta_capacidad_correcta(X) :- booleanask(maceta_capacidad_correcta, X, 'Tienes una maceta de, al menos, 50 litros de capacidad').

% Variables necesarias para el flujo de cuidado de suelo
tiempo_despues_de_transplantacion(X) :- booleanask(tiempo_despues_de_transplantacion, X, 
  'Ha pasado al menos 1 mes desde que se transplanto el manzano?').
uso_frecuente_fertilizantes(X) :- booleanask(uso_frecuente_fertilizantes, X, 
  'Has usado fertilizantes constantemente en la zona de tierra?').
historial_contaminacion_suelo(X) :- booleanask(historial_contaminacion_suelo, X, 
  'El suelo ha tenido un historial de contaminacion o plagas persistentes? ').
tipo_fertilizante(X) :- menuask(tipo_fertilizante, X, [1, 2, 3], 
  'Si deseas utilizar algun fertilizante, que tipo de fertilizantes consideras usar?', [
    '1. Organico',
    '2. Sintetico',
    '3. Otro'
  ]).
profundidad_correcta_suelo(X) :- menuask(profundidad_correcta_suelo, X, [1, 2, 3], 
  'La profundidad del suelo te permite sembrar tu manzano al menos a 60cm de profundidad?', [
    '1. Si',
    '2. No',
    '3. No se'
  ]).
drenaje_deficiente_suelo(X) :- booleanask(drenaje_deficiente_suelo, X, 'El suelo tiene problemas de compactacion o drenaje deficiente?').
extremo_drenaje_deficiente_suelo(X) :- booleanask(extremo_drenaje_deficiente_suelo, X, 'El drenaje o compactacion es extremadamente deficiente?').
extremo_arcilloso_suelo(X) :- booleanask(extremo_arcilloso_suelo, X, 'El suelo es extremadamente arcilloso?').
ph_suelo(X) :- menuask(ph_suelo, X, [1, 2, 3, 4], 'Reconoces si el tipo de suelo es:', [
  '1. Acido',
  '2. Neutro',
  '3. Alcalino',
  '4. Desconocido, pero quiero conocerlo'
]).
deteccion_ph_suelo(X) :- menuask(deteccion_ph_suelo, X, [1, 2, 3, 4], 
  'Suponiendo que no cuentas con instrumentos de medicion de pH (de ser asi, usalos y omite esto). Toma dos muestras del suelo para evitar alteraciones. En la primer muestra coloca jugo de limon, y si reacciona, es alcalino. En la segunda muestra coloca vinagre, y si reacciona, es acido. Ahora indica el tipo de suelo:', [
    '1. Acido',
    '2. Neutro',
    '3. Alcalino',
    '4. No aplica'
  ]).
tratamientos_suelo_alcalino(X) :- menuask(tratamientos_suelo_alcalino, X, [1, 2, 3, 4], 
  'Para suelos alcalinos se recomienda aplicar alguna de las siguientes enmiendas (recuerda utilizar guantes, gafas y mascarilla para manipular polvos):', [
    '1. Aplicacion de azufre elemental',
    '2. Aplicacion de fertilizantes acidos',
    '3. Aplicacion de materia organica',
    '4. Ninguno accesible'
  ]).
fertilizante_acido(X) :- menuask(fertilizante_acido, X, [1, 2, 3], 
  'Selecciona uno de los sig. fertilizantes a utilizar:', [
    '1. De sulfato de hierro',
    '2. De nitrato de amonio',
    '3. Ninguno accesible'
  ]).
tratamientos_suelo_acido(X) :- menuask(tratamientos_suelo_acido, X, [1, 2, 3, 4], 
  'Para suelos acidos se recomienda aplicar alguna de las siguientes enmiendas (recuerda utilizar guantes, gafas y mascarilla para manipular polvos):', [
    '1. Uso de cal viva o cal apagada',
    '2. Uso de ceniza de lenia sana',
    '3. Uso de carbonato calcico',
    '4. Ninguno accesible'
  ]).

% Flujos 
% Flujo cuidado suelo
flujo_cuidado_suelo :-
  write('Para recomendarte los mejores consejos sobre como cuidar tu suelo, necesito preguntar algunas cosas:'), nl, nl,
  solucion_adaptacion_al_suelo(X),
  solucion_accesible_sembrar(Y),
  solucion_tipo_fertilizante(Z),
  solucion_profundidad_suelo(A),
  solucion_drenaje_deficiente_suelo(B),
  solucion_caracteristicas_suelo(C),
  solucion_ph_suelo(D),
  solucion_tratamiento_suelo_acido(E),
  solucion_tratamiento_suelo_alcalino(F),
  solucion_tratamiento_advertencias(G),
  solucion_instrucciones_suelo(H),
  nl, nl, write('A continuacion, la respuesta del flujo:'), nl, nl,
  mensaje_solucion_adaptacion_al_suelo,
  mensaje_solucion_accesible_sembrar,
  mensaje_solucion_tipo_fertilizante,
  mensaje_solucion_profundidad_suelo,
  mensaje_solucion_drenaje_deficiente_suelo,
  mensaje_solucion_caracteristicas_suelo,
  mensaje_solucion_ph_suelo,
  mensaje_solucion_tratamiento_suelo_acido,
  mensaje_solucion_tratamiento_suelo_alcalino,
  mensaje_solucion_tratamiento_advertencias,
  mensaje_solucion_instrucciones_suelo,
  nl, write('Valida los niveles de nutrientes en el suelo y en caso de ser necesario apoya al aumento del nutriente requerido para la siembra de manzanos con un plan de fertilizacion adecuado o en su defecto evite el uso de fertilizantes para evitar que los nutrientes sean excesivos.'), nl,
  write('Es todo por este flujo'), nl, nl.

mensaje_solucion_adaptacion_al_suelo :-
  solucion_adaptacion_al_suelo(1),
  write('* CON RESPECTO A INFORMACION DE ADAPTACION AL SUELO: '), nl,
  write('Si la raiz no se adapta al lugar de plantacion por, al menos, un mes, puedes correr el riesgo de quemar la raiz. No es un buen lugar para plantar'), nl, nl.
mensaje_solucion_adaptacion_al_suelo :-
  !.  

mensaje_solucion_accesible_sembrar :-
  solucion_accesible_sembrar(1),
  write('* CON RESPECTO A INFORMACION DE ACCESIBILIDAD A SEMBRAR: '), nl,
  write('No se recomienda sembrar en esta zona.'), nl, nl.
mensaje_solucion_accesible_sembrar :-
  solucion_accesible_sembrar(2),
  write('* CON RESPECTO A INFORMACION DE ACCESIBILIDAD A SEMBRAR: '), nl,
  write('No es recomendable plantar manzanos en la zona debido a la dificultad de la tierra para retener  nutrientes.'), nl, nl.
mensaje_solucion_accesible_sembrar :-
  !.  

mensaje_solucion_tipo_fertilizante :-
  solucion_tipo_fertilizante(1),
  write('* CON RESPECTO A INFORMACION DE TIPO DE FERTILIZANTE: '), nl,
  write('Continue con un plan de fertilizacion natural.'), nl, nl.
mensaje_solucion_tipo_fertilizante :-
  solucion_tipo_fertilizante(2),
  write('* CON RESPECTO A INFORMACION DE TIPO DE FERTILIZANTE: '), nl,
  write('Asegurese de no exceder las dosis recomendadas.'), nl, nl.
mensaje_solucion_tipo_fertilizante :-
  solucion_tipo_fertilizante(3),
  write('* CON RESPECTO A INFORMACION DE TIPO DE FERTILIZANTE: '), nl,
  write('Considere un plan combinado para maximizar beneficios.'), nl, nl.
mensaje_solucion_tipo_fertilizante :-
  !.  


mensaje_solucion_profundidad_suelo :-
  solucion_profundidad_suelo(1),
  write('* CON RESPECTO A INFORMACION DE PROFUNDIDAD DE SUELO: '), nl,
  write('Se recomienda validar que la profundidad del suelo sea de al menos 60cm para no limitar el crecimiento de los manzanos.'), nl, nl.
mensaje_solucion_profundidad_suelo :-
  !.  

mensaje_solucion_drenaje_deficiente_suelo :-
  solucion_drenaje_deficiente_suelo(1),
  write('* CON RESPECTO A INFORMACION DE DEFICIENCIA DE DRENAJE DEL SUELO: '), nl,
  write('No es recomendable plantar manzanos en la zona debido a que el drenaje deficiente podria causar asfixia a las raices y ocasionar pudricion.'), nl, nl.
mensaje_solucion_drenaje_deficiente_suelo :-
  solucion_drenaje_deficiente_suelo(2),
  write('* CON RESPECTO A INFORMACION DE DEFICIENCIA DE DRENAJE DEL SUELO: '), nl,
  write('Recomendacion: Realizar trabajos de aireacion o drenaje.'), nl, nl.
mensaje_solucion_drenaje_deficiente_suelo :-
  !.  

mensaje_solucion_caracteristicas_suelo :-
  solucion_caracteristicas_suelo(1),
  write('* CON RESPECTO A INFORMACION DE CARACTERISTICAS DEL SUELO: '), nl,
  write('Suelo de drenaje rapido. Asegurese de una fertilizacion adecuada.'), nl, nl.
mensaje_solucion_caracteristicas_suelo :-
  solucion_caracteristicas_suelo(2),
  write('* CON RESPECTO A INFORMACION DE CARACTERISTICAS DEL SUELO: '), nl,
  write('Suelo pesado. Considere mejorar la aireacion.'), nl, nl.
mensaje_solucion_caracteristicas_suelo :-
  solucion_caracteristicas_suelo(3),
  write('* CON RESPECTO A INFORMACION DE CARACTERISTICAS DEL SUELO: '), nl,
  write('Es un suelo optimo.'), nl, nl.
mensaje_solucion_caracteristicas_suelo :-
  !.  

mensaje_solucion_ph_suelo :-
  solucion_ph_suelo(1),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL PH DEL SUELO: '), nl,
  write('Es altamente probable que el suelo presente caracteristicas acidas.'), nl, nl.
mensaje_solucion_ph_suelo :-
  solucion_ph_suelo(2),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL PH DEL SUELO: '), nl,
  write('Es altamente probable que el suelo presente caracteristicas neutras.'), nl, nl.
mensaje_solucion_ph_suelo :-
  solucion_ph_suelo(3),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL PH DEL SUELO: '), nl,
  write('Es altamente probable que el suelo presente caracteristicas alcalinas.'), nl, nl.
mensaje_solucion_ph_suelo :-
  !.  

mensaje_solucion_tratamiento_suelo_acido :-
  solucion_tratamiento_suelo_acido(1),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL TRATAMIENTO DEL SUELO ACIDO: '), nl,
  write('Para subir 1 unidad de pH se recomienda utilizar  entre 300 gr y 500 gr por m2 de cal viva o entre 200gr y 400gr por m2.'), nl, nl.
mensaje_solucion_tratamiento_suelo_acido :-
  solucion_tratamiento_suelo_acido(2),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL TRATAMIENTO DEL SUELO ACIDO: '), nl,
  write('Para subir 1 unidad de pH se recomienda utilizar  entre 100 gr y 200 gr por m2 de ceniza de lenia sana.'), nl, nl.
mensaje_solucion_tratamiento_suelo_acido :-
  solucion_tratamiento_suelo_acido(3),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL TRATAMIENTO DEL SUELO ACIDO: '), nl,
  write('Para subir 1 unidad de pH se recomienda utilizar  entre 0.750 kg y 1.250 kg por m2.'), nl, nl.
mensaje_solucion_tratamiento_suelo_acido :-
  !.  

mensaje_solucion_tratamiento_suelo_alcalino :-
  solucion_tratamiento_suelo_alcalino(1),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL TRATAMIENTO DEL SUELO ALCALINO: '), nl,
  write('Para bajar 1 unidad de pH se recomienda utilizar  entre 400 gr y 600 gr de sulfato de hierro por m2 y para diluir en agua de riego 4gr por litro.'), nl, nl.
mensaje_solucion_tratamiento_suelo_alcalino :-
  solucion_tratamiento_suelo_alcalino(2),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL TRATAMIENTO DEL SUELO ALCALINO: '), nl,
  write(' Para bajar 1 unidad de pH se recomienda utilizar  entre 1 kg de nitrato de amonio por m2 o para diluir en agua de riego 1kg por litro. Al finalizar riegue el area para ayudar a que el nitrato se integre en el suelo'), nl, nl.
mensaje_solucion_tratamiento_suelo_alcalino :-
  solucion_tratamiento_suelo_alcalino(3),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL TRATAMIENTO DEL SUELO ALCALINO: '), nl,
  write('Se recomienda la aplicacion de estiercol compostado. Esta aplicacion es Ideal para suelos sueltos y con buen drenaje. Para bajar 1 unidad de pH se recomienda utilizar  entre 1 kg a 4kg por m2 de estiercol. Los diferentes tipos de estiercol tienen distintos niveles de nutrientes.'), nl, nl.
mensaje_solucion_tratamiento_suelo_alcalino :-
  solucion_tratamiento_suelo_alcalino(4),
  write('* CON RESPECTO A INFORMACION RELACIONADA AL TRATAMIENTO DEL SUELO ALCALINO: '), nl,
  write('El uso de azufre elemental se considera mas lento que otros fertilizantes pero efectivo. Para bajar 1 unidad de pH se recomienda utilizar entre 0.5 kg y 1 kg por m2 de Azufre.'), nl, nl.
mensaje_solucion_tratamiento_suelo_alcalino :-
  !.  

mensaje_solucion_tratamiento_advertencias :-
  solucion_tratamiento_advertencias(1),
  write('* CON RESPECTO A INFORMACION RELACIONADA A ADVERTENCIAS CON EL TRATAMIENTO: '), nl,
  write('- Adecuado para situaciones donde se requiere una accion rapida, pero debe aplicarse con precaucion para evitar sobrepasar el nivel de pH deseado.'), nl,
  write('- En caso de usar cal viva es importante no mezclar con agua, ya  que produce una reaccion exotermica y corrosiva.'), nl, nl.
mensaje_solucion_tratamiento_advertencias :-
  solucion_tratamiento_advertencias(2),
  write('* CON RESPECTO A INFORMACION RELACIONADA A ADVERTENCIAS CON EL TRATAMIENTO: '), nl,
  write('- Ideal para suelos compactos.'), nl,
  write('- Actua mas rapido que el azufre pero persiste durante menos tiempo. '), nl,
  write('- Evite aplicar en dias calurosos o secos.'), nl,
  write('- Es necesario vigilar los niveles de salinidad para evitar danios a los cultivos.'), nl,
  write('- Su uso excesivo puede llevar a la acumulacion de hierro.'), nl, nl.
mensaje_solucion_tratamiento_advertencias :-
  solucion_tratamiento_advertencias(3),
  write('* CON RESPECTO A INFORMACION RELACIONADA A ADVERTENCIAS CON EL TRATAMIENTO: '), nl,
  write('- No es recomendable su uso en suelos arenosos ya que puede reducir la eficacia del fertilizante.'), nl,
  write('- Es posible que requieran aplicaciones repetidas'), nl,
  write('- A largo plazo puede llevar a desequilibrios nutricionales, ya que un pH elevado puede limitar la disponibilidad de otros nutrientes.'), nl, nl.
mensaje_solucion_tratamiento_advertencias :-
  solucion_tratamiento_advertencias(4),
  write('* CON RESPECTO A INFORMACION RELACIONADA A ADVERTENCIAS CON EL TRATAMIENTO: '), nl,
  write('- Asegurese de que el estiercol se encuentra bien compostado para reducir riesgos de patogenos.'), nl,
  write('- No debe excederse la dosis ya que podria provocar problemas de sanidad o contaminacion.'), nl, nl.
mensaje_solucion_tratamiento_advertencias :-
  solucion_tratamiento_advertencias(5),
  write('* CON RESPECTO A INFORMACION RELACIONADA A ADVERTENCIAS CON EL TRATAMIENTO: '), nl,
  write('- De no manejarse adecuadamente la conductividad electrica del suelo puede aumentar debido al incremento de sales solubles.'), nl, nl.
mensaje_solucion_tratamiento_advertencias :-
  !.  

mensaje_solucion_instrucciones_suelo :-
  solucion_instrucciones_suelo(1),
  write('* CON RESPECTO A INFORMACION RELACIONADA A INSTRUCCIONES DE TRATAMIENTO: '), nl,
  write('- Introduce la cal en el suelo a una profundidad de 15 a 20 cm y mezcla.'), nl,
  write('- Advertencia: Recuerda que cualquier enmienda debe ser de forma escalonada y progresiva. Se recomienda verificar el pH del suelo cada 3 o 4 meses.'), nl, nl.
mensaje_solucion_instrucciones_suelo :-
  solucion_instrucciones_suelo(2),
  write('* CON RESPECTO A INFORMACION RELACIONADA A INSTRUCCIONES DE TRATAMIENTO: '), nl,
  write('- Introduce la cal en el suelo a una profundidad de 15 a 20 cm y mezcla.'), nl,
  write('- Advertencia: Recuerda que cualquier enmienda debe ser de forma escalonada y progresiva. Se recomienda verificar el pH del suelo cada 3 o 4 meses.'), nl, nl.
mensaje_solucion_instrucciones_suelo :-
  !.  

solucion_adaptacion_al_suelo(1) :-
  tiempo_despues_de_transplantacion(no).
solucion_adaptacion_al_suelo(0) :-
  !.

solucion_accesible_sembrar(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_accesible_sembrar(1) :-
  uso_frecuente_fertilizantes(si),
  historial_contaminacion_suelo(si).
solucion_accesible_sembrar(1) :-
  uso_frecuente_fertilizantes(no),
  (tipo_fertilizante(1) ; tipo_fertilizante(2) ; tipo_fertilizante(3)),
  profundidad_correcta_suelo(2).
solucion_accesible_sembrar(2) :-
  tipo_suelo(2),
  extremo_arcilloso_suelo(si).
solucion_accesible_sembrar(0) :-
  !.

solucion_tipo_fertilizante(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_tipo_fertilizante(1) :- % organico
  tipo_fertilizante(1).
solucion_tipo_fertilizante(2) :- % sintetico
  tipo_fertilizante(2).
solucion_tipo_fertilizante(3) :- % otro
  tipo_fertilizante(3).
solucion_tipo_fertilizante(0) :-
  !.

solucion_profundidad_suelo(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_profundidad_suelo(1) :-
  profundidad_correcta_suelo(3).
solucion_profundidad_suelo(0) :-
  !.

solucion_drenaje_deficiente_suelo(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_drenaje_deficiente_suelo(1) :- 
  drenaje_deficiente_suelo(si),
  extremo_drenaje_deficiente_suelo(si).
solucion_drenaje_deficiente_suelo(2) :- 
  drenaje_deficiente_suelo(si),
  extremo_drenaje_deficiente_suelo(no).
solucion_drenaje_deficiente_suelo(0) :-
  !.

solucion_caracteristicas_suelo(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_caracteristicas_suelo(1) :-
  tipo_suelo(3).
solucion_caracteristicas_suelo(2) :-
  tipo_suelo(2),
  extremo_arcilloso_suelo(no).
solucion_caracteristicas_suelo(3) :-
  tipo_suelo(1).
solucion_caracteristicas_suelo(0) :-
  !.

solucion_ph_suelo(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_ph_suelo(1) :- % acido
  tiempo_despues_de_transplantacion(si),
  uso_frecuente_fertilizantes(si),
  historial_contaminacion_suelo(no).
solucion_ph_suelo(1) :- % acido
  ph_suelo(1).
solucion_ph_suelo(2) :- % neutro
  ph_suelo(2).
solucion_ph_suelo(3) :- % alcalino
  ph_suelo(3).
solucion_ph_suelo(1) :- % acido
  ph_suelo(4),
  deteccion_ph_suelo(1).
solucion_ph_suelo(2) :- % neutro
  ph_suelo(4),
  deteccion_ph_suelo(2).
solucion_ph_suelo(3) :- % alcalino
  ph_suelo(4),
  deteccion_ph_suelo(3).
solucion_ph_suelo(0) :-
  !.

solucion_tratamiento_suelo_acido(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_tratamiento_suelo_acido(1) :-
  solucion_ph_suelo(1),
  tratamientos_suelo_acido(1).
solucion_tratamiento_suelo_acido(2) :-
  solucion_ph_suelo(1),
  tratamientos_suelo_acido(2).
solucion_tratamiento_suelo_acido(3) :-
  solucion_ph_suelo(1),
  tratamientos_suelo_acido(3).
solucion_tratamiento_suelo_acido(0) :-
  !.

solucion_tratamiento_suelo_alcalino(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_tratamiento_suelo_alcalino(3) :-
  solucion_ph_suelo(3),
  tratamientos_suelo_alcalino(3).
solucion_tratamiento_suelo_alcalino(4) :-
  solucion_ph_suelo(3),
  tratamientos_suelo_alcalino(1).
solucion_tratamiento_suelo_alcalino(1) :-
  solucion_ph_suelo(3),
  tratamientos_suelo_alcalino(2),
  fertilizante_acido(1).
solucion_tratamiento_suelo_alcalino(2) :-
  solucion_ph_suelo(3),
  tratamientos_suelo_alcalino(2),
  fertilizante_acido(2).
solucion_tratamiento_suelo_alcalino(0) :-
  !.

solucion_tratamiento_advertencias(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_tratamiento_advertencias(1) :-
  solucion_ph_suelo(1),
  solucion_tratamiento_suelo_acido(1).
solucion_tratamiento_advertencias(2) :-
  solucion_ph_suelo(3),
  solucion_tratamiento_suelo_alcalino(1).
solucion_tratamiento_advertencias(3) :-
  solucion_ph_suelo(3),
  solucion_tratamiento_suelo_alcalino(2).
solucion_tratamiento_advertencias(4) :-
  solucion_ph_suelo(3),
  solucion_tratamiento_suelo_alcalino(3).
solucion_tratamiento_advertencias(5) :-
  solucion_ph_suelo(3),
  solucion_tratamiento_suelo_alcalino(4).
solucion_tratamiento_advertencias(0) :-
  !.

solucion_instrucciones_suelo(0) :-
  tiempo_despues_de_transplantacion(no),
  !.
solucion_instrucciones_suelo(1) :-
  solucion_ph_suelo(1),
  (solucion_tratamiento_suelo_acido(1) ; solucion_tratamiento_suelo_acido(2) ; solucion_tratamiento_suelo_acido(3)).
solucion_instrucciones_suelo(2) :-
  solucion_ph_suelo(3),
  (solucion_tratamiento_suelo_alcalino(1) ; solucion_tratamiento_suelo_alcalino(2) ; solucion_tratamiento_suelo_alcalino(3) ; solucion_tratamiento_suelo_alcalino(4)).
solucion_instrucciones_suelo(0) :-
  !.


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
  write('Compra una planta de manzano en un vivero cercano, ya que suelen venir injertadas y son mas faciles de cuidar. Busca viveros en paginas web o redes sociales.'), nl, nl.
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
  write('Ten en cuenta que, si plantas desde semilla, hay menos posibilidades de exito y tardaras mas de un anio en poder trasladarla a tierra. Ademas, se recomienda empezar el proceso utilizando una maceta.'), nl, nl.
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
  write('1.- Coloca las semillas en un papel ligeramente humedo y ponlas en el refrigerador durante un mes.'), nl,
  write('2.- Despues de un mes, cada semilla debe tener un pequenio brote.'), nl,
  write('3.- Para plantar el brote, usa un palito de madera pequenio para hacer un hoyo de al menos 10 cm en una maceta con tierra (suficiente para cubrir la semilla del brote).'), nl,
  write('4.- Coloca la semilla en el hoyo y cubrela con tierra, dejando el brote fuera.'), nl,
  write('La planta puede tardar unos 5 meses en crecer. Se recomienda plantar cada semilla con brote en su propia maceta. Una vez que la planta crezca, puede trasladarse a un terreno fijo para continuar su cultivo.'), nl, nl.
mensaje_solucion_instrucciones_plantar :-
  solucion_instrucciones_plantar(1),
  write('* CON RESPECTO A INFORMACION DE COMO PLANTAR: '), nl,
  write('La planta tiene las caracteristicas adecuadas para crecer bien en este lugar. Antes de comenzar, asegurate de que el area donde plantaras tenga suficiente espacio. Se recomienda que este a una distancia de 5 a 8 metros de otras plantas:'), nl,
  write('1.- Marca con una varilla el lugar donde ira el arbol y usa una azada para remover la hierba.'), nl,
  write('2.- Con una pala, termina de retirar la hierba.'), nl,
  write('3.- Usa un pico para comenzar a cavar el hoyo. Continua con la pala. El hoyo debe tener aproximadamente 50 cm de profundidad (esto puede variar segun la profundidad de las raices de la planta).'), nl,
  write('4.- Saca la planta de la bolsa y dejala en agua durante 15 minutos (preferentemente en una cubeta).'), nl,
  write('5.- Mientras la planta esta en el agua, puedes colocar un poco de abono en el hoyo, como una mezcla de turba y humus.'), nl,
  write('6.- Despues de 15 minutos, saca la planta del agua e introducela en el hoyo.'), nl,
  write('7.- Cubre las raices con tierra y un poco de abono. Nota: Asegurate de no cubrir el injerto del tronco. Pisa suavemente la tierra para asegurar la profundidad correcta.'), nl,
  write('8.- Una vez cubierto el hoyo, vierte el agua que usaste para remojar la planta en el area del hoyo.'), nl,
  write('9.- Si es necesario, coloca una estaca junto al tronco para evitar que el viento lo derribe. Usa un mazo para enterrar la estaca y amarra el tronco a ella con una cinta.'), nl, nl.
mensaje_solucion_instrucciones_plantar :-
  solucion_instrucciones_plantar(2),
  write('* CON RESPECTO A INFORMACION DE COMO PLANTAR: '), nl,
  write('No esperes resultados perfectos debido a las caracteristicas de la planta. Antes de comenzar, asegurate de que el area donde plantaras tenga suficiente espacio. Se recomienda que este a una distancia de 5 a 8 metros de otras plantas:'), nl,
  write('1.- Marca con una varilla el lugar donde ira el arbol y usa una azada para remover la hierba.'), nl,
  write('2.- Con una pala, termina de retirar la hierba.'), nl,
  write('3.- Usa un pico para comenzar a cavar el hoyo. Continua con la pala. El hoyo debe tener aproximadamente 50 cm de profundidad (esto puede variar segun la profundidad de las raices de la planta).'), nl,
  write('4.- Saca la planta de la bolsa y dejala en agua durante 15 minutos (preferentemente en una cubeta).'), nl,
  write('5.- Mientras la planta esta en el agua, puedes colocar un poco de abono en el hoyo, como una mezcla de turba y humus.'), nl,
  write('6.- Despues de 15 minutos, saca la planta del agua e introducela en el hoyo.'), nl,
  write('7.- Cubre las raices con tierra y un poco de abono. Nota: Asegurate de no cubrir el injerto del tronco. Pisa suavemente la tierra para asegurar la profundidad correcta.'), nl,
  write('8.- Una vez cubierto el hoyo, vierte el agua que usaste para remojar la planta en el area del hoyo.'), nl,
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
  write('Esta todo correcto.'), nl.
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
  write('- Mejorar la ventilacion del arbol.'), nl, nl,
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
  write('- Evitar heridas en el arbol durante la temporada de crecimiento.'), nl, nl,
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
  write('- Aplicar insecticidas especificos como spinosad o lufenuron.'), nl,
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
  write('Es probable que tu manzano tenga la plaga arania roja (Panonychus ulmi).'), nl, nl,
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
  write('- Aplicar insecticidas especificos como spinosad.'), nl, 
  write('- Recolectar hojas y frutos daniados.'), nl, nl,
  write('* CON RESPECTO A ESPECIFICIDAD:'), nl,
  write('- General, pero mas comun en climas templados.'), nl, nl,
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

% --- Leer la opcion del usuario ---
leer_opcion :-
  read(Opcion),
  manejar_opcion(Opcion).

% --- Manejar la opcion del usuario ---
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
  flujo_cuidado_suelo,
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