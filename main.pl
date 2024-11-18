:- dynamic known/3.

% Predicados necesarios
multivalued(soporte).
multivalued(rango_temperatura).
multivalued(riego).
multivalued(rango_horas_luz_solar).

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

booleanask(A, V):- 
  known(si, A, V),
  !.  
booleanask(A, V):- 
  known(si, A, _),
  !, 
  fail. 
booleanask(A, V):-
  format('~w? (si/no): ', [A]),
  read(S),
  (S == si ; S == no),
  save_booleanask(S, A, V).
save_booleanask(S, A, V) :-
  S == V,
  asserta(known(si, A, S)).
save_booleanask(S, A, V) :-
  S \== V,
  asserta(known(si, A, S)),
  !,
  fail.

% menuask(rango_temperatura, 2, [...])

menuask(A, V, MenuList) :- 
  known(si, A, V),  % succeed if true 
  !.  % stop looking  
menuask(A, V, MenuList) :- 
  known(_, A, V),  % fail if false 
  !, 
  fail. 
menuask(A, V, MenuList) :- 
  multivalued(A),  
  known(si, A, V2),
  V \== V2,
  !,
  fail.
menuask(A, V, MenuList) :- 
  multivalued(A),  
  known(si, A, V2),
  V == V2,
  !.
menuask(A, V, MenuList) :- 
  write('Cual es el valor para '), write(A), write('?'), nl,  
  write(MenuList), nl,  
  read(X),  
  check_val(X, A, V, MenuList),  
  save_menuask(X, A, V).

save_menuask(X, A, V) :-
  X == V,
  asserta(known(si, A, X)),
  !.
save_menuask(X, A, V) :-
  X \== V,
  asserta(known(si, A, X)),
  !,
  fail.

check_val(X, A, V, MenuList) :- 
  member(X, MenuList), 
  !. 
check_val(X, A, V, MenuList) :- 
  write(X), write(' no es una opcion disponible. Intenta nuevamente.'), nl,  
  menuask(A, V, MenuList).

% Peticiones necesarias

sequedad_tierra(X) :- booleanask(sequedad_tierra, X).
acumulacion_despues_riego(X) :- booleanask(acumulacion_despues_riego, X).
suelo_humedo(X) :- booleanask(suelo_humedo, X).
soporte(X) :- menuask(soporte, X, [suelo, maceta]). % maceta o suelo

rango_temperatura(X) :-
  menuask(rango_temperatura, X, [1, 2, 3, 4, 5, 6]).
% 1: Muy frio - menos de 0 grados
% 2: Frío - entre 0 y 15 grados
% 3: Templado - entre 15 y 25 grados
% 4: Cálido - entre 25 y 35 grados
% 5: Muy caliente - entre 35 y 40 grados
% 6: Extremadamente caliente - mas de 40 grados

riego(X) :-
  menuask(riego, X, [1, 2, 3, 4, 5, 6, 7]).
% 1. No he regado en absoluto (0 veces)
% 2. 1 vez
% 3. 2 veces
% 4. 3 veces
% 5. 4 veces
% 6. 5 veces
% 7.  Más de 5 veces

rango_horas_luz_solar(X) :-
  menuask(rango_horas_luz_solar, X, [1, 2, 3, 4, 5, 6]).
% 1. Menos de 2 horas
% 2. 2 a 4 horas
% 3. 4 a 6 horas
% 4. 6 a 8 horas
% 5. 8 a 10 horas
% 6. Más de 10 horas

% Flujos riego/luz solar
flujo_riego_luz_solar :-
  write('Para recomendarte los mejores consejos en riego y luz solar, necesito preguntar algunas cosas:'), nl, nl,
  solucion_riego(X),
  solucion_encharcamiento(Y),
  solucion_luz_solar(Z),
  nl, nl, write('A continuacion, la respuesta del flujo:'), nl, nl,
  write('* Con respecto a riego: '), nl,
  mensaje_solucion_riego, nl,
  write('* Con respecto a encharcamiento: '), nl,
  mensaje_solucion_encharcamiento, nl,
  write('* Con respecto a luz solar: '), nl,
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
  write('En tiempo de frio, se recomienda un manzano se riegue, por lo menos, 2 veces a la semana. Por lo tanto, riega una vez más en lo que queda de la semana.'), nl.
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


% Base de conocimiento

% --- Interfaz principal ---
inicio :-
  nl,
  write('Bienvenido al sistema experto para principiantes en manzanos!!'), nl,
  write('--- Acciones ---'), nl,
  write('Introduce un numero para:'), nl,
  write('1. Cultivo'), nl,
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
  nl, write('Seleccionaste: Cultivo'), nl,
  inicio.

manejar_opcion(2) :-
  nl, write('Seleccionaste: Control de plagas y enfermedades'), nl,
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
  nl, write('Borrando memoria...'), nl,
  inicio.

manejar_opcion(6) :-
  nl, write('Saliendo... Adios!'), nl.

manejar_opcion(_) :-
  nl, write('Error, intenta de nuevo.'), nl,
  inicio.