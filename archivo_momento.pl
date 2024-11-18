% Del libro
ask(A, V):- 
  not multivalued(A),  
  known(si, A, V2),  
  V \== V2,  
  !, 
  fail.


% Mio
ask(A, V):- 
  multivalued(A),  
  known(si, A, _),
  !,
  fail.

menuask(A, V, MenuList):- 
  known(si, A, V),  % succeed if true 
  !.  % stop looking  
menuask(A, V, MenuList):- 
  known(_, A, V),  % fail if false 
  !, 
  fail. 
menuask(A, V, MenuList):- 
  multivalued(A),  
  known(si, A, _),
  !,
  fail.