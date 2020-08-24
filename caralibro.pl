
usuario(leo).
viveEn(leo, direccion(argentina, liniers)).
nacimiento(leo,anios(27)).
tieneAmigo(leo, gaston).
tieneAmigo(leo, gabi).

usuario(gabi).
viveEn(gabi, direccion(argentina, liniers,1407)).
nacimiento(gabi, fecha(30,09,1982)).
tieneAmigo(gabi, leo).
tieneAmigo(gabi,fer).

usuario(fer).
viveEn(fer, direccion(argentina, liniers)).
nacimiento(fer, anios(27)).
tieneAmigo(fer, gabi).

hoy(fecha(2,10,2008)).

grupo(ayudantesParadigmas).
integra(ayudantesParadigmas, leo).
integra(ayudantesParadigmas, gabi).
integra(ayudantesParadigmas, fer). 


% ----- 1 ---------

amigosDe(leo,Amigos):-
    tieneAmigo(leo,Amigos).

edadLeo(leo,Edad):-
    nacimiento(leo,anios(Edad)).

tieneEsteCP(Usuario,Cp):-
    viveEn(Usuario,direccion(_,_,1407)).

% -------- 2 --------

sonAmigos(Usuario,OtroUsuario):-
    tieneAmigo(Usuario,OtroUsuario),
    tieneAmigo(OtroUsuario,Usuario).


% -------  3 --------
amigosQueCumplenEsteMes(Usuario,Amigo):-
    sonAmigos(Usuario,Amigo),
    cumpleaniosEsteMes(Amigo).

cumpleaniosEsteMes(Usuario):-
    nacimiento(Usuario,fecha(_,Mes,_)),
    hoy(fecha(_,Mes,_)).

% ----- 4 -------

talVezConozcas(Usuario,OtroUsuario):-
    seConocenSi(Usuario,OtroUsuario),
    Usuario \= OtroUsuario.

seConocenSi(Usuario,OtroUsuario):-
    viveEn(Usuario,direccion(Pais,Barrio)),
    viveEn(OtroUsuario,direccion(Pais,Barrio)).
seConocenSi(Usuario,OtroUsuario):-
    sonAmigos(Usuario,Alguien),
    sonAmigos(OtroUsuario,Alguien).
seConocenSi(Usuario,OtroUsuario):-
    integra(Grupo,Usuario),
    integra(Grupo,OtroUsuario).

% -------- 5 --------
 esPopular(Usuario):-
    forall(mismoGrupo(Usuario,OtroUsuario),sonAmigos(Usuario,OtroUsuario)).
    
    
mismoGrupo(Usuario,OtroUsuario):-
    integra(Grupo,Usuario),
    integra(Grupo,Usuario),
    Usuario \= OtroUsuario.

losPopulares(Populares):-
    findall(Usuario, esPopular(Usuario), Populares).
    
    
% ------ 6 ------

estanconectados(Usuario,OtroUsuario):-
    sonAmigos(Usuario,Alguien),
    sonAmigos(OtroUsuario,Alguien),
    Usuario \= OtroUsuario.