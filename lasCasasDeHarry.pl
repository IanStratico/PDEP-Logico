
%sangre(mago,tipoDeSangre)
sangre(harry,mestiza).
sangre(draco,pura).
sangre(hermione,impura).


mago(Mago):-
    sangre(Mago,_).
mago(ron).
mago(luna).

%caracteristicasMago(mago,listaDeCaracteristicas)
caracteristicasMago(harry,[corajudo,amistoso,inteligente,orgulloso]).
caracteristicasMago(draco,[inteligente,orgulloso]).
caracteristicasMago(hermione,[inteligente,orgulloso,responsable]).

%caracteristicaBuscada(casa,caracteristica)
caracteristicaCasa(gryffindor,corajudo).
caracteristicaCasa(slytherin,orgulloso).
caracteristicaCasa(slytherin,inteligente).
caracteristicaCasa(ravenclaw,inteligente).
caracteristicaCasa(ravenclaw,responsable).
caracteristicaCasa(hufflepuff,amistoso).

%odia(mago,casa)
odia(harry,slytherin).
odia(draco,hufflepuff).

%casa(nombre)
casa(gryffindor).
casa(slytherin).
casa(ravenclaw).
casa(hufflepuff).

% ------ 1 -------

permiteEntrar(Casa,Mago):-
    mago(Mago),
    casa(Casa),
    Casa \= slytherin.
permiteEntrar(slytherin,Mago):-
    sangre(Mago,TipoDeSangre),
    TipoDeSangre \= impura.

% -------- 2 --------

caracterApropiadoParaEntrar(Mago,Casa):-
    casa(Casa),
    mago(Mago),
    forall(caracteristicaCasa(Casa,Caracteristica),tieneCaracteristica(Mago,Caracteristica)).

tieneCaracteristica(Mago,Caracteristica):-
    caracteristicasMago(Mago,ListaCaracteristicas),
    member(Caracteristica,ListaCaracteristicas).

% -------- 3 --------

quedaEn(Mago,Casa):-
    caracterApropiadoParaEntrar(Mago,Casa),
    permiteEntrar(Casa,Mago),
    not(odia(Mago,Casa)).
quedaEn(hermione,gryffindor).

% ------- 4 --------

% ESTO NO LO VI JAMAS EN  LA CURSADA NOS VEMO NEMO, QUE CARAJO ES RECURSIVIDAD EN PROLOG O LA FUNCION NTH1?????????

% ----- Parte 2 -------

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).


%accion(mago,accion,puntos)
accion(harry,fueraDeLaCama,-50).
accion(hermione,irA(tercerPiso),-75).
accion(hermione,irA(biblioteca),-10).
accion(harry,irA(bosque),-50).
accion(harry,irA(tercerPiso),-75).
accion(draco,irA(mazmorras),0).
accion(ron,buenaAccion(ajedrez),50).
accion(hermione,buenaAccion(salvarAmigos),50).
accion(harry,buenaAccion(ganarleAVoldemort),60).

% -------- 1 -------

esBuenAlumno(Mago):-
    hizoAlgunaAccion(Mago),
    not(hizoAlgoMalo(Mago)).

hizoAlgunaAccion(Mago):-
    accion(Mago,_,_).

hizoAlgoMalo(Mago):-
    accion(Mago,_,Puntos),
    Puntos < 0.

accionRecurrente(Accion):-
    accion(_,Accion,_),
    findall(Mago,accion(Mago,Accion,_),ListaMagos),
    length(ListaMagos, CantidadMagos),
    CantidadMagos > 1.


% ------- 2 -------

puntajeTotal(Casa,CantidadDePuntos):-
    casa(Casa),
    findall(Puntos,(esDe(Mago,Casa),accion(Mago,_,Puntos)),ListaPuntos),
    sum_list(ListaPuntos,CantidadDePuntos).

% ------ 3 -------

casaGanadora(Casa):- % POR AlGUNA RAZON NO ME ANDA ESTA SOLUCION PERO ES LA MISMA QUE EL VIDEITO (HICE DISTINTO EL RESTO DE LOS PUNTOS)
    puntajeTotal(Casa, PuntajeMayor), % TAL VEZ ES POR ESO QUE ALGO ROMPE DENTRO DEL FINDALL DE VERGA
    forall((puntajeTotal(OtraCasa, PuntajeMenor), Casa \= OtraCasa), PuntajeMayor > PuntajeMenor).
