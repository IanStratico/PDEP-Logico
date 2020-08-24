%persona(Apodo, Edad, Peculiaridades).
persona(ale, 15, [claustrofobia, cuentasRapidas, amorPorLosPerros]).
persona(agus, 25, [lecturaVeloz, ojoObservador, minuciosidad]).
persona(fran, 30, [fanDeLosComics]).
persona(rolo, 12, []).

%esSalaDe(NombreSala, Empresa).
esSalaDe(elPayasoExorcista, salSiPuedes).
esSalaDe(socorro, salSiPuedes).
esSalaDe(linternas, elLaberintoso).
esSalaDe(guerrasEstelares, escapepepe).
esSalaDe(fundacionDelMulo, escapepepe).
esSalaDe(estrellasDePelea,supercelula).
esSalaDe(choqueDeLaRealeza,supercelula).
esSalaDe(miseriaDeLaNoche,skpista).


%terrorifica(CantidadDeSustos, EdadMinima).
%familiar(Tematica, CantidadDeHabitaciones).
%enigmatica(Candados).

%sala(Nombre, Experiencia).
sala(elPayasoExorcista, terrorifica(100, 18)).
sala(socorro, terrorifica(20, 12)).
sala(linternas, familiar(comics, 5)).
sala(guerrasEstelares, familiar(futurista, 7)).
sala(fundacionDelMulo, enigmatica([combinacionAlfanumerica, deLlave, deBoton])).
sala(estrellasDePelea,familiar(videojuegos,7)).
sala(choqueDeLaRealeza,familiar(videojuegos)).
sala(miseriaDeLaNoche,terrorifica(150,21)).



nivelDeDificultadDeLaSala(Sala,Dificultad):-
    sala(Sala,TipoDeSala),
    esDeTipo(TipoDeSala,Dificultad).

esDeTipo(terrorifica(CantidadDeSustos,EdadMinima),Dificultad):-
    Dificultad is CantidadDeSustos - EdadMinima.
esDeTipo(familiar(futurista,_),15).
esDeTipo(familiar(Tematica,CantidadDeHabitaciones),CantidadDeHabitaciones):-
    Tematica \= futurista.
esDeTipo(enigmatica(Candados),Dificultad):-
    length(Candados, Dificultad).
    
puedeSalir(Persona,Sala):-
    persona(Persona,_,Caracteristicas),
    not(member(claustrofobia, Caracteristicas)),
    nivelDeDificultadDeLaSala(Sala,Dificultad),
    Dificultad == 1.
puedeSalir(Persona,Sala):-
    persona(Persona,Edad,Caracteristicas),
    not(member(claustrofobia, Caracteristicas)),
    Edad > 13,
    nivelDeDificultadDeLaSala(Sala,Dificultad),
    Dificultad < 5.

tieneSuerte(Persona,Sala):-
    puedeSalir(Persona,Sala),
    persona(Persona,_,[]).

esMacabra(Empresa):-
    esSalaDe(_,Empresa),
    forall(esSalaDe(Sala,Empresa),sala(Sala,terrorifica(_,_))).


empresaCopada(Empresa):-
    esSalaDe(_,Empresa),
    not(esMacabra(Empresa)),
    cantidadDeSalasDeEmpresa(Empresa,CantidadDeSalas),
    sumaDeDificultadesSalasDeEmpresa(Empresa,TotalDeDificultades),
    TotalDeDificultades / CantidadDeSalas < 4.

cantidadDeSalasDeEmpresa(Empresa,CantidadDeSalas):-
    findall(Sala,esSalaDe(Sala,Empresa),Salas),
    length(Salas,CantidadDeSalas).    

sumaDeDificultadesSalasDeEmpresa(Empresa,TotalDeDificultades):-
    findall(Dificultad,(esSalaDe(Sala,Empresa),nivelDeDificultadDeLaSala(Sala,Dificultad)),Dificultades),
    sum_list(Dificultades, TotalDeDificultades).

