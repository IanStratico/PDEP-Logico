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
    not(esPersonaClaustrofobica(Caracteristicas)),
    nivelDeDificultadDeLaSala(Sala,1).% CORREGIDO % tenia que usar pattern matchig, osea no decir dificultad == 1 y poner directamente el 1 
   % Dificultad == 1. CORREGIDO% borrar la palabra dificultad de la linea de arriba y poner un 1 en su lugar, ademas borrar esta linea, es innecesaria
puedeSalir(Persona,Sala):-
    persona(Persona,Edad,Caracteristicas),
    not(esPersonaClaustrofobica(Caracteristicas)),% CORREGIDO % hacer una consulta que para abstraer si una persona es claustrofobica para evitar rep logica
    Edad > 13, % Esto queda asi, no me rompan las pelotas% hacer una abstraccion de las condiciones de edad y dificultad para no repetir logica (ver solucion sugerida)
    nivelDeDificultadDeLaSala(Sala,Dificultad),
    Dificultad < 5.

esPersonaClaustrofobica(Caracteristicas):-
    member(claustrofobia,Caracteristicas).

tieneSuerte(Persona,Sala):-
    puedeSalir(Persona,Sala),
    persona(Persona,_,[]).

esMacabra(Empresa):-
    esSalaDe(_,Empresa),
    forall(esSalaDe(Sala,Empresa),sala(Sala,terrorifica(_,_))).


empresaCopada(Empresa):-
    esSalaDe(_,Empresa),
    not(esMacabra(Empresa)),
    sumaDeDificultadesSalasDeEmpresa(Empresa,TotalDeDificultades,CantidadDeSalas),
    (TotalDeDificultades / CantidadDeSalas) < 4.

% cantidadDeSalasDeEmpresa(Empresa,CantidadDeSalas):- % este findall no es necesario
%     findall(Sala,esSalaDe(Sala,Empresa),Salas),
%     length(Salas,CantidadDeSalas). CORREGIDO    

sumaDeDificultadesSalasDeEmpresa(Empresa,TotalDeDificultades,CantidadDeSalas):- % CORREGIDO % si hago un lenght de dificultades tambien haria referencia a la cantidad de salas
    findall(Dificultad,(esSalaDe(Sala,Empresa),nivelDeDificultadDeLaSala(Sala,Dificultad)),Dificultades),
    length(Dificultades,CantidadDeSalas),
    sum_list(Dificultades, TotalDeDificultades). % total de salas ya que cada sala tiene 1 dificultad, por eso esta demas el otro findall

