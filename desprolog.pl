%vende(Empresa, EstiloDeViaje, Destino).
vende(mcs, crucero(10), rioDeJaneiro).
vende(mcs, crucero(20), mykonos).
vende(vacaviones, allInclusive(burjAlArab), dubai).
vende(vacaviones, allInclusive(wyndhamPlayaDelCarmen), playaDelCarmen).
vende(moxileres, mochila([carpa, bolsaDeDormir, linterna]), elBolson).
vende(moxileres, mochila([camara, cantimplora, protectorSolar, malla]), puntaDelDiablo).
vende(tuViaje, clasico(primavera, avion), madrid).
vende(tuViaje, clasico(verano, micro), villaGesell).

%crucero(CantidadDeDias).
%allInclusive(Hotel).
%mochila(Objetos).
%clasico(Temporada, MedioDeTransporte).

%continente(Destino, Continente).
continente(rioDeJaneiro, sudAmerica).
continente(mykonos, europa).
continente(dubai, asia).
continente(playaDelCarmen, centroAmerica).
continente(puntaDelDiablo, sudAmerica).
continente(sydney, oceania).
continente(madagascar, africa).
continente(madrid, europa).

%moneda(Destino, Moneda).
moneda(rioDeJaneiro, real).
moneda(miami, dolar).
moneda(shenzhen, renminbi).

%cambioAPesos(Moneda, Conversion).
cambioAPesos(real, 11).
cambioAPesos(dolar, 44).
cambioAPesos(pesoMexicano, 2).
cambioAPesos(ariaryMalgache, 0.012).

%
%----- PUNTO 1
%

viajaA(Empresa,Continente):-
    vende(Empresa,_,Destino),
    continente(Destino,Continente).


% ---- PUNTO 2

esMasEconomico(Destino1,Destino2):-
    cambioAPesosDeUnDestino(Destino1,Cambio1),
        cambioAPesosDeUnDestino(Destino2,Cambio2),
    Cambio1 < Cambio2.

cambioAPesosDeUnDestino(Destino,Cambio):-
    moneda(Destino,Moneda),
    cambioAPesos(Moneda,Cambio).

% ---- PUNTO 3

conviene(Destino):-
    cambioAPesosDeUnDestino(Destino,Cambio),
    Cambio < 1,
    not(continente(Destino,europa)).

% ---- PUNTO 4

elDestinoMasPopular(Destino,Empresa):-
    vende(Empresa,_,Destino),
    forall((vende(Empresa,_,OtroDestino),Destino \= OtroDestino),esMasEconomico(Destino,OtroDestino)).

% ---- PUNTO 5

ventaExtravagante(Destino):-
    vende(_,EstiloDeViaje,Destino), % Lo ligo para poder hacer polimorfismo despues
    not(conviene(Destino)),
    esAsiElViajeA(EstiloDeViaje). % aca uso el polimorfismo para usar cualquier tipo de viaje "hago como una funcion de orden superior"

esAsiElViajeA(crucero(Duracion)):-
    Duracion > 365.

esAsiElViajeA(mochila([])).

esAsiElViajeA(allInclusive(pangu)).
esAsiElViajeA(allInclusive(burjAlArab)).

% ---- PUNTO 6


nivelExtravagancia(Nivel,Empresa):-
    vende(Empresa,_,_),
    findall(Destino,(vende(Empresa,_,Destino),ventaExtravagante(Destino)),VentasExtravagantes),
    length(VentasExtravagantes,CantidadVentasExtravagantes),
    Nivel is CantidadVentasExtravagantes * 8.

% ----- PUNTO 7

% AGREGAR ESTA INFO A LA BASE DE CONOCIMIENTOS

/*

la empresa destina3 va a hacer viajes clásicos en avión a Londres (Europa) y Filadelfia (Norte América). 
Londres tiene como moneda la libra cuyo cambio a pesos es $54 y dicen que la mejor época para viajar es en otoño. 
En cambio, a Filadelfia es preferible viajar en invierno.
pdepViajes es una empresa que le encantaría algún día hacer un viaje en crucero de 15 días por Amalfi pero no lo hace aún. 
Lo que sí ofrece son viajes con mochila a Colombo, en Sri Lanka, 
donde tiene como moneda la rupia pero no sabemos su conversión a pesos argentinos. 
Nos comentan que sí o sí es necesario llevar a ese destino repelente de insectos, un adaptador eléctrico,
una malla y un bucito para la noche.

*/

   