/*

Consignas:

https://docs.google.com/document/d/1bblUbyuVNoGQKDRVq0usSkHEIts7WXNrkTMQlnkJC9w/edit#

*/


% -----
% -----     PUNTO 1
% -----



partido(frank, rojo).
partido(claire, rojo).
partido(garrett, azul).
partido(jackie, amarillo).
partido(linda, azul).
partido(catherine, rojo).
partido(seth, amarillo).
partido(heather, amarillo).

% Peter no es candidato del partido amarillo. No se expresa en la base de conocimientos ya que solo tenemos en cuenta aquellas cosas que consideramos verdaderas, no las que consideramos que no lo son.

% El partido violeta no tiene candidatos. No se expresa en la base de conocimientos
% porque solo tenemos en cuenta los candidatos que son de un partido (los partidos que tienen al menos un candidato).
% Se podría obtener que no hay ningún candidato del partido violeta haciendo la consulta
% partido(_, violeta) -> false.

edad(frank, 50).
edad(claire, 52).
edad(garrett, 64).
edad(peter, 26).
edad(jackie, 38).
edad(linda, 30).
edad(catherine, 59).

% Heather nació en 1969. No lo tenemos en cuenta ya que el parámetro que tomamos con los candidatos es la edad, no el año de nacimiento.
% Al día de hoy, podría tener 50 o 51 años, y no podemos asumir uno de ellos.


% -----------------------------------------


sePostulaEn(azul, buenosAires).
sePostulaEn(azul, chaco).
sePostulaEn(azul, tierraDelFuego).
sePostulaEn(azul, sanLuis).
sePostulaEn(azul, neuquen).

sePostulaEn(rojo, buenosAires).
sePostulaEn(rojo, santaFe).
sePostulaEn(rojo, cordoba).
sePostulaEn(rojo, chubut).
sePostulaEn(rojo, tierraDelFuego).
sePostulaEn(rojo, sanLuis).

sePostulaEn(amarillo, chaco).
sePostulaEn(amarillo, formosa).
sePostulaEn(amarillo, tucuman).
sePostulaEn(amarillo, salta).
sePostulaEn(amarillo, santaCruz).
sePostulaEn(amarillo, laPampa).
sePostulaEn(amarillo, corrientes).
sePostulaEn(amarillo, misiones).
sePostulaEn(amarillo, buenosAires).


% -----------------------------------------


habitantes(buenosAires, 15355000).
habitantes(chaco, 1143201).
habitantes(tierraDelFuego, 160720).
habitantes(sanLuis, 489255).
habitantes(neuquen, 637913).
habitantes(santaFe, 3397532).
habitantes(cordoba, 3567654).
habitantes(chubut, 577466).
habitantes(formosa, 527895).
habitantes(tucuman, 1687305).
habitantes(salta, 1333365).
habitantes(santaCruz, 273964).
habitantes(laPampa, 349299).
habitantes(corrientes, 992595).
habitantes(misiones, 1189446).



% -----
% -----     PUNTO 2
% -----



esPicante(Provincia):-
    sePostulaEn(UnPartido, Provincia),
    sePostulaEn(OtroPartido, Provincia),
    UnPartido \= OtroPartido,
    habitantes(Provincia, CantidadHabitantes),
    CantidadHabitantes > 1000000.



% -----
% -----     PUNTO 3
% -----
%HOLAS 
intencionDeVotoEn(buenosAires, rojo, 40).
intencionDeVotoEn(buenosAires, azul, 30).
intencionDeVotoEn(buenosAires, amarillo, 30).
intencionDeVotoEn(chaco, rojo, 50).
intencionDeVotoEn(chaco, azul, 20).
intencionDeVotoEn(chaco, amarillo, 0).
intencionDeVotoEn(tierraDelFuego, rojo, 40).
intencionDeVotoEn(tierraDelFuego, azul, 20).
intencionDeVotoEn(tierraDelFuego, amarillo, 10).
intencionDeVotoEn(sanLuis, rojo, 50).
intencionDeVotoEn(sanLuis, azul, 20).
intencionDeVotoEn(sanLuis, amarillo, 0).
intencionDeVotoEn(neuquen, rojo, 80).
intencionDeVotoEn(neuquen, azul, 10).
intencionDeVotoEn(neuquen, amarillo, 0).
intencionDeVotoEn(santaFe, rojo, 20).
intencionDeVotoEn(santaFe, azul, 40).
intencionDeVotoEn(santaFe, amarillo, 40).
intencionDeVotoEn(cordoba, rojo, 10).
intencionDeVotoEn(cordoba, azul, 60).
intencionDeVotoEn(cordoba, amarillo, 20).
intencionDeVotoEn(chubut, rojo, 15).
intencionDeVotoEn(chubut, azul, 15).
intencionDeVotoEn(chubut, amarillo, 15).
intencionDeVotoEn(formosa, rojo, 0).
intencionDeVotoEn(formosa, azul, 0).
intencionDeVotoEn(formosa, amarillo, 0).
intencionDeVotoEn(tucuman, rojo, 40).
intencionDeVotoEn(tucuman, azul, 40).
intencionDeVotoEn(tucuman, amarillo, 20).
intencionDeVotoEn(salta, rojo, 30).
intencionDeVotoEn(salta, azul, 60).
intencionDeVotoEn(salta, amarillo, 10).
intencionDeVotoEn(santaCruz, rojo, 10).
intencionDeVotoEn(santaCruz, azul, 20).
intencionDeVotoEn(santaCruz, amarillo, 30).
intencionDeVotoEn(laPampa, rojo, 25).
intencionDeVotoEn(laPampa, azul, 25).
intencionDeVotoEn(laPampa, amarillo, 40).
intencionDeVotoEn(corrientes, rojo, 30).
intencionDeVotoEn(corrientes, azul, 30).
intencionDeVotoEn(corrientes, amarillo, 10).
intencionDeVotoEn(misiones, rojo, 90).
intencionDeVotoEn(misiones, azul, 0).
intencionDeVotoEn(misiones, amarillo, 0).

% -----------------------------------------
leGanaA(UnCandidato,OtroCandidato,Provincia):-
    partido(UnCandidato,UnPartido),
    partido(OtroCandidato,UnPartido),
    sePostulaEn(UnPartido,Provincia).

leGanaA(UnCandidato,OtroCandidato,Provincia):-
    partido(UnCandidato,UnPartido),
    sePostulaEn(UnPartido,Provincia),
    partido(OtroCandidato,OtroPartido),
    sePostulaEn(OtroPartido,Provincia),
    intencionDeVotoEn(Provincia,UnPartido,UnPorcentaje),
    intencionDeVotoEn(Provincia,OtroPartido,OtroPorcentaje),
    UnPorcentaje \= OtroPorcentaje,
    UnPorcentaje > OtroPorcentaje.

leGanaA(UnCandidato,_,Provincia):-
    partido(UnCandidato,UnPartido),
    sePostulaEn(UnPartido,Provincia).
    
% -----
% -----     PUNTO 4
% -----

hayAlguienMasChicoDeSuPartido(UnCandidato,UnPartido):-
    partido(UnCandidato,UnPartido),
    partido(OtroCandidato,UnPartido),
    edad(UnCandidato,UnaEdad),
    edad(OtroCandidato,OtraEdad),
    UnaEdad > OtraEdad.
noHayAlguienMasChicoDeSuPartido(UnCandidato,UnPartido):-
    not(hayAlguienMasChicoDeSuPartido(UnCandidato,UnPartido)).

masJovenDelPartido(UnCandidato):-
    edad(UnCandidato,_),
   forall(partido(UnCandidato,UnPartido),noHayAlguienMasChicoDeSuPartido(UnCandidato,UnPartido)).

elGranCandidato(UnCandidato):-
    partido(UnCandidato,_),
    forall((partido(UnCandidato,UnPartido),sePostulaEn(UnPartido,Provincia)),(leGanaA(UnCandidato,OtroCandidato,Provincia),masJovenDelPartido(UnCandidato))).
    
    

    

