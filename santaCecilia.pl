%banda(Nombre, AnioDeFormacion, Localidad, Integrantes).
banda(finta, 2001, haedo, [ale, juli, gabo, ludo, anuk]).
banda(aloeVera, 2007, cordoba, [lula, bren, gaby]).
banda(losEscarabajos, 1960, losPiletones, [juan, pablo, jorge, ricardo]).
banda(plastica, 1983, palermoHollywood, [jaime, kirk, rober, lars]).
banda(oceania, 1978, lasHeras, [jose, juanito, juancito, mic, ian]).
banda(brazoFuerte, 1914, nuevaOrleans, [luis]).
banda(lasLiendres,1988,palomar,[andy,gus,migue,sebas,juan]). %agregado
banda(monoJugo,1982,buenosAires,[gus,equis,carlitos]). %agregado

%pop(CantidadDeHits, CantidadDeDiscos)
%rock(TipoDeRock, Decada)
%jazz(Instrumentos)

%genero(NombreBanda, Genero).
genero(finta, pop(20, 7)).
genero(losEscarabajos, rock(mixto ,60)).
genero(plastica, rock(heavy, 80)).
genero(oceania, rock(glam, 80)).
genero(brazoFuerte, jazz([trompeta, corneta])).
genero(monoJugo,rock(glam,80)). %agregado

%festivalMusicalEn(NombreFestival, Localidad).
festivalMusical(mangueraMusmanoRockFestival, cordoba). 
festivalMusical(nueveAuxilios, haedo).
festivalMusical(hullabalooza, camposVerdes). %agregado

%bandaConfirmadaPara(Banda, Festival).
bandaConfirmadaPara(finta, mangueraMusmanoRockFestival).
bandaConfirmadaPara(aloeVera, mangueraMusmanoRockFestival).
bandaConfirmadaPara(mariaLaCuerda, mangueraMusmanoRockFestival).
bandaConfirmadaPara(reyesDeLaEraDelHielo, mangueraMusmanoRockFestival).
bandaConfirmadaPara(cantoRodado, nueveAuxilios).
bandaConfirmadaPara(lasLiendres, nueveAuxilios).
bandaConfirmadaPara(juanPrincipe, nueveAuxilios).
bandaConfirmadaPara(fluidoVerde, nueveAuxilios).
bandaConfirmadaPara(losEscarabajos,hullabalooza). %agregado
bandaConfirmadaPara(plastica,hullabalooza). %agregado
bandaConfirmadaPara(oceania,hullabalooza). %agregado

esExitosa(Banda):-
    genero(Banda,Genero),
    esDeGeneroExitoso(Genero).

esDeGeneroExitoso(pop(Hits,Albumes)):-
    PromedioDeHits is Hits / Albumes,
    PromedioDeHits >= 4.
esDeGeneroExitoso(rock(mixto,_)).
esDeGeneroExitoso(rock(glam,80)).
esDeGeneroExitoso(jazz(Instrumentos)):-
    member(trompeta,Instrumentos).

seraEterna(Banda):-
    esExitosa(Banda),
    condicionDeEternidad(Banda).

condicionDeEternidad(Banda):-
    banda(Banda,_,_,Integrantes),
    length(Integrantes,4).

condicionDeEternidad(Banda):-
    banda(Banda,AnioDeFormacion,_,_),
    AnioDeFormacion =< 1980,
    AnioDeFormacion >= 1960.

leConvieneParticipar(Banda,Festival):-
    festivalMusical(Festival, LocalidadDeFormacion),
    banda(Banda,_,LocalidadDeFormacion,_),
    not(bandaConfirmadaPara(Banda, Festival)).

seGraba(Festival):-
    festivalMusical(Festival,_),
    forall(bandaConfirmadaPara(Banda,Festival),seraEterna(Banda)).

anioHistorico(Anio):-
   banda(_,Anio,_,_),
   findall(Banda,banda(Banda,Anio,_,_),Bandas),
   length(Bandas,TotalDeBandas),
   TotalDeBandas > 10. 

% FIN