%principal(Nombre,Calorias)
%entrada(Nombre,[Ingredientes],Calorias)
%postre(Nombre,SaborPrincipal,Calorias)

%cocina(nombre, plato, puntos)
cocina(mariano, principal(nioquis, 50), 80).
cocina(julia, principal(pizza, 100), 60).
cocina(hernan, postre(panqueque, dulceDeLeche, 100), 60).
cocina(hernan, postre(trufas, dulceDeLeche, 60), 80).
cocina(hernan, entrada(ensalada, [tomate, zanahoria, lechuga], 70), 29).
cocina(susana, entrada(empanada, [carne, cebolla, papa], 50), 50).
cocina(susana, postre(pastelito, dulceDeMembrillo, 50), 60).
cocina(melina, postre(torta, zanahoria, 60),50).

%sonAmigos(Persona,OtraPersona)
sonAmigos(mariano,susana).
sonAmigos(mariano,hernan).
sonAmigos(hernan,pedro).
sonAmigos(melina,carlos).
sonAmigos(carlos,susana).

%esPopular(Ingrediente)
esPopular(carne).
esPopular(dulceDeLeche).
esPopular(dulceDeMembrillo).

esComidaSaludable(Comida):-
    cocina(_,Comida,_),
    esDeTipo(Comida).

esDeTipo(entrada(_,_,Calorias)):-Calorias =< 60.
esDeTipo(principal(_,Calorias)):- 
    Calorias > 70,
    Calorias < 90.
esDeTipo(postre(_,_,Calorias)):- Calorias < 100.

soloSalado(Cocinero):-
    cocina(Cocinero,_,_),
    not(cocina(Cocinero,postre(_,_,_),_)).

tieneUnaGranFama(Cocinero):-
    cocina(Cocinero,_,_),
    nivelDeFama(Cocinero,Fama),
    forall(nivelDeFama(_,UnaFama),UnaFama =< Fama).

nivelDeFama(Cocinero,Fama):-
    cocina(Cocinero,_,_),
    findall(Puntos,cocina(Cocinero,_,Puntos),TodosLosPuntos),
    sum_list(TodosLosPuntos, Fama).

noEsSaludable(Cocinero):-
    cocina(Cocinero,Comida,_),
    esComidaSaludable(Comida),
    forall((cocina(Cocinero,OtraComida,_), Comida \= OtraComida),not(esComidaSaludable(Comida))).

