vive(tiaAghata,mansionDreadbury).
vive(carnicero,mansionDreadbury).
vive(charles,mansionDreadbury).

odia(charles,Persona2):-
    vive(Persona2,mansionDreadbury),
    not(odia(tiaAghata,Persona2)).

odia(carnicero,Persona2):-
    odia(tiaAghata,Persona2).

odia(tiaAghata,Persona2):-
    vive(Persona2,mansionDreadbury),  
    Persona2 \= carnicero.

mata(Asesino,Victima):-
    odia(Asesino,Victima),
    not(esMasRico(Asesino,Victima)),
    vive(Asesino,mansionDreadbury).

esMasRico(Persona1,tiaAghata):-
    not(odia(carnicero,Persona1)),
    vive(Persona1,mansionDreadbury).

/*   

?- mata(Asesino,tiaAghata).  
false.

---2---

?- odia(_,milhouse).
false.

?- odia(charles,Alguien).
Alguien = carnicero ;
false.

?- odia(Alguien,tiaAghta).
false.

?- odia(Alguien,Otro).
Alguien = charles,
Otro = carnicero ;
Alguien = carnicero,
Otro = tiaAghata ;
Alguien = carnicero,
Otro = charles ;
Alguien = Otro, Otro = tiaAghata ;
Alguien = tiaAghata,
Otro = charles.

?- odia(carnicero,_).
true.

*/