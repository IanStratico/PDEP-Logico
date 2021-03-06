% mensaje(ListaDePalabras, Receptor).
%	Los receptores posibles son:
%	Persona: un simple átomo con el nombre de la persona; ó
%	Grupo: una lista de al menos 2 nombres de personas que pertenecen al grupo.
mensaje(['hola', ',', 'qué', 'onda', '?'], nico).
mensaje(['todo', 'bien', 'dsp', 'hablamos'], nico).
mensaje(['q', 'parcial', 'vamos', 'a', 'tomar', '?'], [nico, lucas, maiu]).
mensaje(['todo', 'bn', 'dsp', 'hablamos'], [nico, lucas, maiu]).
mensaje(['todo', 'bien', 'después', 'hablamos'], mama).
mensaje(['¿','y','q', 'onda', 'el','parcial', '?'], nico).
mensaje(['¿','y','qué', 'onda', 'el','parcial', '?'], lucas).

% abreviatura(Abreviatura, PalabraCompleta) relaciona una abreviatura con su significado.
abreviatura('dsp', 'después').
abreviatura('q', 'que').
abreviatura('q', 'qué').
abreviatura('bn', 'bien').

% signo(UnaPalabra) indica si una palabra es un signo.
signo('¿').    signo('?').   signo('.').   signo(','). 

% filtro(Contacto, Filtro) define un criterio a aplicar para las predicciones para un contacto
filtro(nico, masDe(0.5)).
filtro(nico, ignorar(['interestelar'])).
filtro(lucas, masDe(0.7)).
filtro(lucas, soloFormal).
filtro(mama, ignorar(['dsp','paja'])).

% ------ 1 --------

recibioMensaje(Persona,Mensaje):-
    mensaje(Mensaje,Persona),
    Persona \= [nico,lucas,maiu].

% ------  2 ------

demasiadoFormal(Mensaje):-
    mensaje(Mensaje,_),
    esFormal(Mensaje).

esFormal(Mensaje):-
    length(Mensaje, LargoMensaje),
    LargoMensaje > 4,
    member(signo(_), Mensaje).

% ----- 3 ------


% LA VERDAD ME ABURRI Y ESTO TIENEN COSAS QUE JAMAS VI COMO LISTAS EN PROLOG ASI QUE LO DEJAMOS ACA Y QUE SEA LO QUE DON DIOS QUIERA