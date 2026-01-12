:- module(saving, [guardar_todo/0, recurso_a_string/2]).

:- use_module(data).

% ========== GUARDAR DATOS ==========
guardar_todo :-
    guardar_eventos,
    guardar_recursos.

guardar_eventos :-
    data:archivo_eventos(Archivo),
    setup_call_cleanup(
        open(Archivo, write, Stream),
        escribir_eventos_a_stream(Stream),
        close(Stream)
    ).

guardar_recursos :-
    data:archivo_recursos(Archivo),
    setup_call_cleanup(
        open(Archivo, write, Stream),
        escribir_recursos_a_stream(Stream),
        close(Stream)
    ).

escribir_eventos_a_stream(Stream) :-
    findall([Nombre, Fecha, Duracion], data:mi_evento(Nombre, Fecha, Duracion), EventosAll),
    list_to_set(EventosAll, Eventos),
    forall(
        member([Nombre, Fecha, Duracion], Eventos),
        format(Stream, '~w|~w|~w~n', [Nombre, Fecha, Duracion])
    ).

escribir_recursos_a_stream(Stream) :-
    findall(Nombre, data:mi_evento(Nombre, _, _), EventosAll),
    list_to_set(EventosAll, EventosOrden),
    escribir_recursos_por_orden(Stream, EventosOrden).

escribir_recursos_por_orden(_, []).
escribir_recursos_por_orden(Stream, [Nombre|Resto]) :-
    (data:mis_recursos(Nombre, Recursos), Recursos \= [] ->
        maplist(recurso_a_string, Recursos, RecursosStrList),
        atomic_list_concat(RecursosStrList, ',', RecursosStr),
        format(Stream, '~w~n', [RecursosStr])
    ;
        format(Stream, '~n', [])
    ),
    escribir_recursos_por_orden(Stream, Resto).

recurso_a_string([Nombre, Cant], Str) :-
    format(atom(Str), '~w ~w', [Nombre, Cant]).