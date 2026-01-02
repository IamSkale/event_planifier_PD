:- module(charging, [cargar_todo/0, limpiar_lista_recursos/2]).

:- use_module(data).
:- use_module(utils, [string_trim/2, leer_linea_archivo/2]).

% ========== CARGA DE DATOS ==========
cargar_todo :-
    cargar_eventos,
    cargar_recursos,
    cargar_inventario.

cargar_eventos :-
    data:archivo_eventos(Archivo),
    (exists_file(Archivo) ->
        limpiar_eventos,
        setup_call_cleanup(
            open(Archivo, read, Stream),
            leer_eventos_desde_stream(Stream),
            close(Stream)
        )
    ;
        format('   ⚠️  No se encontró ~w, se iniciará vacío~n', [Archivo]),
        limpiar_eventos
    ).

leer_eventos_desde_stream(Stream) :-
    data:leer_linea_archivo(Stream, Linea),
    (Linea == end_of_file -> 
        true
    ;
        (Linea \= "" ->
            procesar_linea_evento(Linea)
        ;
            true
        ),
        leer_eventos_desde_stream(Stream)
    ).

cargar_recursos :-
    data:archivo_recursos(Archivo),
    (exists_file(Archivo) ->
        limpiar_recursos,
        setup_call_cleanup(
            open(Archivo, read, Stream),
            leer_recursos_desde_stream(Stream),
            close(Stream)
        )
    ;
        format('   ⚠️  No se encontró ~w, se iniciará sin recursos~n', [Archivo]),
        limpiar_recursos
    ).

leer_recursos_desde_stream(Stream) :-
    findall(Nombre, data:mi_evento(Nombre, _), EventosOrden),
    leer_recursos_por_evento(Stream, EventosOrden, 1).

leer_recursos_por_evento(_, [], _).
leer_recursos_por_evento(Stream, [Nombre|RestoEventos], NumLinea) :-
    data:leer_linea_archivo(Stream, Linea),
    (Linea == end_of_file -> 
        format('   ⚠️  Fin de archivo alcanzado en línea ~d~n', [NumLinea])
    ;
        (Linea = "" ->
            % Línea vacía = sin recursos
            Recursos = []
        ;
            string_trim(Linea, LineaTrim),
            (LineaTrim = "" ->
                Recursos = []
            ;
                split_string(LineaTrim, ",", "", RecursosRaw),
                limpiar_lista_recursos(RecursosRaw, Recursos)
            )
        ),
        (Recursos = [] -> 
            true
        ;
            assertz(data:mis_recursos(Nombre, Recursos))
        ),
        NumLinea1 is NumLinea + 1,
        leer_recursos_por_evento(Stream, RestoEventos, NumLinea1)
    ).

cargar_inventario :-
    data:archivo_inventario(Archivo),
    (exists_file(Archivo) ->
        retractall(data:recurso_inventario(_, _)),
        setup_call_cleanup(
            open(Archivo, read, Stream),
            leer_inventario_desde_stream(Stream),
            close(Stream)
        )
    ;
        format('   ⚠️  No se encontró ~w, inventario vacío~n', [Archivo]),
        retractall(data:recurso_inventario(_, _))
    ).

leer_inventario_desde_stream(Stream) :-
    data:leer_linea_archivo(Stream, Linea),
    (Linea == end_of_file ->
        true
    ;
        (Linea \= "" ->
            string_trim(Linea, T),
            (T \= "" -> 
                split_string(T, " ", "", Partes),
                (Partes = [Nombre, CantStr] ->
                    (atom_number(CantStr, Cant), integer(Cant), Cant >= 0 ->
                        (data:recurso_inventario(Nombre, _) -> true ; assertz(data:recurso_inventario(Nombre, Cant)))
                    ; 
                        format('   ⚠️  Línea mal formada en inventario: ~w~n', [Linea])
                    )
                ; 
                    format('   ⚠️  Línea mal formada en inventario: ~w~n', [Linea])
                )
            ; true)
        ; true),
        leer_inventario_desde_stream(Stream)
    ).

contar_inventario(Total) :-
    findall(Cant, data:recurso_inventario(_, Cant), Lista),
    sum_list(Lista, Total).

procesar_linea_evento(Linea) :-
    split_string(Linea, "|", "", Partes),
    (Partes = [Nombre, Fecha] ->
        (data:mi_evento(Nombre, Fecha) ->
            true
        ;
            assertz(data:mi_evento(Nombre, Fecha))
        )
    ;
        format('   ⚠️  Línea mal formada: ~w~n', [Linea])
    ).

limpiar_eventos :-
    retractall(data:mi_evento(_, _)).

limpiar_recursos :-
    retractall(data:mis_recursos(_, _)).

contar_eventos(Total) :-
    findall(_, data:mi_evento(_, _), Lista),
    length(Lista, Total).

contar_recursos(Total) :-
    findall(_, data:mis_recursos(_, _), Lista),
    length(Lista, Total).

limpiar_lista_recursos([], []).
limpiar_lista_recursos([R|Resto], Limpios) :-
    string_trim(R, Rlimpio),
    (Rlimpio = "" ->
        limpiar_lista_recursos(Resto, Limpios)
    ;
        split_string(Rlimpio, " ", "", Partes),
        (Partes = [Nombre, CantStr] ->
            (atom_number(CantStr, Cant), integer(Cant), Cant > 0 ->
                limpiar_lista_recursos(Resto, RestoLimpios),
                Limpios = [[Nombre, Cant]|RestoLimpios]
            ;
                limpiar_lista_recursos(Resto, Limpios)
            )
        ;
            limpiar_lista_recursos(Resto, Limpios)
        )
    ).