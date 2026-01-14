:- module(core, [iniciar/0]).

:- use_module(data).
:- use_module(saving, [guardar_todo/0, recurso_a_string/2]).
:- use_module(charging, [cargar_todo/0,limpiar_lista_recursos/2]).
:- use_module(utils, [leer_linea/1, sort_eventos/2]).
:- use_module(dates_work, [validar_fecha/1,fecha_siguiente/2]).
:- use_module(checking, [solicitar_recursos_en_fecha/4, solicitar_recursos_sin_fecha/3]).
:- use_module(restrictions, [check_restrictions/2]).


% ========== INICIALIZAR SISTEMA ==========
iniciar :-
    write('================================'), nl,
    write('         SISTEMA EVENTOS        '), nl,
    write('================================'), nl, nl,
    cargar_todo,
    bucle_principal.

% ========== BUCLE PRINCIPAL ==========
bucle_principal :-
    mostrar_menu,
    leer_opcion(Opcion),
    procesar_opcion(Opcion),
    (Opcion =:= 5 -> 
        write('Saliendo del sistema...'), nl
    ; 
        bucle_principal
    ).

mostrar_menu :-
    write('OPCIONES DISPONIBLES:'), nl,
    write('  1. Ver lista de eventos'), nl,
    write('  2. Ver inventario'), nl,
    write('  3. Ver eventos por fecha'), nl,
    write('  4. Agregar evento con recursos'), nl,
    write('  5. Salir'), nl, nl.

leer_opcion(Opcion) :-
    write('Seleccione una opción (1-5): '), 
    flush_output,
    leer_linea(Input),
    (atom_number(Input, Num), integer(Num), Num >= 1, Num =< 5 -> 
        Opcion = Num
    ;
        write('❌ Opción inválida. Intente de nuevo.'), nl, nl,
        leer_opcion(Opcion)
    ).

procesar_opcion(1) :-
    ver_eventos_con_recursos.
    
procesar_opcion(2) :-
    ver_inventario.

procesar_opcion(3) :-
    ver_eventos_fecha.

procesar_opcion(4) :-
    agregar_evento_con_recursos.

procesar_opcion(5) :-
    guardar_todo,
    write('👋 ¡Hasta luego!'), nl.


% ========== VER EVENTOS ==========
ver_eventos_con_recursos :-
    nl,
    write('================================'), nl,
    write('        LISTA DE EVENTOS        '), nl,
    write('================================'), nl, nl,
    
    findall([Nombre, Fecha, Duracion], data:mi_evento(Nombre, Fecha, Duracion), ListaEventos), 

    (ListaEventos == [] ->
        write('📭 No hay eventos registrados.'), nl
    ;
        sort_eventos(ListaEventos, EventosOrdenados),
        mostrar_eventos_con_recursos(EventosOrdenados, 1), nl,
        length(ListaEventos, Cantidad),
        format('Total: ~d eventos~n', [Cantidad])
    ), nl.

mostrar_eventos_con_recursos([], _).
mostrar_eventos_con_recursos([[Nombre, Fecha, Duracion]|Resto], N) :-
    format('~d. ~w~n', [N, Nombre]),
    format('   📅 Fecha: ~w~n', [Fecha]),
    format('   ⏱️  Duración: ~d días~n', [Duracion]), 
    
    (data:mis_recursos(Nombre, Recursos) ->
        (Recursos == [] -> 
            format('   📦 Sin recursos~n', [])
        ;
            maplist(recurso_a_string, Recursos, RecursosStrList),
            atomic_list_concat(RecursosStrList, ', ', RecursosStr),
            format('   📦 Recursos: ~w~n', [RecursosStr])
        )
    ;
        format('   📦 Sin recursos asignados~n', [])
    ),
    N1 is N + 1,
    mostrar_eventos_con_recursos(Resto, N1).

% ========== VER INVENTARIO ==========
ver_inventario :-
    nl,
    write('================================'), nl,
    write('           INVENTARIO           '), nl,
    write('================================'), nl, nl,
    findall([R, C], data:recurso_inventario(R, C), Lista),
    (Lista == [] ->
        write('📭 Inventario vacío.'), nl
    ;
        mostrar_inventario(Lista, 1),
        findall(C, data:recurso_inventario(_, C), Cantidades),
        sum_list(Cantidades, TotalUnidades),
        length(Lista, NumRecursos),
        format('~nTotal: ~d recursos (~d unidades)~n', [NumRecursos, TotalUnidades])
    ),
    nl.

mostrar_inventario([], _).
mostrar_inventario([[R, C]|Resto], N) :-
    format('~d. ~w (~d)~n', [N, R, C]),
    N1 is N + 1,
    mostrar_inventario(Resto, N1).

% ========== VER EVENTOS POR FECHA ==========
ver_eventos_fecha :-
    nl,
    write('================================'), nl,
    write('        EVENTOS POR FECHA       '), nl,
    write('================================'), nl, nl,
    
    write('Fecha a consultar (YYYY-MM-DD): '),
    flush_output,
    leer_linea(Fecha),
    
    (validar_fecha(Fecha) ->
        findall([Nombre, Recursos], 
                (data:mi_evento(Nombre, Fecha, _), 
                 (data:mis_recursos(Nombre, Recursos) -> true ; Recursos = [])), 
                Lista),
        (Lista == [] ->
            format('📭 No hay eventos para la fecha ~w~n', [Fecha])
        ;
            format('📅 Eventos del ~w:~n~n', [Fecha]),
            mostrar_eventos_fecha(Lista, 1),
            length(Lista, Cantidad),
            format('~nTotal: ~d eventos~n', [Cantidad])
        )
    ;
        write('❌ Formato de fecha inválido. Use YYYY-MM-DD'), nl
    ),
    nl.

mostrar_eventos_fecha([], _).
mostrar_eventos_fecha([[Nombre, Recursos]|Resto], N) :-
    format('~d. ~w~n', [N, Nombre]),
    (Recursos == [] -> 
        format('   📦 Sin recursos~n', [])
    ;
        maplist(recurso_a_string, Recursos, RecursosStrList),
        atomic_list_concat(RecursosStrList, ', ', RecursosStr),
        format('   📦 Recursos: ~w~n', [RecursosStr])
    ),
    N1 is N + 1,
    mostrar_eventos_fecha(Resto, N1).

% ========== AGREGAR EVENTO CON RECURSOS ==========
agregar_evento_con_recursos :-
    nl,
    write('================================'), nl,
    write('   NUEVO EVENTO CON RECURSOS    '), nl,
    write('================================'), nl, nl,

    write('Nombre del evento: '),
    flush_output,
    leer_linea(Nombre),
    (Nombre = "" ->
        write('❌ El nombre no puede estar vacío.'), nl,
        agregar_evento_con_recursos
    ;
        write('Fecha (formato: YYYY-MM-DD) o presione Enter para la proxima fecha disponible: '),
        flush_output,
        leer_linea(Fecha),
        (Fecha = "" ->
            write('Duracion del evento: '),
            flush_output,
            leer_linea(Duracion),
            write('Recursos (formato: recurso cantidad, recurso cantidad, o presiona Enter para ninguno): '),
            flush_output,
            leer_linea(RecursosInput),
            (RecursosInput = "" ->
                Recursos = []
            ;
                split_string(RecursosInput, ",", "", RecursosLista),
                limpiar_lista_recursos(RecursosLista, Recursos)
            ),
            solicitar_recursos_sin_fecha(Nombre,Recursos,Duracion)
        ;
            (validar_fecha(Fecha) ->
                write('Duracion del evento: '),
                flush_output,
                leer_linea(Duracion),
                write('Recursos (formato: recurso cantidad, recurso cantidad, o presiona Enter para ninguno): '),
                flush_output,
                leer_linea(RecursosInput), 
                (RecursosInput = "" ->
                    Recursos = []
                ;
                    split_string(RecursosInput, ",", "", RecursosLista),
                    limpiar_lista_recursos(RecursosLista, Recursos)
                ),
                solicitar_recursos_en_fecha(Nombre,Recursos,Fecha,Duracion)
            ;
                write('❌ Formato de fecha inválido. Use YYYY-MM-DD'), nl,
                agregar_evento_con_recursos
            )
        )
    ).