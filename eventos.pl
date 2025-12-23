:- module(eventos, [iniciar/0, main/0]).

% Predicados dinámicos para mantener eventos en memoria
:- dynamic mi_evento/2.

% Archivo de persistencia en texto plano
archivo_txt('eventos.txt').

% ========== INICIALIZAR SISTEMA ==========
iniciar :-
    mostrar_bienvenida,
    cargar_eventos,
    bucle_principal.

mostrar_bienvenida :-
    write('================================'), nl,
    write('   SISTEMA DE GESTIÓN DE EVENTOS'), nl,
    write('================================'), nl, nl,
    format('ℹ️  Para salir en cualquier momento, presiona Ctrl+D (Linux/Mac) o Ctrl+Z (Windows)~n~n').

% ========== BUCLE PRINCIPAL ==========
bucle_principal :-
    mostrar_menu,
    leer_opcion(Opcion),
    procesar_opcion(Opcion),
    (Opcion \= 3 -> bucle_principal ; true).

% ========== MENÚ ==========
mostrar_menu :-
    write('OPCIONES DISPONIBLES:'), nl,
    write('  1. Ver lista de eventos'), nl,
    write('  2. Agregar nuevo evento'), nl,
    write('  3. Salir y guardar'), nl, nl.

leer_opcion(Opcion) :-
    write('Seleccione una opción (1-3): '),
    flush_output,
    leer_linea(Input),
    (atom_number(Input, Num), integer(Num), Num >= 1, Num =< 3 ->
        Opcion = Num
    ;
        write('❌ Opción inválida. Intente de nuevo.'), nl, nl,
        leer_opcion(Opcion)
    ).

procesar_opcion(1) :-
    ver_eventos.
    
procesar_opcion(2) :-
    agregar_evento.
    
procesar_opcion(3) :-
    guardar_eventos,
    write('👋 ¡Hasta luego!'), nl.

% ========== CARGA DE EVENTOS DESDE ARCHIVO TEXTO ==========
cargar_eventos :-
    archivo_txt(Archivo),
    (exists_file(Archivo) ->
        setup_call_cleanup(
            open(Archivo, read, Stream),
            leer_eventos_desde_stream(Stream),
            close(Stream)
        ),
        format('✅ Eventos cargados desde ~w~n', [Archivo])
    ;
        write('ℹ️  No se encontró archivo de eventos. Se iniciará vacío.~n'),
        limpiar_eventos
    ).

% Leer eventos línea por línea
leer_eventos_desde_stream(Stream) :-
    limpiar_eventos,
    repeat,
    leer_linea_archivo(Stream, Linea),
    (Linea == end_of_file -> ! ;
     Linea \= "" ->
        procesar_linea_evento(Linea),
        fail
    ).

% Leer una línea del archivo
leer_linea_archivo(Stream, Linea) :-
    (at_end_of_stream(Stream) -> 
        Linea = end_of_file
    ;
        read_line_to_string(Stream, Linea)
    ).

% Procesar línea con formato: Nombre|Fecha
procesar_linea_evento(Linea) :-
    split_string(Linea, "|", "", Partes),
    Partes = [Nombre, Fecha],
    assertz(mi_evento(Nombre, Fecha)).

% Versión alternativa más robusta de split
leer_linea_archivo_robusto(Stream, Linea) :-
    (at_end_of_stream(Stream) -> 
        Linea = end_of_file
    ;
        read_line_to_codes(Stream, Codes),
        (Codes == end_of_file -> 
            Linea = end_of_file
        ;
            atom_codes(Atom, Codes),
            atom_string(Atom, Linea)
        )
    ).

limpiar_eventos :-
    retractall(mi_evento(_, _)).

% ========== GUARDAR EVENTOS EN ARCHIVO TEXTO ==========
guardar_eventos :-
    archivo_txt(Archivo),
    setup_call_cleanup(
        open(Archivo, write, Stream),
        escribir_eventos_a_stream(Stream),
        close(Stream)
    ),
    format('💾 Eventos guardados en ~w~n', [Archivo]).

% Escribir todos los eventos al archivo
escribir_eventos_a_stream(Stream) :-
    forall(
        mi_evento(Nombre, Fecha),
        format(Stream, '~w|~w~n', [Nombre, Fecha])
    ).

% ========== VER EVENTOS ==========
ver_eventos :-
    nl,
    write('================================'), nl,
    write('          LISTA DE EVENTOS       '), nl,
    write('================================'), nl, nl,
    findall([Nombre, Fecha], mi_evento(Nombre, Fecha), Lista),
    (Lista == [] ->
        write('📭 No hay eventos registrados.'), nl
    ;
        mostrar_eventos(Lista, 1),
        nl,
        length(Lista, Cantidad),
        format('Total: ~d eventos~n', [Cantidad])
    ),
    nl.

mostrar_eventos([], _).
mostrar_eventos([[Nombre, Fecha]|Resto], N) :-
    format('~d. ~w~n', [N, Nombre]),
    format('   📅 Fecha: ~w~n', [Fecha]),
    N1 is N + 1,
    mostrar_eventos(Resto, N1).

% ========== AGREGAR EVENTO ==========
agregar_evento :-
    nl,
    write('================================'), nl,
    write('       NUEVO EVENTO'), nl,
    write('================================'), nl, nl,
    
    write('Nombre del evento: '),
    flush_output,
    leer_linea(Nombre),
    (Nombre = "" ->
        write('❌ El nombre no puede estar vacío.'), nl,
        agregar_evento
    ;
        write('Fecha (formato: YYYY-MM-DD): '),
        flush_output,
        leer_linea(Fecha),
        (validar_fecha(Fecha) ->
            assertz(mi_evento(Nombre, Fecha)),
            format('✅ Evento "~w" agregado para la fecha ~w~n', [Nombre, Fecha])
        ;
            write('❌ Formato de fecha inválido. Use YYYY-MM-DD'), nl,
            agregar_evento
        )
    ).

validar_fecha(Fecha) :-
    split_string(Fecha, "-", "", Partes),
    length(Partes, 3),
    catch(
        maplist(atom_number, Partes, [Anio, Mes, Dia]),
        _,
        fail
    ),
    integer(Anio), Anio >= 1, Anio =< 9999,
    integer(Mes), Mes >= 1, Mes =< 12,
    integer(Dia), Dia >= 1, Dia =< 31.

% ========== UTILIDADES ==========
leer_linea(String) :-
    read_line_to_codes(user_input, Codes),
    (Codes == end_of_file -> 
        String = "",
        write('~n'),
        fail
    ;
        atom_codes(Atom, Codes),
        atom_string(Atom, String)
    ).

% Para leer strings directamente (compatibilidad)
read_line_to_string(Stream, String) :-
    read_line_to_codes(Stream, Codes),
    (Codes == end_of_file -> 
        String = ""
    ;
        atom_codes(Atom, Codes),
        atom_string(Atom, String)
    ).