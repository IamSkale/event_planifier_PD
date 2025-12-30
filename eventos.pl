:- module(eventos, [iniciar/0, main/0]).

% PREDICADOS DINÁMICOS
:- dynamic mi_evento/2.
:- dynamic mis_recursos/2.
:- dynamic recurso_inventario/1.

% Archivos de persistencia
archivo_eventos('eventos.txt').
archivo_recursos('recursos.txt').
archivo_inventario('inventario.txt').

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

% ========== GUARDAR DATOS ==========
guardar_todo :-
    guardar_eventos,
    guardar_recursos.

guardar_eventos :-
    archivo_eventos(Archivo),
    setup_call_cleanup(
        open(Archivo, write, Stream),
        escribir_eventos_a_stream(Stream),
        close(Stream)
    ).

guardar_recursos :-
    archivo_recursos(Archivo),
    setup_call_cleanup(
        open(Archivo, write, Stream),
        escribir_recursos_a_stream(Stream),
        close(Stream)
    ).

% Escribir eventos al archivo
escribir_eventos_a_stream(Stream) :-
    findall([Nombre, Fecha], mi_evento(Nombre, Fecha), EventosAll),
    % Eliminar duplicados si existen (preserva el primer orden de aparición)
    list_to_set(EventosAll, Eventos),
    forall(
        member([Nombre, Fecha], Eventos),
        format(Stream, '~w|~w~n', [Nombre, Fecha])
    ).

% Escribir recursos al archivo manteniendo el orden de los eventos
escribir_recursos_a_stream(Stream) :-
    % Obtener todos los eventos en el orden actual (sin duplicados)
    findall(Nombre, mi_evento(Nombre, _), EventosAll),
    list_to_set(EventosAll, EventosOrden),
    escribir_recursos_por_orden(Stream, EventosOrden).

escribir_recursos_por_orden(_, []).
escribir_recursos_por_orden(Stream, [Nombre|Resto]) :-
    (mis_recursos(Nombre, Recursos), Recursos \= [] ->
        % Unir recursos con comas (sin espacios)
        atomic_list_concat(Recursos, ',', RecursosStr),
        format(Stream, '~w~n', [RecursosStr])
    ;
        % Si no hay recursos, escribir línea vacía
        format(Stream, '~n', [])
    ),
    escribir_recursos_por_orden(Stream, Resto).

% ========== CARGA DE DATOS ==========
cargar_todo :-
    cargar_eventos,
    cargar_recursos,
    cargar_inventario.

cargar_eventos :-
    archivo_eventos(Archivo),
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

% Método para leer eventos
leer_eventos_desde_stream(Stream) :-
    leer_linea_archivo(Stream, Linea),
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
    archivo_recursos(Archivo),
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

% Método para leer recursos (ahora mantiene el orden de los eventos)
leer_recursos_desde_stream(Stream) :-
    findall(Nombre, mi_evento(Nombre, _), EventosOrden),
    leer_recursos_por_evento(Stream, EventosOrden, 1).

leer_recursos_por_evento(_, [], _).
leer_recursos_por_evento(Stream, [Nombre|RestoEventos], NumLinea) :-
    leer_linea_archivo(Stream, Linea),
    (Linea == end_of_file -> 
        format('   ⚠️  Fin de archivo alcanzado en línea ~d~n', [NumLinea])
    ;
        % Procesar la línea (puede estar vacía o contener recursos separados por comas)
        (Linea = "" ->
            % Línea vacía = sin recursos
            Recursos = []
        ;
            % Eliminar espacios al principio y final usando string_trim/2
            string_trim(Linea, LineaTrim),
            (LineaTrim = "" ->
                % Solo espacios = sin recursos
                Recursos = []
            ;
                % Dividir por comas y limpiar cada recurso
                split_string(LineaTrim, ",", "", RecursosRaw),
                limpiar_lista_recursos(RecursosRaw, Recursos)
            )
        ),
        % Solo almacenar si hay recursos
        (Recursos = [] -> 
            true
        ;
            assertz(mis_recursos(Nombre, Recursos))
        ),
        NumLinea1 is NumLinea + 1,
        leer_recursos_por_evento(Stream, RestoEventos, NumLinea1)
    ).

% ========== INVENTARIO ==========
cargar_inventario :-
    archivo_inventario(Archivo),
    (exists_file(Archivo) ->
        retractall(recurso_inventario(_)),
        setup_call_cleanup(
            open(Archivo, read, Stream),
            leer_inventario_desde_stream(Stream),
            close(Stream)
        )
    ;
        format('   ⚠️  No se encontró ~w, inventario vacío~n', [Archivo]),
        retractall(recurso_inventario(_))
    ).

leer_inventario_desde_stream(Stream) :-
    leer_linea_archivo(Stream, Linea),
    (Linea == end_of_file ->
        true
    ;
        (Linea \= "" ->
            string_trim(Linea, T),
            (T \= "" -> (recurso_inventario(T) -> true ; assertz(recurso_inventario(T))) ; true)
        ; true),
        leer_inventario_desde_stream(Stream)
    ).

contar_inventario(Total) :-
    findall(_, recurso_inventario(_), Lista),
    length(Lista, Total).

% Formato eventos: Nombre|Fecha
procesar_linea_evento(Linea) :-
    split_string(Linea, "|", "", Partes),
    (Partes = [Nombre, Fecha] ->
        % Evitar insertar eventos duplicados al leer desde archivo
        (mi_evento(Nombre, Fecha) ->
            true
        ;
            assertz(mi_evento(Nombre, Fecha))
        )
    ;
        format('   ⚠️  Línea mal formada: ~w~n', [Linea])
    ).

limpiar_eventos :-
    retractall(mi_evento(_, _)).

limpiar_recursos :-
    retractall(mis_recursos(_, _)).

% Contar eventos cargados
contar_eventos(Total) :-
    findall(_, mi_evento(_, _), Lista),
    length(Lista, Total).

% Contar recursos cargados
contar_recursos(Total) :-
    findall(_, mis_recursos(_, _), Lista),
    length(Lista, Total).

% ========== VER EVENTOS CON RECURSOS ==========
ver_eventos_con_recursos :-
    nl,
    write('================================'), nl,
    write('        LISTA DE EVENTOS        '), nl,
    write('================================'), nl, nl,
    
    findall([Nombre, Fecha], mi_evento(Nombre, Fecha), ListaEventos),
    
    (ListaEventos == [] ->
        write('📭 No hay eventos registrados.'), nl
    ;
        sort_eventos(ListaEventos, EventosOrdenados),
        mostrar_eventos_con_recursos(EventosOrdenados, 1),
        nl,
        length(ListaEventos, Cantidad),
        format('Total: ~d eventos~n', [Cantidad])
    ),
    nl.

% Ordenar eventos por fecha y luego por nombre
sort_eventos(Eventos, Ordenados) :-
    predsort(comparar_eventos, Eventos, Ordenados).

comparar_eventos(Orden, [Nombre1, Fecha1], [Nombre2, Fecha2]) :-
    (Fecha1 @< Fecha2 -> Orden = <
    ; Fecha1 @> Fecha2 -> Orden = >
    ; Nombre1 @< Nombre2 -> Orden = <
    ; Nombre1 @> Nombre2 -> Orden = >
    ; Orden = =
    ).

mostrar_eventos_con_recursos([], _).
mostrar_eventos_con_recursos([[Nombre, Fecha]|Resto], N) :-
    format('~d. ~w~n', [N, Nombre]),
    format('   📅 Fecha: ~w~n', [Fecha]),
    
    (mis_recursos(Nombre, Recursos) ->
        (Recursos == [] -> 
            format('   📦 Sin recursos~n', [])
        ;
            atomic_list_concat(Recursos, ', ', RecursosStr),
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
    findall(R, recurso_inventario(R), Lista),
    (Lista == [] ->
        write('📭 Inventario vacío.'), nl
    ;
        mostrar_inventario(Lista, 1),
        length(Lista, Cantidad),
        format('~nTotal: ~d recursos~n', [Cantidad])
    ),
    nl.

mostrar_inventario([], _).
mostrar_inventario([R|Resto], N) :-
    format('~d. ~w~n', [N, R]),
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
                (mi_evento(Nombre, Fecha), 
                 (mis_recursos(Nombre, Recursos) -> true ; Recursos = [])), 
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
        atomic_list_concat(Recursos, ', ', RecursosStr),
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
        write('Fecha (formato: YYYY-MM-DD): '),
        flush_output,
        leer_linea(Fecha),
        (validar_fecha(Fecha) ->
            % Solicitar recursos
            write('Recursos (separados por comas, o presiona Enter para ninguno): '),
            flush_output,
            leer_linea(RecursosInput),
            (RecursosInput = "" ->
                Recursos = []
            ;
                split_string(RecursosInput, ",", "", RecursosLista),
                limpiar_lista_recursos(RecursosLista, Recursos)
            ),
            % Verificar que los recursos solicitados estén en el inventario
            findall(R, (member(R, Recursos), \+ recurso_inventario(R)), NoInvAll),
            list_to_set(NoInvAll, NoInv),
            (NoInv \= [] ->
                format('❌ Recursos no presentes en inventario: ~w~nNo se agregó el evento.~n', [NoInv])
            ;
                % Verificar disponibilidad de recursos para la fecha
                recursos_ocupados_en_fecha(Fecha, Ocupados),
                findall(R, (member(R, Recursos), member(R, Ocupados)), NoDisponiblesAll),
                list_to_set(NoDisponiblesAll, NoDisponibles),
                (NoDisponibles \= [] ->
                    format('❌ Recursos no disponibles para ~w: ~w~nNo se agregó el evento.~n', [Fecha, NoDisponibles])
                ;
                    % Agregar evento (evitar duplicados)
                    (mi_evento(Nombre, Fecha) ->
                        format('❗ En la fecha ~w ya existe el evento "~w".~n', [Fecha, Nombre])
                    ;
                        assertz(mi_evento(Nombre, Fecha))
                    ),
                    % Agregar/actualizar recursos (reemplaza si ya existen)
                    (Recursos == [] ->
                        true
                    ;
                        retractall(mis_recursos(Nombre, _)),
                        assertz(mis_recursos(Nombre, Recursos))
                    ),
                    % Guardar todo
                    guardar_todo,
                    format('✅ Evento "~w" agregado para la fecha ~w~n', [Nombre, Fecha]),
                    (Recursos == [] ->
                        format('   📦 Sin recursos asignados~n', [])
                    ;
                        atomic_list_concat(Recursos, ', ', RecursosStr),
                        format('   📦 Recursos: ~w~n', [RecursosStr])
                    )
                )
            )
        ;
            write('❌ Formato de fecha inválido. Use YYYY-MM-DD'), nl,
            agregar_evento_con_recursos
        )
    ).

% Limpiar lista de recursos (quitar elementos vacíos)
limpiar_lista_recursos([], []).
limpiar_lista_recursos([R|Resto], Limpios) :-
    string_trim(R, Rlimpio),
    (Rlimpio = "" ->
        limpiar_lista_recursos(Resto, Limpios)
    ;
        limpiar_lista_recursos(Resto, RestoLimpios),
        Limpios = [Rlimpio|RestoLimpios]
    ).

% Recursos ocupados en una fecha específica
recursos_ocupados_en_fecha(Fecha, Ocupados) :-
    findall(Res, (mi_evento(N, Fecha), mis_recursos(N, Lista), member(Res, Lista)), All),
    list_to_set(All, Ocupados).

% ========== VALIDACIÓN DE FECHA ==========
validar_fecha(Fecha) :-
    string(Fecha),
    split_string(Fecha, "-", "", Partes),
    Partes = [AnioStr, MesStr, DiaStr],
    maplist(string_codes, [AnioStr, MesStr, DiaStr], [AnioCodes, MesCodes, DiaCodes]),
    catch(
        (number_codes(Anio, AnioCodes),
         number_codes(Mes, MesCodes),
         number_codes(Dia, DiaCodes)),
        _,
        fail
    ),
    integer(Anio), Anio >= 1,
    integer(Mes), Mes >= 1, Mes =< 12,
    integer(Dia), Dia >= 1, Dia =< 31,
    validar_dias_mes(Mes, Dia, Anio).

validar_dias_mes(2, Dia, Anio) :-
    (es_bisiesto(Anio) -> DiasMax = 29 ; DiasMax = 28),
    Dia =< DiasMax.
validar_dias_mes(Mes, Dia, _) :-
    member(Mes, [1,3,5,7,8,10,12]), Dia =< 31.
validar_dias_mes(Mes, Dia, _) :-
    member(Mes, [4,6,9,11]), Dia =< 30.

es_bisiesto(Anio) :-
    (Anio mod 400 =:= 0 -> true ;
     Anio mod 100 =:= 0 -> false ;
     Anio mod 4 =:= 0).

% ========== UTILIDADES ==========
% Eliminar espacios al principio y final de una cadena
string_trim(Str, Trimmed) :-
    string(Str),
    string_codes(Str, Codes),
    trim_whitespace_codes(Codes, TrimmedCodes),
    string_codes(Trimmed, TrimmedCodes).

trim_whitespace_codes(Codes, Trimmed) :-
    trim_leading(Codes, NoLead),
    reverse(NoLead, Rev),
    trim_leading(Rev, RevTrim),
    reverse(RevTrim, Trimmed).

trim_leading([C|Cs], Trimmed) :-
    whitespace_code(C), !,
    trim_leading(Cs, Trimmed).
trim_leading(List, List).

whitespace_code(9).   % tab
whitespace_code(10).  % newline
whitespace_code(13).  % carriage return
whitespace_code(32).  % space

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

% Leer línea de archivo
leer_linea_archivo(Stream, Linea) :-
    (at_end_of_stream(Stream) -> 
        Linea = end_of_file
    ;
        read_line_to_string(Stream, Linea)
    ).

read_line_to_string(Stream, String) :-
    read_line_to_codes(Stream, Codes),
    (Codes == end_of_file -> 
        String = ""
    ;
        atom_codes(Atom, Codes),
        atom_string(Atom, String)
    ).

% ========== PREDICADO PRINCIPAL ==========
main :-
    catch(
        iniciar,
        Error,
        (format('❌ Error crítico: ~w~n', [Error]),
         halt(1))
    ),
    halt(0).

% Para ejecutar: swipl -s eventos.pl -g main -t halt