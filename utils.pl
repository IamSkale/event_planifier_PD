:- module(utils, [string_trim/2, leer_linea/1, leer_linea_archivo/2, read_line_to_string/2,
                 validar_fecha/1, sort_eventos/2, solicitar_recursos_en_fecha/3, ocupado_cant/3,
                 fecha_actual/1,fecha_siguiente/2, solicitar_recursos_sin_fecha/2]).
:- use_module(charging, [limpiar_lista_recursos/2]).
:- use_module(saving, [guardar_todo/0, recurso_a_string/2]).

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

sort_eventos(Eventos, Ordenados) :-
    predsort(comparar_eventos, Eventos, Ordenados).

comparar_eventos(Orden, [Nombre1, Fecha1], [Nombre2, Fecha2]) :-
    (Fecha1 @< Fecha2 -> Orden = <
    ; Fecha1 @> Fecha2 -> Orden = >
    ; Nombre1 @< Nombre2 -> Orden = <
    ; Nombre1 @> Nombre2 -> Orden = >
    ; Orden = =
    ).

recursos_ocupados_en_fecha(Fecha, Ocupados) :-
    findall([Res, Cant], (data:mi_evento(N, Fecha), data:mis_recursos(N, Lista), member([Res, Cant], Lista)), All),
    agrupar_sumar(All, Ocupados).

verificar_disponibilidad(Fecha,Recursos,NoDisponiblesAll):-
    recursos_ocupados_en_fecha(Fecha, Ocupados),
    findall([R, C], (member([R, C], Recursos), 
    (data:recurso_inventario(R, InvCant) ->
        (ocupado_cant(R, Ocupados, OcupCant),
        C + OcupCant > InvCant)
    ; 
        false
    )), NoDisponiblesAll).                                                              

agrupar_sumar([], []).
agrupar_sumar([[Res, Cant]|Resto], Ocupados) :-
    agrupar_sumar(Resto, RestoAgrupado),
    (select([Res, CantExistente], RestoAgrupado, SinRes) ->
        NuevaCant is CantExistente + Cant,
        Ocupados = [[Res, NuevaCant]|SinRes]
    ;
        Ocupados = [[Res, Cant]|RestoAgrupado]
    ).

ocupado_cant(R, Ocupados, Cant) :-
    (member([R, Cant], Ocupados) -> true ; Cant = 0).

fecha_actual(Fecha) :-
    get_time(Tiempo),
    format_time(atom(Fecha), '%Y-%m-%d', Tiempo).

% ========== FECHA DEL DÍA SIGUIENTE ==========

% fecha_siguiente(FechaActual, FechaSiguiente)
fecha_siguiente(FechaInput, FechaSiguienteStr) :-
    % Asegurarnos de que es string
    (atom(FechaInput) -> atom_string(FechaInput, FechaStr) ; FechaStr = FechaInput),
    string(FechaStr),
    % Validar formato y obtener partes
    split_string(FechaStr, "-", "", [AnioStr, MesStr, DiaStr]),
    % Convertir a números con manejo de errores
    catch(
        (atom_number(AnioStr, Anio),
         atom_number(MesStr, Mes),
         atom_number(DiaStr, Dia)),
        _,
        fail
    ),
    % Validar que son enteros
    integer(Anio), integer(Mes), integer(Dia),
    % Calcular fecha siguiente
    fecha_siguiente_calculo(Anio, Mes, Dia, AnioSig, MesSig, DiaSig),
    % Formatear resultado
    % Año siempre con 4 dígitos
    format(atom(AnioAtom), '~`0t~d~4+', [AnioSig]),
    % Mes con 2 dígitos
    format(atom(MesAtom), '~`0t~d~2+', [MesSig]),
    % Día con 2 dígitos
    format(atom(DiaAtom), '~`0t~d~2+', [DiaSig]),
    % Unir todo
    atomic_list_concat([AnioAtom, MesAtom, DiaAtom], '-', FechaSiguienteAtom),
    atom_string(FechaSiguienteAtom, FechaSiguienteStr).

% Caso 1: Día normal dentro del mes
fecha_siguiente_calculo(Anio, Mes, Dia, Anio, Mes, DiaSig) :-
    dias_en_mes(Mes, Anio, DiasMes),
    Dia < DiasMes,
    DiaSig is Dia + 1.

% Caso 2: Fin de mes (no diciembre)
fecha_siguiente_calculo(Anio, Mes, Dia, Anio, MesSig, 1) :-
    dias_en_mes(Mes, Anio, DiasMes),
    Dia =:= DiasMes,
    Mes < 12,
    MesSig is Mes + 1.

% Caso 3: Fin de año (31 de diciembre)
fecha_siguiente_calculo(Anio, 12, 31, AnioSig, 1, 1) :-
    AnioSig is Anio + 1.

% Caso 4: 28 de febrero (no bisiesto)
fecha_siguiente_calculo(Anio, 2, 28, Anio, 3, 1) :-
    \+ es_bisiesto(Anio).

% Caso 5: 28 de febrero (bisiesto) -> 29 de febrero
fecha_siguiente_calculo(Anio, 2, 28, Anio, 2, 29) :-
    es_bisiesto(Anio).

% Caso 6: 29 de febrero -> 1 de marzo
fecha_siguiente_calculo(Anio, 2, 29, Anio, 3, 1).

% Días en cada mes
dias_en_mes(1, _, 31).   % Enero
dias_en_mes(2, Anio, Dias) :-  % Febrero
    (es_bisiesto(Anio) -> Dias = 29 ; Dias = 28).
dias_en_mes(3, _, 31).   % Marzo
dias_en_mes(4, _, 30).   % Abril
dias_en_mes(5, _, 31).   % Mayo
dias_en_mes(6, _, 30).   % Junio
dias_en_mes(7, _, 31).   % Julio
dias_en_mes(8, _, 31).   % Agosto
dias_en_mes(9, _, 30).   % Septiembre
dias_en_mes(10, _, 31).  % Octubre
dias_en_mes(11, _, 30).  % Noviembre
dias_en_mes(12, _, 31).  % Diciembre

solicitar_recursos_en_fecha(Nombre,RecursosInput,Fecha):-
    (RecursosInput = "" ->
        Recursos = []
    ;
        split_string(RecursosInput, ",", "", RecursosLista),
        limpiar_lista_recursos(RecursosLista, Recursos)
    ),
    % Verificar que los recursos solicitados estén en el inventario
    findall([R, C], 
    (member([R, C], Recursos), 
     (\+ data:recurso_inventario(R, _) -> 
        true  
     ; 
        data:recurso_inventario(R, InvCant), 
        C > InvCant  
     )
    ), 
    NoInvAll),
    list_to_set(NoInvAll, NoInv),
    (NoInv \= [] ->
        format('❌ Recursos no presentes en inventario: ~w~nNo se agregó el evento.~n', [NoInv])
    ;
        % Verificar disponibilidad de recursos para la fecha
        verificar_disponibilidad(Fecha,Recursos,NoDisponiblesAll),
        (NoDisponiblesAll \= [] ->
            format('❌ Recursos no disponibles para ~w: ~w~nNo se agregó el evento.~n', [Fecha, NoDisponiblesAll])
        ;
            % Agregar evento (evitar duplicados)
            (data:mi_evento(Nombre, Fecha) ->
                format('❗ En la fecha ~w ya existe el evento "~w".~n', [Fecha, Nombre])
            ;
                assertz(data:mi_evento(Nombre, Fecha))
            ),
            % Agregar/actualizar recursos (reemplaza si ya existen)
            (Recursos == [] ->
                true
            ;
                retractall(data:mis_recursos(Nombre, _)),
                assertz(data:mis_recursos(Nombre, Recursos))
            ),
            % Guardar todo
            guardar_todo,
            format('✅ Evento "~w" agregado para la fecha ~w~n', [Nombre, Fecha]),
            (Recursos == [] ->
                format('   📦 Sin recursos asignados~n', [])
            ;
                maplist(recurso_a_string, Recursos, RecursosStrList),
                atomic_list_concat(RecursosStrList, ', ', RecursosStr),
                format('   📦 Recursos: ~w~n', [RecursosStr])
            )
        )
    ).

solicitar_recursos_sin_fecha(Nombre,RecursosInput):-
    (RecursosInput = "" ->
        Recursos = []
    ;
        split_string(RecursosInput, ",", "", RecursosLista),
        limpiar_lista_recursos(RecursosLista, Recursos)
    ),
    % Verificar que los recursos solicitados estén en el inventario
    findall([R, C], 
    (member([R, C], Recursos), 
     (\+ data:recurso_inventario(R, _) -> 
        true  
     ; 
        data:recurso_inventario(R, InvCant), 
        C > InvCant  
     )
    ), 
    NoInvAll),
    list_to_set(NoInvAll, NoInv),
    (NoInv \= [] ->
        format('❌ Recursos no presentes en inventario: ~w~nNo se agregó el evento.~n', [NoInv])
    ;
        % Verificar disponibilidad de recursos
        fecha_actual(Fecha),
        verificar_disponibilidad_aux(Fecha,Recursos,FechaAceptada),
        % Agregar evento (evitar duplicados)
        (data:mi_evento(Nombre, FechaAceptada) ->
            format('❗ En la fecha ~w ya existe el evento "~w".~n', [FechaAceptada, Nombre])
        ;
            assertz(data:mi_evento(Nombre, FechaAceptada))
        ),
        % Agregar/actualizar recursos (reemplaza si ya existen)
        (Recursos == [] ->
            true
        ;
            retractall(data:mis_recursos(Nombre, _)),
            assertz(data:mis_recursos(Nombre, Recursos))
        ),
        % Guardar todo
        guardar_todo,
        format('✅ Evento "~w" agregado para la fecha ~w~n', [Nombre, FechaAceptada]),
        (Recursos == [] ->
            format('   📦 Sin recursos asignados~n', [])
        ;
            maplist(recurso_a_string, Recursos, RecursosStrList),
            atomic_list_concat(RecursosStrList, ', ', RecursosStr),
            format('   📦 Recursos: ~w~n', [RecursosStr])
        )
    ).

verificar_disponibilidad_aux(Fecha,Recursos,FechaAceptada):-
    verificar_disponibilidad(Fecha,Recursos,NoDisponiblesAll),
    (NoDisponiblesAll \= [] ->
        fecha_siguiente(Fecha,FechaSig),
        verificar_disponibilidad_aux(FechaSig,Recursos,FechaAceptada)
    ;
        FechaAceptada = Fecha
    ).
