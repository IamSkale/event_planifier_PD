:- module(checking, [solicitar_recursos_en_fecha/4, solicitar_recursos_sin_fecha/3]).

:- use_module(charging, [limpiar_lista_recursos/2, cargar_todo/0]).
:- use_module(saving, [guardar_todo/0, recurso_a_string/2]).
:- use_module(dates_work, [fecha_actual/1,fecha_siguiente/2,generar_fechas_consecutivas/3]).

% ========== SOLICITAR RECURSOS ==========

solicitar_recursos_en_fecha(Nombre,Recursos,Fecha,Duracion):-
    verificar_en_inventario(Recursos,NoInv),
    (NoInv \= [] ->
        format('❌ Recursos no presentes en inventario: ~w~nNo se agregó el evento.~n', [NoInv])
    ;
        verificar_disponibilidad(Fecha,Duracion,Recursos,NoDisponiblesAll),
        (NoDisponiblesAll \= [] ->
            format('❌ Recursos no disponibles para ~w: ~w~nNo se agregó el evento.~n', [Fecha, NoDisponiblesAll])
        ;
            (data:mi_evento(Nombre, Fecha, Duracion) ->
                format('❗ En la fecha ~w ya existe el evento "~w".~n', [Fecha, Nombre])
            ;
                assertz(data:mi_evento(Nombre, Fecha, Duracion))
            ),
            (Recursos == [] ->
                true
            ;
                retractall(data:mis_recursos(Nombre, _)),
                assertz(data:mis_recursos(Nombre, Recursos))
            ),
            guardar_todo,
            cargar_todo,
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

solicitar_recursos_sin_fecha(Nombre,Recursos,Duracion):-
    verificar_en_inventario(Recursos,NoInv),
    (NoInv \= [] ->
        format('❌ Recursos no presentes en inventario: ~w~nNo se agregó el evento.~n', [NoInv])
    ;
        fecha_actual(Fecha),
        verificar_disponibilidad_rec(Fecha,Recursos,Duracion,FechaAceptada),
        (data:mi_evento(Nombre, FechaAceptada, Duracion) ->
            format('❗ En la fecha ~w ya existe el evento "~w".~n', [FechaAceptada, Nombre])
        ;
            assertz(data:mi_evento(Nombre, FechaAceptada, Duracion))
        ),
        (Recursos == [] ->
            true
        ;
            retractall(data:mis_recursos(Nombre, _)),
            assertz(data:mis_recursos(Nombre, Recursos))
        ),
        guardar_todo,
        cargar_todo,
        format('✅ Evento "~w" agregado para la fecha ~w~n', [Nombre, FechaAceptada]),
        (Recursos == [] ->
            format('   📦 Sin recursos asignados~n', [])
        ;
            maplist(recurso_a_string, Recursos, RecursosStrList),
            atomic_list_concat(RecursosStrList, ', ', RecursosStr),
            format('   📦 Recursos: ~w~n', [RecursosStr])
        )
    ).

% ========== VERIFICAR DISPONIBILIDAD ==========

verificar_disponibilidad(FechaInicio, Duracion, RecursosSolicitados, NoDisponiblesAll) :-
    generar_fechas_consecutivas(FechaInicio, Duracion, Fechas),
    
    verificar_disponibilidad_rango(Fechas, RecursosSolicitados, [], NoDisponiblesAll).

verificar_disponibilidad_rango([], _, Acum, Acum).
verificar_disponibilidad_rango([Fecha|RestoFechas], RecursosSolicitados, Acum, Problemas) :-
    verificar_disponibilidad_fecha(Fecha, RecursosSolicitados, ProblemasFecha),
    
    (ProblemasFecha = [] ->
        verificar_disponibilidad_rango(RestoFechas, RecursosSolicitados, Acum, Problemas)
    ;
        agregar_problemas_fecha(Fecha, ProblemasFecha, Acum, NuevoAcum),
        verificar_disponibilidad_rango(RestoFechas, RecursosSolicitados, NuevoAcum, Problemas)
    ).

verificar_disponibilidad_fecha(Fecha, RecursosSolicitados, NoDisponiblesAll) :-
    findall(RecursosEvento, 
            data:fecha_evento(Fecha, _, RecursosEvento, _), 
            ListaRecursosOcupados),
    
    aplanar_recursos(ListaRecursosOcupados, RecursosOcupadosFlat),
    
    sumar_recursos_ocupados(RecursosOcupadosFlat, RecursosOcupadosTotales),
    
    verificar_recursos_individuales(RecursosSolicitados, RecursosOcupadosTotales, NoDisponiblesAll).

agregar_problemas_fecha(Fecha, ProblemasFecha, Acum, [[Fecha, ProblemasFecha]|Acum]).

verificar_disponibilidad(Fecha, RecursosSolicitados, NoDisponiblesAll) :-
    verificar_disponibilidad_fecha(Fecha, RecursosSolicitados, NoDisponiblesAll).

aplanar_recursos([], []).
aplanar_recursos([RecursosList|Resto], Flat) :-
    aplanar_recursos(Resto, RestoFlat),
    append(RecursosList, RestoFlat, Flat).

sumar_recursos_ocupados(RecursosOcupados, RecursosTotales) :-
    sumar_recursos_ocupados(RecursosOcupados, [], RecursosTotales).

sumar_recursos_ocupados([], Acum, Acum).
sumar_recursos_ocupados([[Nombre, Cant]|Resto], Acum, Totales) :-
    (select([Nombre, CantExistente], Acum, RestoAcum) ->
        NuevaCant is CantExistente + Cant,
        sumar_recursos_ocupados(Resto, [[Nombre, NuevaCant]|RestoAcum], Totales)
    ;
        sumar_recursos_ocupados(Resto, [[Nombre, Cant]|Acum], Totales)
    ).

verificar_recursos_individuales([], _, []).
verificar_recursos_individuales([[Nombre, CantSolicitada]|Resto], RecursosOcupados, NoDisponibles) :-
    (data:recurso_inventario(Nombre, CantInventario) ->
        (member([Nombre, CantOcupada], RecursosOcupados) ->
            CantTotalOcupada = CantOcupada
        ;
            CantTotalOcupada = 0
        ),
        
        CantDisponible is CantInventario - CantTotalOcupada,
        
        (CantSolicitada =< CantDisponible ->
            verificar_recursos_individuales(Resto, RecursosOcupados, NoDisponiblesResto),
            NoDisponibles = NoDisponiblesResto
        ;
            Faltante is CantSolicitada - CantDisponible,
            verificar_recursos_individuales(Resto, RecursosOcupados, NoDisponiblesResto),
            NoDisponibles = [[Nombre, CantSolicitada, CantDisponible, Faltante]|NoDisponiblesResto]
        )
    ;
        verificar_recursos_individuales(Resto, RecursosOcupados, NoDisponiblesResto),
        NoDisponibles = [[Nombre, CantSolicitada, 0, CantSolicitada]|NoDisponiblesResto]
    ).

verificar_disponibilidad_rec(Fecha, Recursos, Duracion, FechaAceptada) :-
    verificar_disponibilidad(Fecha, Duracion, Recursos, NoDisponiblesAll),
    (NoDisponiblesAll \= [] ->
        fecha_siguiente(Fecha,FechaSig),
        verificar_disponibilidad_rec(FechaSig,Duracion,Recursos,FechaAceptada)
    ;
        FechaAceptada = Fecha
    ).

verificar_en_inventario(Recursos,NoInv):-
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
    list_to_set(NoInvAll, NoInv).
