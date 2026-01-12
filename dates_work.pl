:- module(dates_work, [validar_fecha/1,fecha_actual/1,fecha_siguiente/2,generar_fechas_consecutivas/3]).

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

fecha_actual(Fecha) :-
    get_time(Tiempo),
    format_time(atom(Fecha), '%Y-%m-%d', Tiempo).

fecha_siguiente(FechaInput, FechaSiguienteStr) :-
    (atom(FechaInput) -> atom_string(FechaInput, FechaStr) ; FechaStr = FechaInput),
    string(FechaStr),
    split_string(FechaStr, "-", "", [AnioStr, MesStr, DiaStr]),
    catch(
        (atom_number(AnioStr, Anio),
         atom_number(MesStr, Mes),
         atom_number(DiaStr, Dia)),
        _,
        fail
    ),
    integer(Anio), integer(Mes), integer(Dia),
    fecha_siguiente_calculo(Anio, Mes, Dia, AnioSig, MesSig, DiaSig),
    format(atom(AnioAtom), '~`0t~d~4+', [AnioSig]),
    format(atom(MesAtom), '~`0t~d~2+', [MesSig]),
    format(atom(DiaAtom), '~`0t~d~2+', [DiaSig]),
    atomic_list_concat([AnioAtom, MesAtom, DiaAtom], '-', FechaSiguienteAtom),
    atom_string(FechaSiguienteAtom, FechaSiguienteStr).
    
fecha_siguiente_calculo(Anio, Mes, Dia, Anio, Mes, DiaSig) :-
    dias_en_mes(Mes, Anio, DiasMes),
    Dia < DiasMes,
    DiaSig is Dia + 1.

fecha_siguiente_calculo(Anio, Mes, Dia, Anio, MesSig, 1) :-
    dias_en_mes(Mes, Anio, DiasMes),
    Dia =:= DiasMes,
    Mes < 12,
    MesSig is Mes + 1.

fecha_siguiente_calculo(Anio, 12, 31, AnioSig, 1, 1) :-
    AnioSig is Anio + 1.

fecha_siguiente_calculo(Anio, 2, 28, Anio, 3, 1) :-
    \+ es_bisiesto(Anio).

fecha_siguiente_calculo(Anio, 2, 28, Anio, 2, 29) :-
    es_bisiesto(Anio).

fecha_siguiente_calculo(Anio, 2, 29, Anio, 3, 1).

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

generar_fechas_consecutivas(Fecha, Duracion, Fechas) :-
    generar_fechas_consecutivas(Fecha, Duracion, [], FechasReversa),
    reverse(FechasReversa, Fechas).

generar_fechas_consecutivas(_, 0, Acum, Acum).
generar_fechas_consecutivas(Fecha, Duracion, Acum, Fechas) :-
    Duracion > 0,
    Acum1 = [Fecha|Acum],
    fecha_siguiente(Fecha, FechaSig),
    Duracion1 is Duracion - 1,
    generar_fechas_consecutivas(FechaSig, Duracion1, Acum1, Fechas).