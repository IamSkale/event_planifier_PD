:- module(utils, [string_trim/2, leer_linea/1, leer_linea_archivo/2, read_line_to_string/2,
                 validar_fecha/1, sort_eventos/2, recursos_ocupados_en_fecha/2, ocupado_cant/3]).

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