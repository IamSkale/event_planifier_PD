:- module(utils, [string_trim/2, leer_linea/1, leer_linea_archivo/2, read_line_to_string/2,
                 sort_eventos/2]).
:- use_module(charging, [limpiar_lista_recursos/2]).
:- use_module(saving, [guardar_todo/0, recurso_a_string/2]).
:- use_module(dates_work, [fecha_actual/1,fecha_siguiente/2,generar_fechas_consecutivas/3]).

% ========== FUNCIONES AUXILIARES ==========

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

sort_eventos(Eventos, Ordenados) :-
    predsort(comparar_eventos, Eventos, Ordenados).

comparar_eventos(Orden, [Nombre1, Fecha1, Duracion1], [Nombre2, Fecha2, Duracion2]) :-
    (Fecha1 @< Fecha2 -> Orden = <
    ; Fecha1 @> Fecha2 -> Orden = >
    ; Nombre1 @< Nombre2 -> Orden = <
    ; Nombre1 @> Nombre2 -> Orden = >
    ; Duracion1 @< Duracion2 -> Orden = <
    ; Duracion1 @> Duracion2 -> Orden = >
    ; Orden = =
    ).

