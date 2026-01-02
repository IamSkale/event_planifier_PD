:- module(data, []).

% Archivos de persistencia
archivo_eventos('eventos.txt').
archivo_recursos('recursos.txt').
archivo_inventario('inventario.txt').

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