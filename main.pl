:- use_module(core, [iniciar/0]).

% ========== PREDICADO PRINCIPAL ==========
main :-
    catch(
        iniciar,
        Error,
        (format('❌ Error crítico: ~w~n', [Error]),
         halt(1))
    ),
    halt(0).