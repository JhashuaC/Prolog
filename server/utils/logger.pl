% server/utils/logger.pl
:- module(logger, [log/3]).
log(Color, Fmt, Args) :-
    with_output_to(user_output, ansi_format([fg(Color)], Fmt, Args)).
