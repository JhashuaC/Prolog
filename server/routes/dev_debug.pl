% server/routes/dev_debug.pl
:- module(dev_debug, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- http_handler(root(ping), ping, []).
:- http_handler(root(debug/handlers), debug_handlers, []).

ping(_Req) :-
	reply_json_dict(_{ok : true}).

debug_handlers(_Req) :-
	findall(_{
			path : PathStr,
			pred : PredStr,
			opts : OptsStr
			},
		(http_dispatch : handler(Path, Pred, Opts, _),
			term_string(Path, PathStr),
			term_string(Pred, PredStr),
			term_string(Opts, OptsStr)),
		Handlers),
	reply_json_dict(_{handlers : Handlers}).
