% server/paths.pl
:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(project_root, Dir)),
   asserta(user:file_search_path(core, project_root('../knowledge/core'))),
   asserta(user:file_search_path(server, project_root('../server'))),
   asserta(user:file_search_path(knowledge, project_root('../knowledge'))).
