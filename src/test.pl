% test.pl -- tests generated during development

% state(State), test_empty_cells(State, 'out.json').
% test for setting matches to empty
test_empty_cells(State, Filename1, Filename2) :-
    export_state_to_json(State, Filename1),
    find_all_matches(State, All),
    remove_matches(All, State, NewState),
    export_state_to_json(NewState, Filename2).

% state3(State), test_gravity(State, 'p1.json', 'p2.json', 'p3.json').
% test for applying gravity to grid 
test_gravity(State, File1, File2, File3) :-
    export_state_to_json(State, File1),
    find_all_matches(State, All),
    remove_matches(All, State, NewState),
    export_state_to_json(NewState, File2),
    apply_gravity(NewState, Final),
    export_state_to_json(Final, File3).