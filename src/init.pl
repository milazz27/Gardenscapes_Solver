

initial_state(State) :-
    findall(piece(Type, Row, Col), piece(Type, Row, Col), State).