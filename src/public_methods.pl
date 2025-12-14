%===============================================================================================%
%   Public Methods:                                                                             %
%===============================================================================================%

% Use to Check if piece is part of a match in a row
piece_in_match(State, Pos) :-
    rows(Rows),
    get_row(Pos, R),
    nth0(R, Rows, [_|T1]),
    piece_at(State, Pos, Type),
    traverse_for_match(State, Type, T1, [Pos], [], Final),
    member_sublist(Pos, Final).

% Use to check if the piece is part of a Col match
piece_in_match(State, Pos) :-
    cols(Cols),
    get_col(Pos, C),
    nth0(C, Cols, [_|T2]),
    piece_at(State, Pos, Type),
    traverse_for_match(State, Type, T2, [Pos], [], Final2),
    member_sublist(Pos, Final2).

