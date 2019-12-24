%%%% -*- Mode: Prolog -*-
%%% lmc.pl
%%% Progetto prolog
%%% 844824_Pietrasanta_Davide_LP_E1P_2018_LMC
%%% 814247_Orsanigo_Veronica_LP_E1P_2018_LMC



%%% SEZIONE ESECUZIONE


%% Stato definizione

state(Acc, Pc, Mem, In, Out, Flag):-
      check(Acc, Pc, Mem, In, Out, Flag).

halted_state(Acc, Pc, Mem, In, Out, Flag):-
      check(Acc, Pc, Mem, In, Out, Flag).


%% esegue istruzione singola controllando che si passi uno stato

one_instruction(State, NewState):-
      functor(State, state, 6),
      check_state_2(State),
      esegui_istr(State, NewState), !.


%% controlla stato e suoi parametri

check_state_2(state(Acc, Pc, Mem, In, Out, Flag)):-
      check(Acc, Pc, Mem, In, Out, Flag),
      nth0(Pc, Mem, Istr, _Resto),
      input_istr(Istr, In).


input_istr(Istr, _In):-
      Istr \= 901, ! .
input_istr(Istr, In):-
      compare('=', Istr, 901),
      In \= [].


incrementa_Pc(Pc1, Pc2):-
      Res is Pc1 + 1,
      Res > 99,
      Pc2 is Res mod 100, !.
incrementa_Pc(Pc1, Pc2):-
      Res is Pc1 + 1,
      Res =< 99,
      Pc2 is Res.


%% esegue ciclo di istruzioni

execution_loop(State, Out1):-
      decidi(State, Out1).

decidi(State, Out):-
      functor(State, halted_state, 6), % controlla se halted_state
      recupera_arg_state(State, Out), !.
decidi(State, Out1):-
      one_instruction(State, NewState),
      decidi(NewState, Out1).

recupera_arg_state(State, Arg):-
      State =..L,
      nth0(5, L, Arg, _Rest).


%% crea la memoria di 100 elementi

crea_memoria_completa(100, Mem, Mem):- !.
crea_memoria_completa(N, Mem1, Mem3):-
      nth0(N, Mem2, 0, Mem1),
      N1 is N + 1,
      crea_memoria_completa(N1, Mem2, Mem3), !.


%% crea la memoria di LMC, se num istruzioni > 100 fallisce, se
%% etichette doppie/file assembbly non ben formato fallisce

lmc_load(FileName, Mem):-
      NumLab is 0,
      file_into_list_of_lists(FileName, FileList),
      loop_check_dat(FileList),
      length(FileList, Length), % controlla num istruzioni <= 100
      Length =< 100,
      loop_label_to(FileList, ListIstr, ListLabel),
      check_double_label(ListLabel, ListLabel, ListNum),
      max(ListNum, Num),
      Num < 2,
      find_all_label(ListLabel, NumLab, ListIstr, ListIstr1),
      loop_list_of_type_flatten(ListIstr1, ListIstr2),
      loop_frase_value(ListIstr2, Mem1),
      length(Mem1, N),
      crea_memoria_completa(N, Mem1, Mem), !.
lmc_load(FileName, Mem):-
      file_into_list_of_lists(FileName, []),
      list_of_zero(100, Mem), !. % lista di 100 zero: file vuoto

lmc_run(Filename, In, Output):-
      lmc_load(Filename,Mem),
      execution_loop(state(0, 0, Mem, In, [], noflag), Output), !.



%%% SEZIONE FILE


%% Legge un file e lo salva come lista in List.

file(Filename, List):-
      my_read_file_to_string(Filename, String1),
      string_lower(String1, String),
      split_string(String, "\n", "\n\t\s\r", List).

my_read_file_to_string(Filename, Result):-
      my_read_file_to_string(Filename, Result, []).
my_read_file_to_string(Filename, Result, Options):-
      open(Filename, read, In, Options),
      read_string(In, _, Result),
      close(In).


%% converte da stringa a lista ma non viceversa

converter(List, String):-
      atom_codes(A, String), atom_chars(A, List).

%% converte da lista a stringa ma non viceversa

converter1(List, String):-
      atomic_list_concat(List, '', Atom),
      atom_string(Atom, String).


%% rimuove i commenti //

remove_comment_list([],[]):-!.
remove_comment_list([X], [X]):-
      X \= '/', !.
remove_comment_list([X | Xs],[X | Ys]):-
      X \= '/',
      remove_comment_list(Xs,Ys), !.
remove_comment_list([X, Y | _List1], []):-
      X = '/',
      Y = '/', !.
remove_comment_list([X, Y, Z | List1], [X, Y, Z | List2]):-
      X \= '/',
      Y = '/',
      Z \= '/',
      remove_comment_list( List1, List2), !.
remove_comment_list([X, Y | List1], [X, Y | List2]):-
      X = '/',
      Y \= '/',
      remove_comment_list( List1 , List2), !.
remove_comment_list([X, Y, Z | _List1], [X]):-
      X \= '/',
      Y = '/',
      Z = '/',!.


%% rimuove commenti da una stringa

remove_comment_string(String, NewString):-
      converter(List, String),
      remove_comment_list(List, NewList),
      converter1(NewList, NewString).

%% rimuove i commenti da una lista di stringhe

remove_comment_list_of_string([], []):-!.
remove_comment_list_of_string([X], [Y]):-
      remove_comment_string(X, Y),!.
remove_comment_list_of_string([X | Xs], [Y | Ys]):-
      remove_comment_string(X, Y),
      remove_comment_list_of_string(Xs, Ys).


%% Mette il file in una lista di stringhe, ciascuna identifica una riga
%% dalla quale sono stati eliminati commenti, spazi (iniziali),
%% tab (iniziali), carriage, a capo

file_into_list(Filename, List):-
      file(Filename, ListCommentata), % ListCommentata: lista di stringhe
      remove_comment_list_of_string(ListCommentata, List1),
      remove_empty_string(List1, List).


%% Data una lista di stringhe, divide ciascuna stringa in parole
%% (stringhe), che costituiscono un'altra stringa
%% Es: ["add 2", "LOOP sub 3", ...]
%% --> [["Add", "2"], ["LOOP", "sub", "3"], ...]

list_of_lists([], []):- !.
list_of_lists([X], [L]):-
      split_string(X, "\s", "\s\t", L),!. % qui si ignorano gli spazi
list_of_lists([X | Xs], [Y | Zs]):-
      split_string(X, "\s", "\s\t", Y),
      list_of_lists(Xs, Zs), !.


%% Dato un file, lo mette in una lista di liste definite come in
%% list_of_lists e poi formattato a seconda che stringhe siano numeri
%% oppure atomi semplici

file_into_list_of_lists(Filename, List):-
      file_into_list(Filename, List1),
      list_of_lists(List1, List2),
      loop_list_of_type(List2, List).


%% rimuove stringhe vuote

remove_empty_string([], []):- !.
remove_empty_string([X], [X]):-
      X \= "", !.
remove_empty_string([X | Xs], [X | Ys]):-
      X \= "",
      remove_empty_string(Xs, Ys), !.
remove_empty_string(["" | Xs], Ys):-
      remove_empty_string(Xs, Ys).


%% tipi possibili: number (relativi alle istruzioni), atom generico
%% (istruzioni ed etichette)
%% se String rappresenta un numero, Res e' il numero in tipo number,
%% altrimenti fallisce

converti_tipo(String, Res):-
      number_string(Res, String), !.

%% se non e' un numero scarta opzione precedente e applica questa,
%% quindi dalla stringa si ottiene una lista di caratteri su cui
%% applicare il parsing, quindi controllare se e' un'istruzione o
%% etichetta

converti_tipo(String, Res):-
      string_chars(String, Res).


%% usiamo coverti tipo per passare da lista di liste di stringhe
%% (es: [["add", "2"], ["LOOP", "sub", "3"], ...])
%% a lista di liste di numeri/liste di char, poi lo applichiamo in loop

list_of_type([], []):- !.
list_of_type([X], [Y]):-
      converti_tipo(X, Y),!.
list_of_type([X | Xs], [Y | Ys]):-
      converti_tipo(X, Y),
      list_of_type(Xs, Ys), !.

loop_list_of_type([], []):- !.
loop_list_of_type([X], [Y]):-
      list_of_type(X, Y), !.
loop_list_of_type([X | Xs], [Y | Ys]):-
      list_of_type(X, Y),
      loop_list_of_type(Xs, Ys).


%% flatten delle liste determinate con predicato precedente per poterle
%% usare nel parsing, per come definito

loop_list_of_type_flatten([], []):- !.
loop_list_of_type_flatten([X], [Y]):-
      flatten(X,Y), !.
loop_list_of_type_flatten([X | Xs], [Y | Ys]):-
      flatten(X, Y),
      loop_list_of_type_flatten(Xs, Ys), !.



%%% SEZIONE PARSING


frase(Ris) --> space, istr1(X), space, num(XX), space, {Ris is X + XX}.
frase(Ris) --> space, istrdat(X), space, numDat(XX), space, {Ris is X + XX}.
frase(Ris) --> space, istr0(X), space, {Ris is X}.


%% definizione istruzioni per parsing

istr0(X) --> (['I'];['i']) -> (['N'];['n']) -> (['P'];['p']), {X is 901}.
istr0(X) --> (['H'];['h']) -> (['L'];['l']) -> (['T'];['t']), {X is 000}.
istr0(X) --> (['O'];['o']) -> (['U'];['u']) -> (['T'];['t']), {X is 902}.
istr0(X) --> (['D'];['d']) -> (['A'];['a']) -> (['T'];['t']), {X is 0}.

istr1(X) --> (['A'];['a']) -> (['D'];['d']) -> (['D'];['d']), {X is 100}.
istr1(X) --> (['S'];['s']) -> (['U'];['u']) -> (['B'];['b']), {X is 200}.
istr1(X) --> (['S'];['s']) -> (['T'];['t']) -> (['A'];['a']), {X is 300}.
istr1(X) --> (['L'];['l']) -> (['D'];['d']) -> (['A'];['a']), {X is 500}.
istr1(X) --> (['B'];['b']) -> (['R'];['r']) -> (['A'];['a']), {X is 600}.
istr1(X) --> (['B'];['b']) -> (['R'];['r']) -> (['Z'];['z']), {X is 700}.
istr1(X) --> (['B'];['b']) -> (['R'];['r']) -> (['P'];['p']), {X is 800}.

istrdat(X) --> (['D'];['d']) -> (['A'];['a']) -> (['T'];['t']), {X is 0}.


num(X) --> [X],  {number(X), (X >= 0) , (X < 100) }.
numDat(X) --> [X], {number(X), (X >= 0), (X < 1000) }.
space --> [' '], space.
space -->[].


%% trasforma il file in assembly (senza etichette) in linguaggio macchina
%% In caso di commento / o di operazioni non permesse ritorna false

frase_value(L, V) :- frase(V, L, []), !.

loop_frase_value([],[]):- !.
loop_frase_value([X], [Y]):-
      frase_value([X], Y), !.
loop_frase_value([X | Xs], [Val |ListNum]):-
      frase_value(X, Val),
      loop_frase_value( Xs, ListNum), !.



%%% SEZIONE ETICHETTE


%% Verifica che una lista di caratteri sia una istruzione

is_istr(List):-
      phrase(istr0, List), !.
is_istr(List):-
      phrase(istr1, List), !.
is_istr(List):-
      phrase(istrdat, List), !.


%% Verifica che una lista di caratteri sia una etichetta

is_label(List):-
      labelList(List),
      \+ is_istr(List),
      !.


labelList([]):-!.
labelList([X]):-
      char_type(X, alpha),!.
labelList([X | Xs]):-
      char_type(X, alpha),
      alphanumeric(Xs),!.


alphanumeric([]):-!.
alphanumeric([X]):-
      char_type(X, alnum),!.
alphanumeric([X | Xs]):-
      char_type(X, alnum),
      alphanumeric(Xs),!.


istr0 --> (['I'];['i']) -> (['N'];['n']) -> (['P'];['p']).
istr0 --> (['H'];['h']) -> (['L'];['l']) -> (['T'];['t']).
istr0 --> (['O'];['o']) -> (['U'];['u']) -> (['T'];['t']).
istr0 --> (['D'];['d']) -> (['A'];['a']) -> (['T'];['t']).

istr1 --> (['A'];['a']) -> (['D'];['d']) -> (['D'];['d']).
istr1 --> (['S'];['s']) -> (['U'];['u']) -> (['B'];['b']).
istr1 --> (['S'];['s']) -> (['T'];['t']) -> (['A'];['a']).
istr1 --> (['L'];['l']) -> (['D'];['d']) -> (['A'];['a']).
istr1 --> (['B'];['b']) -> (['R'];['r']) -> (['A'];['a']).
istr1 --> (['B'];['b']) -> (['R'];['r']) -> (['Z'];['z']).
istr1 --> (['B'];['b']) -> (['R'];['r']) -> (['P'];['p']).

istrdat --> (['D'];['d']) -> (['A'];['a']) -> (['T'];['t']).


%% determina le etichette a inizio riga e crea una lista (L) dove
%% inserirle, se a inizio riga non c'e' un'etichetta si inserisce 0
%% nella lista, in modo che ogni etichetta sia nella posizione giusta,
%% inoltre si eliminano dalla lista delle istruzioni le etichette a
%% inizio riga e si rida' false se ci sono etichette doppie

loop_label_to([], [], []):- !.
loop_label_to([L], [L1], [X]):-
      label_to(L, X, L1), !.
loop_label_to([L | ListIstr], ListIstr2, LabelList2):-
      label_to(L, X, L1),
      append([X], LabelList1, LabelList2),
      append([L1], ListIstr1, ListIstr2),
      loop_label_to(ListIstr, ListIstr1, LabelList1).

label_to([L | ListIstr], L, ListIstr):-
      is_label(L), !. % etichetta in una lista: es [l,a,b,el]
label_to([L | ListIstr], N, [L | ListIstr]):-
      N is 0.


%% cerca l'etichetta passata in una lista, che e' una istruzione e
%% sostituisce l'etichetta, se la trova, con il numero corrispondente
%% (Num) che indica la posizione in memoria dell'etichetta

find_label_in_one_list([], _Label, _Num, []):- !.
find_label_in_one_list([Label], Label, Num, [Num]):- !.
find_label_in_one_list([X], Label, _Num, [X]):-
      X \= Label, !.
find_label_in_one_list([Label | Xs], Label, Num, [Num | Ys]):-
      find_label_in_one_list(Xs, Label, Num, Ys), !.
find_label_in_one_list([X | Xs], Label, Num, [X | Ys]):-
      X \= Label,
      find_label_in_one_list(Xs, Label, Num, Ys), !.

check_double_label([], _LabelList, []):- !.
check_double_label([0 | Label], LabelList, NumLabel):-
      check_double_label(Label, LabelList, NumLabel), !.
check_double_label([X | Label], LabelList, [Num | NumLabel]):-
      occurrences(LabelList, X, Num),
      check_double_label(Label, LabelList, NumLabel), !.


%% predicato precedente, ma applicato a una lista di liste, quindi a una
%% serie di istruzioni

find_label_in_list_of_lists([], _Label, _Num, []).
find_label_in_list_of_lists([X], Label, Num, [Y]):-
      find_label_in_one_list(X, Label, Num, Y), !.
find_label_in_list_of_lists([X | Xs], Label, Num, [Y | Ys]):-
      find_label_in_one_list(X, Label, Num, Y),
      find_label_in_list_of_lists(Xs, Label, Num, Ys), !.


%% e' passata la lista delle etichette, ognuna nella giusta posizione,
%% cosi', partendo dalla posizione zero (si parte con NumLab = 0), si
%% scorre la lista delle etichette e si cerca ogni etichetta nella lista
%% delle istruzioni, se la si trova la si sotituisce usando
%% find_label_in_list_of_lists e si crea la lista di istruzioni con
%% etichette risolte

find_all_label([0], _NumLab, ListIstr, ListIstr):- !.
find_all_label([L], NumLab, ListIstr, ListIstr1):-
      find_label_in_list_of_lists(ListIstr, L, NumLab, ListIstr1), !.
find_all_label([0 | ListLabel], NumLab, ListIstr, ListIstr2):-
      NumLab1 is NumLab + 1,
      find_all_label(ListLabel, NumLab1, ListIstr, ListIstr2), !.
find_all_label([L | ListLabel], NumLab, ListIstr, ListIstr2):-
      find_label_in_list_of_lists(ListIstr, L, NumLab, ListIstr1),
      NumLab1 is NumLab + 1,
      find_all_label(ListLabel, NumLab1, ListIstr1, ListIstr2), !.



%%% SEZIONE ISTRUZIONI


%% ADD istr

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, _Flag1),
            state(Acc2, Pc2, Mem1, In1, Out1, Flag2)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, XX, First), % XX: numero cella con numero da sommare,
      element_at(Num, Mem1, XX), % First: prima cifra dell'istr
      compare('=', First, 1),
      add(Acc1, Num, state(Acc2, Pc2, Mem1, In1, Out1, Flag2)),
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.

%% istr somma > 999

add(X, Y, state(Acc, _Pc, _Mem, _In, _Out, flag)):-
      Res is X + Y,
      Res > 999,
      Acc is Res mod 1000, !.

%% istr somma <= 999

add(X, Y, state(Acc, _Pc, _Mem, _In, _Out, noflag)):-
      Res is X + Y,
      Res =< 999,
      Acc is Res.


%% SUB istr

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, _Flag1),
            state(Acc2, Pc2, Mem1, In1, Out1, Flag2)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, XX, First),
      element_at(Num, Mem1, XX),
      compare('=', First, 2),
      sub(Acc1, Num, state(Acc2, Pc2, Mem1, In1, Out1, Flag2)),
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.

%% istr sub < 0

sub(X, Y, state(Acc, _Pc, _Mem, _In, _Out, flag)):-
      Res is X - Y,
      Res < 0,
      Acc is 999 + Res + 1, !.

%% istr sub >= 0

sub(X, Y, state(Acc, _Pc, _Mem, _In, _Out, noflag)):-
      Res is X - Y,
      Res >= 0,
      Acc is Res.


%% STORE istr

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, Flag1),
            state(Acc1, Pc2, Mem2, In1, Out1, Flag1)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, XX, First),
      compare('=', First, 3),
      change_at(Acc1, Mem1, XX, Mem2),
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.


%% LOAD istr

esegui_istr(state(_Acc1, Pc1, Mem1, In1, Out1, Flag1),
            state(Acc2, Pc2, Mem1, In1, Out1, Flag1)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, XX, First),
      compare('=', First, 5),
      element_at(Num, Mem1, XX),
      Acc2 is Num,
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.


%% BRANCH istr

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, Flag1),
            state(Acc1, Pc2, Mem1, In1, Out1, Flag1)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, XX, First),
      compare('=', First, 6),
      Pc2 is XX, !.


%% BRANCH IF ZERO istr

esegui_istr(state(0, Pc1, Mem1, In1, Out1, noflag),
            state(0, Pc2, Mem1, In1, Out1, noflag)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, XX, First),
      compare('=', First, 7),
      Pc2 is XX, !.

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, Flag1),
            state(Acc1, Pc2, Mem1, In1, Out1, Flag1)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, _XX, First),
      compare('=', First, 7),
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.


%% BRANCH IF POSITIVE istr

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, noflag),
            state(Acc1, Pc2, Mem1, In1, Out1, noflag)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, XX, First),
      compare('=', First, 8),
      Pc2 is XX, !.

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, Flag1),
            state(Acc1, Pc2, Mem1, In1, Out1, Flag1)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, _XX, First),
      compare('=', First, 8),
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.


%% INPUT istr

esegui_istr(state(_Acc1, Pc1, Mem1, [In1|In1s], Out1, Flag1),
            state(Acc2, Pc2, Mem1, In1s, Out1, Flag1)):-
      element_at(Istr, Mem1, Pc1),
      compare('=', Istr, 901),
      Acc2 is In1,
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.


%% OUTPUT istr

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, Flag1),
            state(Acc1, Pc2, Mem1, In1, Out2, Flag1)):-
      element_at(Istr, Mem1, Pc1),
      compare('=', Istr, 902),
      append(Out1, [Acc1], Out2),
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.


%% HALT istr

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, Flag1),
            halted_state(Acc1, Pc2, Mem1, In1, Out1, Flag1)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, _XX, First),
      compare('=', First, 0),
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.

esegui_istr(state(Acc1, Pc1, Mem1, In1, Out1, Flag1),
            halted_state(Acc1, Pc2, Mem1, In1, Out1, Flag1)):-
      element_at(Istr, Mem1, Pc1),
      discovery(Istr, _XX, First),
      compare('=', First, 0),
      incrementa_Pc(Pc1, Pc),
      Pc2 is Pc, !.



%%% SEZIONE CONTROLLI


%% Controlla che una Lista abbia solo valore in [0, 999]

check_List([]).
check_List([X | Xs]):-
      integer(X), X < 1000, X >= 0,
      check_List(Xs).


%% Controlla Flag

check_Flag(Flag):-
      compare('=', Flag, 'flag'), ! ;
      compare('=', Flag, 'noflag').


%% Controlla lunghezza memoria e elementi memoria

check_Mem(Mem):-
      proper_length(Mem, 100),
      check_List(Mem).


%% Controlla tutto riguardo uno stato

check(Acc, Pc, Mem, In, Out, Flag):-
      integer(Acc), Acc < 1000, Acc >= 0,
      integer(Pc), Pc < 100, Pc >= 0,
      check_Mem(Mem),
      check_List(In),
      check_List(Out),
      check_Flag(Flag).


%% controlla che in una lista di istruzioni la dat sia usata senza una
%% etichetta dopo

loop_check_dat([L]):-
      check_dat(L), !.
loop_check_dat([L | ListIstr]):-
      check_dat(L),
      loop_check_dat(ListIstr),!.

%% controlla la dat, ma in una riga singola del file

check_dat([[d, a, t], X | _Riga]):-
      number(X), !.
check_dat([_R, [d, a, t], X | _Riga]):-
      number(X), !.
check_dat([_X, [d, a, t]]):- !.
check_dat([R, X | _Riga]):-
      X  \== [d, a, t],
      R \== [d, a, t], !.
check_dat([_X]):- !.
check_dat([[]]):- !.


%% SEZIONE ALTRO


%% determina elemto in una determinata posizione in una lista

element_at(X, [X | _T], 0):-!.
element_at(X, [_Y | T], N1):-
      N1 > 0,
      N is N1 - 1,
      T \= [],
      element_at(X, T, N).


%% Cambia l'ennesimo elemento di una lista con NewElement

change_at(NewElement, [_X | Y], 0, [NewElement | Y]).
change_at(NewElement, [], 0, [NewElement]).
change_at(NewElement, [X | Y], N, [X | NewList]):-
    N > 0,
    N1 is N - 1,
    change_at(NewElement, Y, N1, NewList).


%% discovery trova, dato X (es. 249), XX (49) e First(2)

discovery(X, XX, First):-
      nonvar(X), var(XX), var(First),
      XX is (X mod 100),
      First is ( (X - XX) / 100).


%% crea lista di zero

list_of_zero(0, []):- !.
list_of_zero(1, [0]):- !.
list_of_zero(Len, [0 | List]):-
      Len > 1,
      NewLen is Len - 1,
      list_of_zero(NewLen, List), !.


%% conta il numero di occorrenze di un elemento in una lista

occurrences([],_,0):- !.
occurrences([X|Y],X,N):-
      occurrences(Y,X,W),
      N is W + 1, !.
occurrences([X|Y],Z,N):-
      occurrences(Y,Z,N),
      X\=Z, !.


%% determina max in lista di numeri

max([], X):-
      X is 0, !.
max([X],X):- !.
max([H|T], Y):-
      max(T,Y),
      H < Y, !.
max([H|T], H):-
      max(T,Y),
      H >= Y, !.



%%%% end of file -- lmc.pl



























