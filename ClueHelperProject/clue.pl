% Cheng Yu Yao 
% 36210128
% e5k8
% Dongfang Amon Ge
% 20021127
% y8n8

% Program to help play the game CLUE.


% Sets up the game
clue :-
									nl,
									write('Which version of the game do you have?'),nl,
									write('You have the old version if you have a ballroom, lead pipe, etc.'),nl,
									write('You have the new version if you have a patio, bat, etc.'),nl,
									selectfromlist([old,new],Version),
						
									init_ver(Version, Weapons, Locations),
									nl,

									Characters = [mustard, scarlet, plum, green, white, peacock],
						
									write('How many cards do you have?'),nl,nl,
									read(Nc),
									own_cards(Nc, Characters, Weapons, Locations, Newchar, Newweap, Newloc),
									
									append(Newchar,Newweap, Temp),
									append(Newloc,Temp, Total),
									
									nl,
									write('How many players are there?'),nl,
									read(Num),nl,
									
									init_game(Num,Newchar,Newweap,Newloc,Total).

% The list of weapon and location in the new and old version									
init_ver(new, 
		[knife, candlestick, pistol, rope, bat, ax], 
		[kitchen, patio, spa, theatre, livingroom, 
		observatory, hall, guesthouse, diningroom]).
		
init_ver(old, 
		[knife, candlestick, revolver, rope, leadpipe, wrench],
		[kitchen, ballroom, conservatory, billiardroom, library, 
		study, hall, lounge, diningroom]).									
									
% Different initialization of game based on how many players
init_game(2,C,W,L,T) :-
									read_starting_player(Sp),
									game_state(Sp,2,C,W,L,T,[],[],[],[]).
init_game(3,C,W,L,T) :-
									read_starting_player(Sp),
									game_state(Sp,3,C,W,L,T,T,[],[],[]).
init_game(4,C,W,L,T) :-
									read_starting_player(Sp),
									game_state(Sp,4,C,W,L,T,T,T,[],[]).
init_game(5,C,W,L,T) :-
									read_starting_player(Sp),
									game_state(Sp,5,C,W,L,T,T,T,T,[]).
init_game(6,C,W,L,T) :-
									read_starting_player(Sp),
									game_state(Sp,6,C,W,L,T,T,T,T,T).

% Get the starting player as input.
read_starting_player(Sp) :-
	write('Who is the starting player?'),nl,
	write('0: you, 1: player on your left, 2: player two to your left, etc.'),nl,		
	read(Sp).
							
% If player has no more cards
% Pass the list with the cards removed
own_cards(0,C,W,L,C,W,L).

% If player has more cards on hand
% Delete that card and recurse
own_cards(N,C,W,L,Nc,Nw,Nl) :- nl,
									write('What is card #'),
									write(N),
									write('?'),nl,

									write('Category: '),nl,
									selectfromlist([character, weapon, location], Category),
									select_card(Category,C,W,L,Card),

									delete(C, Card, Dc),
									delete(W, Card, Dw),
									delete(L, Card, Dl),
									X is N-1,
									own_cards(X,Dc,Dw,Dl,Nc,Nw,Nl).

select_card(character,C,_,_,Card) :-
	write('Card: '),nl,
	selectfromlist(C, Card).

select_card(weapon,_,W,_,Card) :-
	write('Card: '),nl,
	selectfromlist(W, Card).

select_card(location,_,_,L,Card) :-
	write('Card: '),nl,
	selectfromlist(L, Card).


% Turn manager									
game_state(Curr_Player,Num_Players,C,W,L,P1,P2,P3,P4,P5) :- nl,

									write('Player '),
									write(Curr_Player),
									write('\'s turn.'),nl,
									turn(Curr_Player,Num_Players,C,W,L,P1,P2,P3,P4,P5).
						

% If it's your turn 
% Suggestions are given by subtracting the possible cards of 
% other players from the cards un-shown to the player

turn(0,N,C,W,L,P1,P2,P3,P4,P5) :-
									write('You should suggest one of the following CHARACTERS:'),nl,
									subtract(C,P1,Tempc1),
									subtract(Tempc1,P2,Tempc2),
									subtract(Tempc2,P3,Tempc3),
									subtract(Tempc3,P4,Tempc4),
									subtract(Tempc4,P5,Sugc),
									write(Sugc),
									write(', other choices are '),
									write(C),nl,
									
									nl,
									write('You should suggest one of the following WEAPONS:'),nl,
									subtract(W,P1,Tempw1),
									subtract(Tempw1,P2,Tempw2),
									subtract(Tempw2,P3,Tempw3),
									subtract(Tempw3,P4,Tempw4),
									subtract(Tempw4,P5,Sugw),
									write(Sugw),
									write(', other choices are '),
									write(W),nl,

									nl,
									write('You should suggest one of the following LOCATIONS:'),nl,
									subtract(L,P1,Templ1),
									subtract(Templ1,P2,Templ2),
									subtract(Templ2,P3,Templ3),
									subtract(Templ3,P4,Templ4),
									subtract(Templ4,P5,Sugl),
									write(Sugl),
									write(', other choices are '),
									write(L),nl,

									nl,
									write('Which character did you suggest?'),nl,
									selectfromlist([none|C], Sc),
																
									nl,
									write('Which weapon did you suggest?'),nl,
									selectfromlist([none|W], Sw),
																		
									nl,
									write('Which location did you suggest?'),nl,
									selectfromlist([none|L], Sl),
								
								
									write('Which card was shown to you?'),nl,

									make_suggestions(Sc, Sw, Sl, Suggestions),

									selectfromlist(Suggestions, Shown),
						
									showing(N,Shown,C,W,L,Sc,Sw,Sl,P1,P2,P3,P4,P5).

					
% If it's player1's turn
% (Applies to all other players) 
% Subtract their suggestion from the possible cards they may have						
turn(1,N,C,W,L,P1,P2,P3,P4,P5) :- 

									write('Which character did they suggest?'),nl,
									selectfromlist([none|C], Sc),
									delete(P1,Sc,Tempc),
									write('Which weapon did they suggest?'),nl,
									selectfromlist([none|W], Sw),
									delete(Tempc,Sw,Tempw),
									write('Which location did they suggest?'),nl,
									selectfromlist([none|L], Sl),
									delete(Tempw,Sl,Np1),
									
									rot(1,N,Next_Player),
									game_state(Next_Player,N,C,W,L,Np1,P2,P3,P4,P5).

% If it's player2's turn						
turn(2,N,C,W,L,P1,P2,P3,P4,P5) :- 

									write('Which character did they suggest?'),nl,
									selectfromlist([none|C], Sc),
									delete(P2,Sc,Tempc),
									write('Which weapon did they suggest?'),nl,
									selectfromlist([none|W], Sw),
									delete(Tempc,Sw,Tempw),
									write('Which location did they suggest?'),nl,
									selectfromlist([none|L], Sl),
									delete(Tempw,Sl,Np2),
									
									rot(2,N,Next_Player),
									game_state(Next_Player,N,C,W,L,P1,Np2,P3,P4,P5).

% If it's player3's turn						
turn(3,N,C,W,L,P1,P2,P3,P4,P5) :- 

									write('Which character did they suggest?'),nl,
									selectfromlist([none|C], Sc),
									delete(P3,Sc,Tempc),
									write('Which weapon did they suggest?'),nl,
									selectfromlist([none|W], Sw),
									delete(Tempc,Sw,Tempw),
									write('Which location did they suggest?'),nl,
									selectfromlist([none|L], Sl),
									delete(Tempw,Sl,Np3),
									
									rot(3,N,Next_Player),
									game_state(Next_Player,N,C,W,L,P1,P2,Np3,P4,P5).

% If it's player4's turn						
turn(4,N,C,W,L,P1,P2,P3,P4,P5) :- 

									write('Which character did they suggest?'),nl,
									selectfromlist([none|C], Sc),
									delete(P4,Sc,Tempc),
									write('Which weapon did they suggest?'),nl,
									selectfromlist([none|W], Sw),
									delete(Tempc,Sw,Tempw),
									write('Which location did they suggest?'),nl,
									selectfromlist([none|L], Sl),
									delete(Tempw,Sl,Np4),
									
									rot(4,N,Next_Player),
									game_state(Next_Player,N,C,W,L,P1,P2,P3,Np4,P5).

% If it's player5's turn						
turn(5,N,C,W,L,P1,P2,P3,P4,P5) :- 

									write('Which character did they suggest?'),nl,
									selectfromlist([none|C], Sc),
									delete(P5,Sc,Tempc),
									write('Which weapon did they suggest?'),nl,
									selectfromlist([none|W], Sw),
									delete(Tempc,Sw,Tempw),
									write('Which location did they suggest?'),nl,
									selectfromlist([none|L], Sl),
									delete(Tempw,Sl,Np5),
									
									rot(5,N,Next_Player),
									game_state(Next_Player,N,C,W,L,P1,P2,P3,P4,Np5).
						

% If no suggestion was made, then no one can show you anything.
% If a suggestion was made, then the only possible cards anyone can show you are the char, weapon, or loc you suggested.						
make_suggestions(none, none, none, Suggestions) :-
	Suggestions = [n/a].
make_suggestions(Sc,Sw,Sl, Suggestions) :- 
	Suggestions = [none,Sc,Sw,Sl].

% If nothing has been shown
% Then it must be it!
showing(_,none,_,_,_,Sc,Sw,Sl,_,_,_,_,_) :- 
	write('We found the criminal! Accuse: '),
	write(Sc), 
	write(' in the '), 
	write(Sl), 
	write(' with the '),
	write(Sw),write('!'),nl.
	% turn(0,N,[Sc],[Sw],[Sl],_,_,_,_,_).

% If a card has been shown
% Delete that card from card un-shown to the player
showing(N,Shown,C,W,L,_,_,_,P1,P2,P3,P4,P5) :- 
									delete(C, Shown, Dc),
									delete(W, Shown, Dw),
									delete(L, Shown, Dl),

									% advance - next player is always player 1
									game_state(1,N,Dc,Dw,Dl,P1,P2,P3,P4,P5).
		
% Gets next player given C: current player, T: total players
rot(C, T, N) :-
	N is (C + 1) mod T.


% Prints a list with indexes, reads index, returns what element was selected
selectfromlist(L, Selected)	:-		
	printlist(L, 1),
	read(Index),nl,
	nth1(Index, L, Selected).

% Prints a list with each element's index (starting from 1)	
printlist([], _).
printlist([H|T], N) :-
	
	write(N),
	write(': '),
	write(H), nl,
	N1 is N + 1,
	printlist(T, N1).