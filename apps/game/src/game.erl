-module(game).
-include_lib("game/include/game.hrl").
-include_lib("cmon/include/logger.hrl").
-compile(export_all).

gen_stash() -> [[C, N] || C <- [1,2,3,4,1,2,3,4], N <- [1,2,3,4,5,6,7,8,9,10,11,12,13]].

random_peek(L) ->
	E = lists:nth(random:uniform(length(L)), L),
	{E, lists:delete(E, L)}.

gen_hand(1, Hand, Stash) ->
	{Hand, Stash};
gen_hand(N, Hand, Stash) -> 
	{E, Stash1} = random_peek(Stash),
	gen_hand(N-1, [E] ++ Hand, Stash1).

gen_game() ->
	Stash = gen_stash(),
	{Jocker, Stash1} = random_peek(Stash),
    {Player1, Stash2} = gen_hand(14, [], [[0,0], [0,0]] ++ Stash1),
    {Player2, Stash3} = gen_hand(14, [], Stash2),
    {Player3, Stash4} = gen_hand(14, [], Stash3),
    {Player4, Stash5} = gen_hand(14, [], Stash4),
	{Extra, Stash6}   = random_peek(Stash5),
    G = #game{
    	stamp=now(),
    	active=1,
    	jocker=Jocker,
    	players= [
    		#player{hand=[Extra] ++ Player1, discard=[]},
    		#player{hand=Player2, discard=[]},
    		#player{hand=Player3, discard=[]},
			#player{hand=Player4, discard=[]}
		],
    	stash = Stash6
    },
    G.

next_player(4) -> 1;
next_player(Id) -> Id+1.

prev_player(1) -> 4;
prev_player(Id) -> Id-1.

% Take, Discard are lists of tiles [ [1,2], [3,4] ]

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

active_player(#game{active=No, players=Players}) -> lists:nth(No, Players).
right_player(#game{active=No, players=Players}) -> lists:nth(prev_player(No), Players).

next_turn(G=#game{active=No}) -> G#game{active=next_player(No)}.

update_active_player(G = #game{active=No, players=Players}, NewPlayer) ->
	G#game{ players=setnth(No, Players, NewPlayer) }.

update_active_player_hand(G = #game{active=No}, NewHand) ->
	Player = active_player(G),

take(G = #game{active=No, players=Players}, right) ->
	Player = active_player(G),
	RightPlayer = right_player(G),
	[Tile | NewDiscard] = RightPlayer#player.discard,
	NewPlayer = Player#player{ hand=[Tile] ++ Player#player.hand},
	NewRightPlayer = RightPlayer#player{ discard=NewDiscard },
	G#game{ players=setnth(prev_player(No), setnth(No, Players, NewPlayer), NewRightPlayer) };

take(G = #game{active=No, stash=[Tile|NewStash], players=Players}, table) ->
	Player = active_player(G),
	NewPlayer = Player#player{hand=[Tile] ++ Player#player.hand},
	update_active_player(G#game{stash=NewStash}, NewPlayer).

discard(G = #game{active=No, players=Players}, Tile) ->
	Player = active_player(G),
	NewHand = lists:delete(Tile, Player#player.hand),
	case Player#player.hand of
		NewHand ->
			{fail, no_tail_to_discard, No, Tile};
		_ ->
			update_active_player(G, Player#player{hand = NewHand})
	end.