-module(game).
-include_lib("game/include/game.hrl").
-include_lib("cmon/include/logger.hrl").
-export([gen_game/0]).

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
    #game{
    	stamp=now(),
    	jocker=Jocker,
    	player1=#player{hand=Player1, discard=[]},
    	player2=#player{hand=Player2, discard=[]},
    	player3=#player{hand=Player3, discard=[]},
    	player4=#player{hand=Player4, discard=[]},
    	stash = Stash5
    }.