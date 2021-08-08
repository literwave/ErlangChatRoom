%%%-------------------------------------------------------------------
%%% @author Curry
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 8月 2021 13:33
%%%-------------------------------------------------------------------
-module(chatserv).
-author("Curry").

%% API
-compile(export_all).
-import(ets, [insert_new/2]).

start_server() ->
	ets:new(id, [ordered_set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
	case gen_tcp:listen(1234, [binary, {packet, 0}, {active, true}]) of
		{ok, ListenSocket} ->
			spawn(fun() -> client_connect(ListenSocket) end);
		{error, Reason} ->
			io:format("~p~n", [Reason])
	end.


client_connect(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			%% 进行验证，看是否是注册还是登录
			spawn(fun() -> client_connect(ListenSocket) end),
			loop(Socket);
		{error, Reason} ->
			io:format("~p~n", [Reason])
	end.

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			[Id, Sign, PassWord, SendId, MessageInfos] = binary_to_term(Bin),
			if
				Sign =:= register_user ->
					Info = register_user(Id, PassWord, Socket),
					gen_tcp:send(Socket, term_to_binary(Info)),
					loop(Socket);
				Sign =:= login_user ->
					Info = login_user(Id, PassWord, Socket),
					gen_tcp:send(Socket, term_to_binary(Info)),
					loop(Socket);
				Sign =:= login_out ->
					Info = login_out(Id, Socket),
					gen_tcp:send(Socket, term_to_binary(Info)),
					loop(Socket);
				Sign =:= private_msg ->
					private_chat(SendId, Socket, MessageInfos),
					loop(Socket);
				Sign =:= group_msg ->
					group_chat(Socket, MessageInfos),
					loop(Socket);
				true ->
					io:format("error sign ~n"),
					loop(Socket)
			end;
		{tcp_closed, Socket} ->
			io:format("Server socket closed ~n")
	end.

%% 用户注册
register_user(Id, PassWord, Socket) ->
	case ets:lookup(id, Id) of
		[_Ok] ->
			io:format("Account is fail ~n"),
			"Account is exist ~n";
		_ ->
			ets:insert(id, {Id, PassWord, 0, Socket}),
			"register successed ~n"
	end.

%% 用户登录
login_user(Id, PassWord, Socket) ->
	case ets:match_object(id, {Id, PassWord, 0, Socket}) of
		[_Ok] ->
			ets:update_element(id, Id, [{3, 1}, {4, Socket}]),
			"login successed";
		Reson ->
			io:format("login is fail ~n ~p", [Reson]),
			"Password error or Account is not exist ~n"
	end.

%% 退出用户
login_out(Id, Socket) ->
	%% 因为id对应唯一socket，所以不需要PassWord
	case ets:match_object(id, {Id, '_', 1, Socket}) of
		[_Ok] ->
			ets:update_element(id, Id, [{3, 0}, {4, 0}]),
			"login successed";
		_ ->
			io:format("out is fail ~n"),
			"login is fail"
	end.

%% 群聊
group_chat(Socket, MessageInfos) ->
	case ets:match_object(id, {'_', '_', 1, Socket}) of
		[{Id, _, _, _}] ->
			Res = ets:match_object(id, {'_', '_', 1, '_'}),
			case Res =:= [] of
				true ->
					io:format("no person online ~p ~n", [Res]);
				_ ->
					group_send_msg(Res, Id, MessageInfos)
			end;
		_ ->
			io:format("group chat is fail ~n")
	end.


%% 群聊发送
group_send_msg([], _Id, _MessageInfos) ->
	next;
group_send_msg([Info | Infos], Id, MessageInfos) ->
	{_, _, _, Socket} = Info,
	gen_tcp:send(Socket, term_to_binary("from: " ++ integer_to_list(Id) ++ "say: " ++ MessageInfos)),
	group_send_msg(Infos, Id, MessageInfos).

%% 在线私聊
private_chat(SendId, Socket, MessageInfos) ->
	case ets:match_object(id, {'_', '_', 1, Socket}) of
		[{Id, _, _, _}] ->
			Res = ets:match_object(id, {SendId, '_', 1, '_'}),
			case Res =:= [] of
				true ->
					io:format("send person not online ~p ~n", [Res]);
				_ ->
					private_send_msg(Res, Id, MessageInfos)
			end;
		_ ->
			io:format("private chat is fail ~n")
	end.

%% 私聊发送
private_send_msg([Info], Id, MessageInfos) ->
	{_, _, _, Socket} = Info,
	gen_tcp:send(Socket, term_to_binary("from: " ++ integer_to_list(Id) ++ "say: " ++ MessageInfos)).








