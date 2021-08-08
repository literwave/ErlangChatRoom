%%%-------------------------------------------------------------------
%%% @author Curry
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 8月 2021 14:03
%%%-------------------------------------------------------------------
-module(chatclient).
-author("Curry").

%% API
-compile(export_all).

%客户端
start_client() ->
	{ok, Socket} = gen_tcp:connect("localhost", 1234, [binary, {packet, 0}]),  %连接服务器
	%新建一个进程负责接收消息
	Pid = spawn(fun() -> loop() end),
	gen_tcp:controlling_process(Socket, Pid),
	sendMsg(Socket).

loop() ->
	receive
		{tcp, _Socket, Bin} ->
			Res = binary_to_term(Bin),
			io:format("Message Info! ~p ~n", [Res]),
			loop();
		{tcp_closed, _Socket} ->
			io:format("Socket is closed! ~p ~n")
	end.

sendMsg(Socket) ->
	S = io:get_line("select operation: "),
	{Sign, _Info} = string:to_integer(S),
	SendMsg = operation_message(Sign),
	gen_tcp:send(Socket, term_to_binary(SendMsg)),
	sendMsg(Socket).

%% 用户注册
operation_message(1) ->
	I = io:get_line("id: "),
	{Id, _Info} = string:to_integer(I),
	Password = io:get_line("register password: "),
	[Id, register_user, Password, 0, 0];
%% 用户登录
operation_message(2) ->
	I = io:get_line("id:"),
	Password = io:get_line("login password: "),
	{Id, _Info} = string:to_integer(I),
	[Id, login_user, Password, 0, 0];
%% 用户退出
operation_message(3) ->
	I = io:get_line("id: "),
	{Id, _Info} = string:to_integer(I),
	[Id, login_out, 0, 0, 0];
%% 私聊
operation_message(4) ->
	Sd = io:get_line("send_id: "),
	Msg = io:get_line("MsgInfo: "),
	{SendId, _Info} = string:to_integer(Sd),
	[0, private_msg, 0, SendId, Msg];
%% 群聊
operation_message(5) ->
	Msg = io:get_line("MsgInfo: "),
	[0, group_msg, 0, 0, Msg];
%% 无效操作
operation_message(_) ->
	Msg = io:format("invalid_operation ~n"),
	[0, invalid_operation, 0, 0, Msg].