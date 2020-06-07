%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 协议配置文件读取
%%%
%%% @end
%%% Created : 31. 五月 2020 13:23
%%%-------------------------------------------------------------------
-module(proto_reader).
-author("huangzaoyi").

-define(proto_path, "proto/").

%% API
-export([
    get_cfgs/0
]).

%% 获取协议配置
-spec get_cfgs() -> [{atom(), list(), list()}].
get_cfgs() ->
    Mods = get_proto_mods(),
    get_proto_cfg(Mods).

%% 获取协议定义文件的模块名
-spec get_proto_mods() -> [file:filename_all()].
get_proto_mods() ->
    FileNames = filelib:wildcard("*.erl", ?proto_path),
    [list_to_atom(filename:basename(FileName, ".erl")) || FileName <- FileNames].

%% 获取协议配置文件内容
-spec get_proto_cfg([atom()]) -> [{atom(), list(), list()}].
get_proto_cfg(Mods) when is_list(Mods) ->
    [get_proto_cfg(Mod) || Mod <- Mods];
get_proto_cfg(Mod) ->
    {Mod, Mod:inc(), Mod:proto()}.