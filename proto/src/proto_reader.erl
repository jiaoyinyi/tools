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

%% API
-export([
    get_cfgs/1
]).

%% @doc 获取协议配置
-spec get_cfgs(string()) -> [{atom(), list(), list()}].
get_cfgs(CfgPath) ->
    Mods = get_proto_mods(CfgPath),
    get_proto_cfg(Mods).

%% 获取协议定义文件的模块名
-spec get_proto_mods(string()) -> [file:filename_all()].
get_proto_mods(CfgPath) ->
    FileNames = filelib:wildcard("*.erl", CfgPath),
    [list_to_atom(filename:basename(FileName, ".erl")) || FileName <- FileNames].

%% 获取协议配置文件内容
-spec get_proto_cfg([atom()]) -> [{atom(), list(), list()}].
get_proto_cfg(Mods) when is_list(Mods) ->
    [get_proto_cfg(Mod) || Mod <- Mods];
get_proto_cfg(Mod) ->
    {Mod, Mod:inc(), Mod:proto()}.