%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 协议配置解析
%%%
%%% @end
%%% Created : 31. 五月 2020 17:58
%%%-------------------------------------------------------------------
-module(proto_parser).
-author("huangzaoyi").

%% API
-export([parser/1]).

-include("proto.hrl").

%% 解析协议
parser([]) ->
    ok;
parser([I | List]) ->
    parser(I),
    parser(List);
parser({Mod, Incs, Protos}) ->
    Header = get_header(Mod, Incs),
    Body = get_body(Protos),
    Data = string:join([Header, Body], "\n\n"),
    file:write_file(lists:concat([Mod, ".erl"]), Data).

%% 获取打包解包文件头部
get_header(Mod, Incs) ->
    ModStr = io_lib:format("-module(~w).\n", [Mod]),
    IncStr = lists:concat([io_lib:format("-include(\"~s\").\n", [I]) || I <- Incs]),
    ExportStr = "-export([pack/3, unpack/3]).\n",
    string:join([ModStr, IncStr, ExportStr], "\n\n").

%% 获取打包解包文件内容
get_body(Protos) ->
    get_proto(Protos).

%% 获取协议打包解包
get_proto(Protos) ->
    get_proto(Protos, [], []).
get_proto([], Packs, Unpacks) ->
    lists:reverse(Packs) ++ gen_pack_tail() ++ "\n\n" ++ lists:reverse(Unpacks);
get_proto([#proto{code = Code, req = Req, res = Res} | Protos], Packs, Unpacks) ->
    {NewReq, _} = add_idx(Req, [], 1),
    {NewRes, _} = add_idx(Res, [], 1),
    Pack = get_pack(Code, NewReq, NewRes),
    Unpack = get_unpack(Code, NewReq, NewRes),
    NewPacks = [Pack | Packs],
    NewUnpacks = [Unpack | Unpacks],
    get_proto(Protos, NewPacks, NewUnpacks).

%% 生成打包模块
get_pack(Code, Req, Res) ->
    gen_pack(Code, Req, req) ++ gen_pack(Code, Res, res) ++ "\n".

%% 生成打包内容
%% 判断打包头是列表，还是元组、字典、记录
gen_pack(Code, [Filed = #filed{array = true}], Flag) -> %% 第一个是列表
    gen_pack_header(Code, Flag, Filed) ++ gen_pack_content(Filed);
gen_pack(Code, [Filed = #filed{type = tuple}], Flag) -> %% 第一个是元组
    gen_pack_header(Code, Flag, Filed) ++ gen_pack_content(Filed);
gen_pack(Code, [Filed = #filed{type = map}], Flag) -> %% 第一个是字典
    gen_pack_header(Code, Flag, Filed) ++ gen_pack_content(Filed);
gen_pack(Code, [Filed = #filed{type = rec}], Flag) -> %% 第一个是记录
    gen_pack_header(Code, Flag, Filed) ++ gen_pack_content(Filed);
gen_pack(Code, Fileds, Flag) -> %% 其他情况，转成第一个为元组
    gen_pack(Code, [#filed{type = tuple, name = data, data = Fileds}], Flag).

%% 生成解包模块
get_unpack(Code, Req, Res) ->
    "".

%% 生成打包方法头
gen_pack_header(Code, req, Filed = #filed{}) ->
    io_lib:format("pack(~p, req, ~s) ->\n", [Code, gen_pack_data(Filed)]);
gen_pack_header(Code, res, Filed = #filed{}) ->
    io_lib:format("pack(~p, res, ~s) ->\n", [Code, gen_pack_data(Filed)]).

%% 生成打包方法内容
gen_pack_content(Filed = #filed{}) ->
    io_lib:format("    ~s;\n", [gen_pack_bin(Filed)]).

%% 生成打包方法尾
gen_pack_tail() ->
    "pack(Code, _Flag, _Data) ->\n    {error_bad_code, Code}.\n".

%% 生成打包方法数据头
gen_pack_data(#filed{array = true, name = Name, idx = Idx}) ->
    get_val(Name, Idx);
gen_pack_data(#filed{type = tuple, data = Data}) ->
    Content = string:join([gen_pack_data(D) || D <- Data], ","),
    "{" ++ Content ++ "}";
gen_pack_data(#filed{type = map, data = Data}) ->
    Content = string:join([lists:concat([N, ":=", gen_pack_data(D)]) || D = #filed{name = N} <- Data], ","),
    "#{" ++ Content ++ "}";
gen_pack_data(#filed{type = rec, rec = RecName, data = Data}) ->
    Content = string:join([lists:concat([N, "=", gen_pack_data(D)]) || D = #filed{name = N} <- Data], ","),
    "#" ++ atom_to_list(RecName) ++ "{" ++ Content ++ "}";
gen_pack_data(#filed{name = Name, idx = Idx}) ->
    get_val(Name, Idx).

%% 生成打包方法二进制
gen_pack_bin(Filed) ->
    "<<" ++ gen_pack_bin2(Filed) ++ ">>".
gen_pack_bin2(Filed = #filed{array = true, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Data = gen_pack_data(Filed#filed{array = false}),
    Content = gen_pack_bin2(Filed#filed{array = false}),
    io_lib:format("(length(~s)):16,(list_to_binary([<<~s>>||~s<-~s]))/binary", [Val, Content, Data, Val]);
gen_pack_bin2(#filed{type = tuple, data = Data}) ->
    string:join([gen_pack_bin2(D) || D <- Data], ",");
gen_pack_bin2(#filed{type = map, data = Data}) ->
    string:join([gen_pack_bin2(D) || D <- Data], ",");
gen_pack_bin2(#filed{type = rec, data = Data}) ->
    string:join([gen_pack_bin2(D) || D <- Data], ",");

gen_pack_bin2(#filed{type = int8, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":8/signed";
gen_pack_bin2(#filed{type = uint8, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":8/unsigned";
gen_pack_bin2(#filed{type = int16, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":16/signed";
gen_pack_bin2(#filed{type = uint16, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":16/unsigned";
gen_pack_bin2(#filed{type = int32, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":32/signed";
gen_pack_bin2(#filed{type = uint32, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":32/unsigned";
gen_pack_bin2(#filed{type = int64, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":64/signed";
gen_pack_bin2(#filed{type = uint64, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":64/unsigned";
gen_pack_bin2(#filed{type = string, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    io_lib:format("(byte_size(~s)):16,~s/binary", [Val, Val]).

get_val(Name, Idx) ->
    lists:concat(["Val_", Name, "_", Idx]).

%% 每个协议定义的字段添加唯一的ID
add_idx([], Acc, Idx) ->
    {lists:reverse(Acc), Idx};
add_idx([Filed = #filed{data = Data} | Fileds], Acc, Idx) ->
    {NewData, NewIdx} = add_idx(Data, [], Idx),
    NewFiled = Filed#filed{idx = NewIdx, data = NewData},
    add_idx(Fileds, [NewFiled | Acc], NewIdx + 1).