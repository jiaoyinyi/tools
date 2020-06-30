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
-export([parse/2]).

-include("proto.hrl").

%% 解析协议
parse([], _OutPath) ->
    ok;
parse([I | List], OutPath) ->
    parse(I, OutPath),
    parse(List, OutPath);
parse({Mod, Incs, Protos}, OutPath) ->
    Header = get_header(Mod, Incs),
    Body = get_body(Protos),
    Data = string:join([Header, Body], "\n\n"),
    FileName = filename:join([OutPath, lists:concat([Mod, ".erl"])]),
    ok = file:write_file(FileName, Data),
    Str = unicode:characters_to_binary(io_lib:format("生成解析文件~ts~n", [FileName])),
    io:format(Str),
    ok.

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
    lists:reverse(Packs) ++ gen_pack_tail() ++ "\n\n" ++ lists:reverse(Unpacks) ++ gen_unpack_tail();
get_proto([#proto{code = Code, req = Req, res = Res} | Protos], Packs, Unpacks) ->
    NewReq = deal_filed(Req),
    NewRes = deal_filed(Res),
    Pack = gen_pack(Code, NewReq, NewRes),
    Unpack = get_unpack(Code, NewReq, NewRes),
    NewPacks = [Pack | Packs],
    NewUnpacks = [Unpack | Unpacks],
    get_proto(Protos, NewPacks, NewUnpacks).

%% 如果第一层不是array、tuple、map、rec，则在最外层加一层tuple；并给每个字段添加索引
deal_filed(Fileds = [#filed{array = Array, type = Type}])
    when Array == true orelse Type == tuple orelse Type == map orelse Type == rec ->
    {NewFileds, _} = add_idx(Fileds, [], 1),
    NewFileds;
deal_filed(Fileds) ->
    {NewFileds, _} = add_idx([#filed{type = tuple, name = data, data = Fileds}], [], 1),
    NewFileds.

%% 生成打包
gen_pack(Code, Req, Res) ->
    gen_pack_func(Code, Req, req) ++ gen_pack_func(Code, Res, res) ++ "\n".

%% 生成打包方法
gen_pack_func(Code, [Filed = #filed{}], Flag) ->
    gen_pack_header(Code, Flag, Filed) ++ gen_pack_content(Filed).

%% 生成打包方法头
gen_pack_header(Code, Flag, Filed = #filed{}) ->
    io_lib:format("pack(~p, ~w, ~s) ->\n", [Code, Flag, get_pack_data_name(Filed)]).

%% 生成打包方法内容
gen_pack_content(Filed = #filed{}) ->
    io_lib:format("    ~s;\n", [gen_pack_bin(Filed)]).

%% 生成打包尾
gen_pack_tail() ->
    "pack(Code, _Flag, _Data) ->\n    {error_bad_code, Code}.\n".

%% 获取打包数据变量名
get_pack_data_name(#filed{array = true, name = Name, idx = Idx}) ->
    get_val(Name, Idx);
get_pack_data_name(#filed{type = tuple, data = Data}) ->
    Content = string:join([get_pack_data_name(D) || D <- Data], ","),
    "{" ++ Content ++ "}";
get_pack_data_name(#filed{type = map, data = Data}) ->
    Content = string:join([lists:concat([N, ":=", get_pack_data_name(D)]) || D = #filed{name = N} <- Data], ","),
    "#{" ++ Content ++ "}";
get_pack_data_name(#filed{type = rec, rec = RecName, data = Data}) ->
    Content = string:join([lists:concat([N, "=", get_pack_data_name(D)]) || D = #filed{name = N} <- Data], ","),
    "#" ++ atom_to_list(RecName) ++ "{" ++ Content ++ "}";
get_pack_data_name(#filed{name = Name, idx = Idx}) ->
    get_val(Name, Idx).

%% 生成打包方法二进制
gen_pack_bin(Filed) ->
    "<<" ++ gen_pack_bin2(Filed) ++ ">>".
gen_pack_bin2(Filed = #filed{array = true, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    NewName = list_to_atom(lists:concat(["_", Name])),
    Data = get_pack_data_name(Filed#filed{array = false, name = NewName}),
    Content = gen_pack_bin2(Filed#filed{array = false, name = NewName}),
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
    Val ++ ":8";
gen_pack_bin2(#filed{type = int16, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":16/signed";
gen_pack_bin2(#filed{type = uint16, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":16";
gen_pack_bin2(#filed{type = int32, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":32/signed";
gen_pack_bin2(#filed{type = uint32, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":32";
gen_pack_bin2(#filed{type = int64, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":64/signed";
gen_pack_bin2(#filed{type = uint64, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    Val ++ ":64";
gen_pack_bin2(#filed{type = string, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    io_lib:format("(proto_core:pack_string(~s))/binary", [Val]);
gen_pack_bin2(#filed{type = bool, name = Name, idx = Idx}) ->
    Val = get_val(Name, Idx),
    io_lib:format("(proto_core:pack_bool(~s))/binary", [Val]).

%% 生成解包
get_unpack(Code, Req, Res) ->
    gen_unpack_func(Code, Req, req) ++ gen_unpack_func(Code, Res, res) ++ "\n".

%% 生成解包方法
gen_unpack_func(Code, [Filed = #filed{}], Flag) ->
    gen_unpack_header(Code, Flag) ++ gen_unpack_content(Filed).

%% 生成解包方法头
gen_unpack_header(Code, Flag) ->
    io_lib:format("unpack(~p, ~w, Bin) ->\n", [Code, Flag]).

%% 生成解包方法内容
gen_unpack_content(Filed = #filed{}) ->
    gen_unpack_data(Filed).

%% 获取解包数据变量名
get_unpack_data_name(#filed{array = true, name = Name, idx = Idx}) ->
    get_val(Name, Idx);
get_unpack_data_name(#filed{type = tuple, data = Data}) ->
    Content = string:join([get_unpack_data_name(D) || D <- Data], ","),
    "{" ++ Content ++ "}";
get_unpack_data_name(#filed{type = map, data = Data}) ->
    Content = string:join([lists:concat([N, "=>", get_unpack_data_name(D)]) || D = #filed{name = N} <- Data], ","),
    "#{" ++ Content ++ "}";
get_unpack_data_name(#filed{type = rec, rec = RecName, data = Data}) ->
    Content = string:join([lists:concat([N, "=", get_unpack_data_name(D)]) || D = #filed{name = N} <- Data], ","),
    "#" ++ atom_to_list(RecName) ++ "{" ++ Content ++ "}";
get_unpack_data_name(#filed{name = Name, idx = Idx}) ->
    get_val(Name, Idx).

%% 生成解包数据方法
gen_unpack_data(Filed) ->
    {Str, PreBinIdx} = gen_unpack_data2(Filed, 0, 1),
    TailStr = gen_unpack_data_tail(Filed, PreBinIdx),
    Str ++ TailStr.

%% 生成解包数据尾
gen_unpack_data_tail(Filed, PreBinIdx) ->
    Space = get_space(1),
    DataName = get_unpack_data_name(Filed),
    Bin = get_bin(PreBinIdx),
    io_lib:format("~s<<>> = ~s,\n~s~s;\n", [Space, Bin, Space, DataName]).

%% 第几层 层数*4 第几层，则空格数为层数*4个
%% {Val3, Bin4} =
%%     proto_core:unpack_array(Bin,
%%         fun(Bin1) ->
%%             {Val1, Bin2} = proto_core:unpack_uint8(Bin1),
%%             {Val2, Bin3} = proto_core:unpack_uint8(Bin2),
%%             {{Val1, Val2}, Bin3}
%%         end)
gen_unpack_data2(Filed = #filed{array = true, type = Type, name = Name, idx = Idx, data = Data}, PreBinIdx, Layer) when Type == tuple orelse Type == map orelse Type == rec ->
    %% 获取变量名
    Val = get_val(Name, Idx),
    %% 获取上一个bin变量名
    PreBin = get_bin(PreBinIdx),
    %% 获取第一个子变量的bin变量名
    FirstSubPreBinIdx = PreBinIdx + 1,
    FirstSubPreBin = get_bin(FirstSubPreBinIdx),
    %% 处理生成解包子字段方法
    Fun =
        fun(SubFiled, {AccStr, AccBinIdx}) ->
            {SubStr, NewAccBinIdx} = gen_unpack_data2(SubFiled, AccBinIdx, Layer + 1),
            {AccStr ++ SubStr, NewAccBinIdx}
        end,
    {NewSubStr, NewBinIdx} = lists:foldl(Fun, {"", FirstSubPreBinIdx}, Data),
    %% 获取最后一个子字段的bin变量名
    LastSubPreBin = get_bin(NewBinIdx),
    %% 获取该列表变量的bin变量名
    NewBin = get_bin(NewBinIdx + 1),
    %% 获取解包数据名字
    DataName = get_unpack_data_name(Filed#filed{array = false}),
    %% 获取空格字符串
    Space = get_space(Layer),
    %% 获取数据名的空格字符串
    DataNameSpace = get_space(Layer + 1),
    Str = io_lib:format("~s{~s,~s} = proto_core:unpack_array(~s,\n~sfun(~s) ->\n~s~s{~s,~s}\n~send),\n", [Space, Val, NewBin, PreBin, Space, FirstSubPreBin, NewSubStr, DataNameSpace, DataName, LastSubPreBin, Space]),
    {Str, NewBinIdx + 1};

%% {Val1, Bin2} = proto_core:unpack_array(Bin, fun proto_core:unpack_int8/1),
gen_unpack_data2(#filed{array = true, type = Type, name = Name, idx = Idx}, PreBinIdx, Layer) ->
    %% 获取变量名
    Val = get_val(Name, Idx),
    %% 获取上一个bin变量名
    PreBin = get_bin(PreBinIdx),
    NewBinIdx = PreBinIdx + 1,
    NewBin = get_bin(NewBinIdx),
    FuncStr = get_unpack_func_str(Type),
    Space = get_space(Layer),
    Str = io_lib:format("~s{~s,~s} = proto_core:unpack_array(~s,fun ~s/1),\n", [Space, Val, NewBin, PreBin, FuncStr]),
    {Str, NewBinIdx};

gen_unpack_data2(#filed{type = Type, data = Data}, PreBinIdx, Layer) when Type == tuple orelse Type == map orelse Type == rec ->
    Fun =
        fun(SubFiled, {AccStr, AccBinIdx}) ->
            {SubStr, NewAccBinIdx} = gen_unpack_data2(SubFiled, AccBinIdx, Layer),
            {AccStr ++ SubStr, NewAccBinIdx}
        end,
    {NewSubStr, NewBinIdx} = lists:foldl(Fun, {"", PreBinIdx}, Data),
    {NewSubStr, NewBinIdx};

gen_unpack_data2(#filed{type = Type, name = Name, idx = Idx}, PreBinIdx, Layer) ->
    Val = get_val(Name, Idx),
    PreBin = get_bin(PreBinIdx),
    BinIdx = PreBinIdx + 1,
    Bin = get_bin(BinIdx),
    FuncStr = get_unpack_func_str(Type),
    Space = get_space(Layer),
    Str = io_lib:format("~s{~s,~s} = ~s(~s),\n", [Space, Val, Bin, FuncStr, PreBin]),
    {Str, BinIdx}.

%% 生成解包方法尾
gen_unpack_tail() ->
    "unpack(Code, _Flag, _Bin) ->\n    {error_bad_code, Code}.\n".

%% 获取变量名
get_val(Name, Idx) ->
    lists:concat(["Val_", Name, "_", Idx]).

%% 获取Bin变量名
get_bin(0) ->
    "Bin";
get_bin(Idx) ->
    lists:concat(["Bin_", Idx]).

%% 获取解包方法字符串
get_unpack_func_str(int8) ->
    "proto_core:unpack_int8";
get_unpack_func_str(uint8) ->
    "proto_core:unpack_uint8";
get_unpack_func_str(int16) ->
    "proto_core:unpack_int16";
get_unpack_func_str(uint16) ->
    "proto_core:unpack_uint16";
get_unpack_func_str(int32) ->
    "proto_core:unpack_int32";
get_unpack_func_str(uint32) ->
    "proto_core:unpack_uint32";
get_unpack_func_str(int64) ->
    "proto_core:unpack_int64";
get_unpack_func_str(uint64) ->
    "proto_core:unpack_uint64";
get_unpack_func_str(string) ->
    "proto_core:unpack_string";
get_unpack_func_str(bool) ->
    "proto_core:unpack_bool".

%% 每个协议定义的字段添加唯一的ID
add_idx([], Acc, Idx) ->
    {lists:reverse(Acc), Idx};
add_idx([Filed = #filed{data = Data} | Fileds], Acc, Idx) ->
    {NewData, NewIdx} = add_idx(Data, [], Idx),
    NewFiled = Filed#filed{idx = NewIdx, data = NewData},
    add_idx(Fileds, [NewFiled | Acc], NewIdx + 1).

%% 获取空格
get_space(Layer) ->
    [32 || _ <- lists:seq(1, Layer * 4)].