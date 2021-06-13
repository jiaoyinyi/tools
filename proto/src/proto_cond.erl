%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 协议字段判断
%%%
%%% @end
%%% Created : 07. 六月 2020 18:19
%%%-------------------------------------------------------------------
-module(proto_cond).
-author("huangzaoyi").

%% API
-export([valid/1]).

-include("proto.hrl").

%% 检验每个协议定义文件的协议定义是否正确
valid([]) ->
    true;
valid([ProtoCfg | ProtoCfgs]) ->
    valid(ProtoCfg),
    valid(ProtoCfgs);
valid({Mod, Incs, Protos}) when is_atom(Mod) ->
    valid_inc(Incs),
    valid_proto(Protos).

%% 验证头文件 判断所有的引用的头文件是否为.hrl结尾
valid_inc(Incs) when is_list(Incs) ->
    Fun = fun(Inc) -> lists:suffix(".hrl", Inc) end,
    case lists:all(Fun, Incs) of
        true ->
            true;
        _ ->
            exit({error_incs, Incs})
    end.

%% 验证协议定义
%% 判断是否有相同的协议号
valid_proto(Protos) ->
    valid_proto(Protos, []).
valid_proto([], _ProtoIds) ->
    true;
valid_proto([#proto{code = Code, req = Req, req_desc = ReqDesc, res = Res, res_desc = ResDesc} | Protos], ProtoIds) when is_integer(Code) andalso Code > 0 ->
    case lists:member(Code, ProtoIds) of
        false ->
            valid_desc(ReqDesc),
            valid_desc(ResDesc),
            valid_filed(Req),
            valid_filed(Res),
            valid_proto(Protos, [Code | ProtoIds]);
        _ ->
            exit({erorr_same_code, Code})
    end.

%% 验证描述是否为可打印字符串
valid_desc(Desc) ->
    case io_lib:printable_unicode_list(Desc) of
        true ->
            true;
        _ ->
            exit({error_bad_desc, Desc})
    end.

%% 验证字段
%% 判断相同层有相同的字段名
%% 判断tuple、map、rec类型的data是否为空，array、tuple、map、rec字段不能有默认值
%% 判断类型是否正确
valid_filed([]) ->
    true;
valid_filed(Fileds) when is_list(Fileds) ->
    valid_filed(Fileds, []).
valid_filed([], _FiledNames) ->
    true;
valid_filed([Filed = #filed{name = Name, type = Type, data = Data, default = Default} | Fileds], FiledNames) when Type == tuple orelse Type == map orelse Type == rec ->
    case lists:member(Name, FiledNames) of
        false ->
            case Default =:= undefined of
                true ->
                    case length(Data) > 0 of
                        true ->
                            valid_filed(Data),
                            valid_filed(Fileds, [Name | FiledNames]);
                        _ ->
                            exit({error_data_empty, Filed})
                    end;
                _ ->
                    exit({error_have_default, Filed})
            end;
        _ ->
            exit({error_same_name, Name})
    end;
valid_filed([Filed = #filed{name = Name, type = Type, data = []} | Fileds], FiledNames) ->
    case lists:member(Name, FiledNames) of
        false ->
            case lists:member(Type, ?normal_filed_types) of
                true ->
                    valid_filed(Fileds, [Name | FiledNames]);
                _ ->
                    exit({error_type, Filed})
            end;
        _ ->
            exit({error_same_name, Name})
    end.