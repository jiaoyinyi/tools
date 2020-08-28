%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 五月 2020 18:01
%%%-------------------------------------------------------------------
-module(proto).
-author("huangzaoyi").


%% API
-export([
    gen_proto/2
]).

%% 执行这个方法，读取协议定义文件，验证协议定义，生成协议打包解包文件
-spec gen_proto(string(), string()) -> ok.
gen_proto(CfgPath, OutPath) ->
    ProtoCfgs = proto_reader:get_cfgs(CfgPath),
    proto_cond:valid(ProtoCfgs),
    NewOutPath = unicode:characters_to_binary(io_lib:format("~ts",[OutPath])),
    proto_parser:parse(ProtoCfgs, NewOutPath),
    ok.

