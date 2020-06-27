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
    main/1
]).

%% 执行这个方法，读取协议定义文件，验证协议定义，生成协议打包解包文件
-spec main(list()) -> ok.
main([CfgPath, OutPath]) ->
    NewCfgPath = atom_to_list(CfgPath),
    NewOutPath = atom_to_list(OutPath),
    ProtoCfgs = proto_reader:get_cfgs(NewCfgPath),
    proto_cond:valid(ProtoCfgs),
    proto_parser:parse(ProtoCfgs, NewOutPath).

