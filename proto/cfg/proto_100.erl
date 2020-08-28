
-module(proto_100).

-include("proto.hrl").

%% API
-export([inc/0, proto/0]).

%% @doc 引用头文件列表
-spec inc() -> list().
inc() ->
    [].

%% @doc 定义协议
-spec proto() -> [#proto{}|_].
proto() ->
    [
        #proto{
            code = 10000
            ,res_desc = "测试相应"
            ,res = [
                #filed{type = uint32, name = time, desc = "时间戳"}
            ]
        }
    ].
