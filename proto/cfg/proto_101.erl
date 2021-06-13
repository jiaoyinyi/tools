-module(proto_101).

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
            code = 10100
            , req_desc = "获取服务器角色信息"
            , req = [
                #filed{type = map, name = test_map, desc = "测试map", data = [
                    #filed{type = uint32, name = id, desc = "角色ID", default = 0}
                    , #filed{type = string, name = name, desc = "角色名", default = <<>>}
                    , #filed{type = map, name = test_map2, data = [
                        #filed{type = uint32, name = id, desc = "角色ID", default = 0}
                        , #filed{type = string, name = name, desc = "角色名"}
                    ]}
                ]}
            ]
            , res_desc = "获取服务器角色信息"
            , res = []
        }
    ].
