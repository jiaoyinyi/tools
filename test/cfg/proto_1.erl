%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 五月 2020 00:11
%%%-------------------------------------------------------------------
-module(proto_1).
-author("huangzaoyi").

-include("proto.hrl").

%% API
-export([inc/0, proto/0]).

inc() ->
    ["head.hrl"].

proto() ->
    [
        #proto{
            code = 10001
            , req_desc = "这是协议请求描述"
            , req = [
                #filed{type = uint8, name = id, desc = "ID"}
                , #filed{type = tuple, name = infos, desc = "信息", array = true, data = [
                    #filed{type = string, name = name, desc = "姓名"}
                    , #filed{type = uint8, name = sex, desc = "性别"}
                ]}
            ]
        },
        #proto{
            code = 10002
            , req_desc = "这是协议请求描述"
            , req = [
                #filed{type = uint8, name = id, desc = "ID"}
                , #filed{type = int32, name = infos, desc = "整型值", array = true}
                , #filed{type = map, name = infos2, desc = "map列表", array = true, data = [
                    #filed{type = string, name = name, desc = "姓名"}
                    , #filed{type = uint8, name = sex, desc = "性别"}
                ]}
                , #filed{type = map, name = infos3, desc = "map列表", array = true, data = [
                    #filed{type = string, name = name, desc = "姓名"}
                    , #filed{type = uint8, name = sex, desc = "性别"}
                ]}
            ]
        }
    ]
.
