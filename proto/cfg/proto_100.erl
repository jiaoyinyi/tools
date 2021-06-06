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
            , req_desc = "获取服务器角色信息"
            , req = [
                #filed{type = string, name = srv_id, desc = "服务器ID"}
                , #filed{type = string, name = account, desc = "账号"}
                , #filed{type = uint32, name = timestamp, desc = "时间戳"}
                , #filed{type = string, name = sign, desc = "标识"}
            ]
            , res_desc = "获取服务器角色信息"
            , res = [
                #filed{type = tuple, name = role_info, desc = "角色信息", array = true, data = [
                    #filed{type = uint32, name = id, desc = "角色ID"}
                    , #filed{type = string, name = srv_id, desc = "服务器ID"}
                    , #filed{type = string, name = name, desc = "角色名"}
                    , #filed{type = uint16, name = lev, desc = "等级"}
                    , #filed{type = uint8, name = sex, desc = "性别"}
                ]}
            ]
        },
        #proto{
            code = 10001
            , req_desc = "注册角色"
            , req = [
                #filed{type = string, name = srv_id, desc = "服务器ID"}
                , #filed{type = string, name = account, desc = "账号"}
                , #filed{type = uint32, name = timestamp, desc = "时间戳"}
                , #filed{type = string, name = sign, desc = "标识"}
                %% 其他注册角色信息未处理
            ]
            , res_desc = "注册角色"
            , res = [
                #filed{type = bool, name = code, desc = "是否成功"}
                #filed{type = uint32, name = id, desc = "角色ID"}
                #filed{type = string, name = srv_id, desc = "服务器ID"}
            ]
        },
        #proto{
            code = 10002
            , req_desc = "登录角色"
            , req = [
                #filed{type = string, name = srv_id, desc = "服务器ID"}
                , #filed{type = string, name = account, desc = "账号"}
                , #filed{type = uint32, name = timestamp, desc = "时间戳"}
                , #filed{type = uint32, name = id, desc = "角色ID"}
                %% 其他登录角色信息未处理
            ]
            , res_desc = "登录角色"
            , res = [
                #filed{type = bool, name = code, desc = "是否成功"}
                #filed{type = uint32, name = id, desc = "角色ID"}
                #filed{type = string, name = srv_id, desc = "服务器ID"}
            ]
        },
        #proto{
            code = 10003
            , res_desc = "角色下线"
            , res = [
                #filed{type = uint8, name = code, desc = "错误码"}
                , #filed{type = string, name = args, desc = "参数", array = true}
                , #filed{type = string, name = desc, desc = "描述"}
            ]
        },
        #proto{
            code = 10004
            , req_desc = "请求服务器时间信息"
            , req = []
            , res_desc = "服务器时间信息"
            , res = [
                #filed{type = uint32, name = timestamp, desc = "时间戳"}
                , #filed{type = int8, name = time_zone, desc = "时区"}
            ]
        }
    ].
