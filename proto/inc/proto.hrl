%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 协议头文件
%%%
%%% @end
%%% Created : 31. 五月 2020 00:01
%%%-------------------------------------------------------------------
-author("huangzaoyi").

%% 协议定义结构
-record(proto, {
    code = 0            :: integer()       %% 协议号
    ,req_desc = []      :: string()        %% 协议请求定义描述
    ,req = []           :: list()          %% 协议请求定义
    ,res = []           :: list()          %% 协议响应定义
    ,res_desc = []      :: string()        %% 协议响应定义描述
}).

%% 协议字段项结构
-record(filed, {
    type             :: atom()             %% 类型   int8,uint8,int16,uint16,int32,uint32,int64,uint64,bool,string,tuple,rec,map
    ,name            :: atom()             %% 名字
    ,desc = ""       :: string()           %% 描述
    ,rec             :: atom()             %% 记录名
    ,data = []       :: list()             %% 数据
    ,array = false   :: boolean()          %% 是否为列表
    ,idx = 0         :: pos_integer()      %% 字段的唯一id
}).

-define(normal_filed_types, [int8, uint8, int16, uint16, int32, uint32, int64, uint64, bool, string]).
-define(other_filed_types, [tuple, rec, map]).
