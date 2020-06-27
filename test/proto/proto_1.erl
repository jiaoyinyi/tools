-module(proto_1).


-include("head.hrl").


-export([pack/3, unpack/3]).


pack(10001, req, {Val_id_1,Val_infos_4}) ->
    <<Val_id_1:8,(length(Val_infos_4)):16,(list_to_binary([<<(proto_core:pack_string(Val_name_2))/binary,Val_sex_3:8>>||{Val_name_2,Val_sex_3}<-Val_infos_4]))/binary>>;
pack(10001, res, {}) ->
    <<>>;

pack(10002, req, {Val_id_1,Val_infos_2,Val_infos2_5,Val_infos3_8}) ->
    <<Val_id_1:8,(length(Val_infos_2)):16,(list_to_binary([<<Val__infos_2:32/signed>>||Val__infos_2<-Val_infos_2]))/binary,(length(Val_infos2_5)):16,(list_to_binary([<<(proto_core:pack_string(Val_name_3))/binary,Val_sex_4:8>>||#{name:=Val_name_3,sex:=Val_sex_4}<-Val_infos2_5]))/binary,(length(Val_infos3_8)):16,(list_to_binary([<<(proto_core:pack_string(Val_name_6))/binary,Val_sex_7:8>>||#{name:=Val_name_6,sex:=Val_sex_7}<-Val_infos3_8]))/binary>>;
pack(10002, res, {}) ->
    <<>>;

pack(Code, _Flag, _Data) ->
    {error_bad_code, Code}.


unpack(10001, req, Bin) ->
    {Val_id_1,Bin_1} = proto_core:unpack_uint8(Bin),
    {Val_infos_4,Bin_5} = proto_core:unpack_array(Bin_1,
    fun(Bin_2) ->
        {Val_name_2,Bin_3} = proto_core:unpack_string(Bin_2),
        {Val_sex_3,Bin_4} = proto_core:unpack_uint8(Bin_3),
        {{Val_name_2,Val_sex_3},Bin_4}
    end),
    {{Val_id_1,Val_infos_4},Bin_5};
unpack(10001, res, Bin) ->
    {{},Bin};

unpack(10002, req, Bin) ->
    {Val_id_1,Bin_1} = proto_core:unpack_uint8(Bin),
    {Val_infos_2,Bin_2} = proto_core:unpack_array(Bin_1,fun proto_core:unpack_int32/1),
    {Val_infos2_5,Bin_6} = proto_core:unpack_array(Bin_2,
    fun(Bin_3) ->
        {Val_name_3,Bin_4} = proto_core:unpack_string(Bin_3),
        {Val_sex_4,Bin_5} = proto_core:unpack_uint8(Bin_4),
        {#{name=>Val_name_3,sex=>Val_sex_4},Bin_5}
    end),
    {Val_infos3_8,Bin_10} = proto_core:unpack_array(Bin_6,
    fun(Bin_7) ->
        {Val_name_6,Bin_8} = proto_core:unpack_string(Bin_7),
        {Val_sex_7,Bin_9} = proto_core:unpack_uint8(Bin_8),
        {#{name=>Val_name_6,sex=>Val_sex_7},Bin_9}
    end),
    {{Val_id_1,Val_infos_2,Val_infos2_5,Val_infos3_8},Bin_10};
unpack(10002, res, Bin) ->
    {{},Bin};

unpack(Code, _Flag, _Bin) ->
    {error_bad_code, Code}.
