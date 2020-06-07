-module(proto_1).


-include("head.hrl").


-export([pack/3, unpack/3]).


pack(10001, req, {Val_id_1,Val_infos_4}) ->
    <<Val_id_1:8/unsigned,(length(Val_infos_4)):16,(list_to_binary([<<(byte_size(Val_name_2)):16,Val_name_2/binary,Val_sex_3:8/unsigned>>||{Val_name_2,Val_sex_3}<-Val_infos_4]))/binary>>;
pack(10001, res, {}) ->
    <<>>;

pack(10002, req, {Val_id_1,Val_infos_2,Val_infos2_5,Val_infos3_8}) ->
    <<Val_id_1:8/unsigned,(length(Val_infos_2)):16,(list_to_binary([<<Val_infos_2:32/signed>>||Val_infos_2<-Val_infos_2]))/binary,(length(Val_infos2_5)):16,(list_to_binary([<<(byte_size(Val_name_3)):16,Val_name_3/binary,Val_sex_4:8/unsigned>>||#{name:=Val_name_3,sex:=Val_sex_4}<-Val_infos2_5]))/binary,(length(Val_infos3_8)):16,(list_to_binary([<<(byte_size(Val_name_6)):16,Val_name_6/binary,Val_sex_7:8/unsigned>>||#{name:=Val_name_6,sex:=Val_sex_7}<-Val_infos3_8]))/binary>>;
pack(10002, res, {}) ->
    <<>>;

pack(Code, _Flag, _Data) ->
    {error_bad_code, Code}.


