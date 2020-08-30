#!/bin/bash

# ebin路径
EBIN=ebin/
# 头文件路径
INC=inc/
# 协议配置文件路径
CFG_PATH=./cfg/
OUT_PATH=./out/
PRO_PATH=xxx/

# 编译源文件
function make() {
    echo "编译erl源文件"
    erl -make
    echo "编译erl源文件完成"
}

# 编译源文件并且生协议解析文件
function gen_proto() {
    make
    # 执行生成解析协议文件脚本
    erl -noshell -pa ${EBIN} -I ${INC} -eval "proto:gen_proto(\"${CFG_PATH}\",\"${OUT_PATH}\")" -s init stop && cp -r ${OUT_PATH} ${PRO_PATH} && echo "生成解析协议文件完成"

}

# 生成协议配置模块
function gen_proto_cfg() {
    MOD=$1
    FILE=${CFG_PATH}${MOD}.erl
    cat proto.tpl | sed "s/{mod}/"${MOD}"/g" > ${FILE} && echo "生成协议配置文件"${FILE}
}

function help() {
    echo "gen_proto_cfg : 生成协议配置文件"
    echo "gen_proto     : 编译源文件并且生协议解析文件"
}

case $1 in
  gen_proto_cfg)
    gen_proto_cfg $2
  ;;
  gen_proto)
    gen_proto
  ;;
  *)
    help
esac
