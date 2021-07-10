#!/usr/bin/env bash

# 获取根路径
function get_root() {
    target_file=`dirname $0`
    target_file=`basename ${target_file}`
    while [ -L "${target_file}" ]
    do
        target_file=`readlink ${target_file}`
        cd `dirname ${target_file}`
        target_file=`basename ${target_file}`
    done
    echo `pwd -P`
}

# 引用配置脚本
ROOT=$(get_root)
source ${ROOT}/setting.sh

# 定义字典
declare -A cfg
cfg[root]=${ROOT}
cfg[server_path]=${SERVER_PATH}
cfg[proto_path]=${SERVER_PATH}/src/proto/

if [ ! $msg ]; then
    declare -A msg
fi

# 获取依赖库
msg[gen_proto]="生协议解析文件"
function gen_proto() {
    cd ${cfg[root]}/proto && sh gen_proto.sh gen_proto $cfg[proto_path] $@
}

# 获取依赖库
msg[gen_proto_cfg]="生成协议配置模块"
function gen_proto_cfg() {
    cd ${cfg[root]}/proto && sh gen_proto.sh gen_proto_cfg $@
}

function help() {
    printf "请输入以下指令：\n"
    for key in "${!msg[@]}"; do
        printf "%-20s%s\n" $key ${msg[$key]}
    done
}

if [[ ${!msg[@]} =~ $1 ]]; then
    cmd=$1
    args=${@:2}
    ${cmd} ${args}
else
    help
fi