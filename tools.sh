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
declare -A CFG
CFG[root]=${ROOT}
CFG[code_path]=${CODE_PATH}
CFG[proto_path]=${CODE_PATH}/src/proto/

declare -A MSG

# 获取依赖库
MSG[gen_proto]="生协议解析文件"
function gen_proto() {
    cd ${CFG[root]}/proto && sh gen_proto.sh gen_proto ${CFG[proto_path]} $@
}

# 获取依赖库
MSG[gen_proto_cfg]="生成协议配置模块(gen_proto_cfg:file)"
function gen_proto_cfg() {
    cd ${CFG[root]}/proto && sh gen_proto.sh gen_proto_cfg $@
}

function help() {
    printf "请输入以下指令：\n"
    for key in "${!MSG[@]}"; do
        printf "%-20s%s\n" $key ${MSG[$key]}
    done
}

# 检查键是否在字典中
check_key_in_dict() {
    dict=$1
    check=$2
    for key in "${!dict[@]}"; do
      if [[ ${check} == ${key} ]]; then
          return 0
      fi
    done
    return 1
}

if [[ $(check_key_in_dict ${MSG} $1) == 0 ]]; then
    cmd=$1
    args=${@:2}
    ${cmd} ${args}
else
    help
fi