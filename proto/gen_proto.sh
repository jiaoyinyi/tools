#!/bin/bash

# ebin路径
EBIN=ebin/
# 头文件路径
INC=inc/
# 协议配置文件路径
CFG_PATH=./cfg/
OUT_PATH=./out/

echo "编译erl源文件"
erl -make
echo "编译erl源文件完成"
# 执行生成解析协议文件脚本
erl -noshell -pa ${EBIN} -I ${INC} -s proto main ${CFG_PATH} ${OUT_PATH} -s init stop && echo "生成解析协议文件完成"

