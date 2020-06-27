#!/bin/bash

# 协议配置路径
PROTO_CFG_PATH="./test/cfg/"
# 生成的协议解析文件路径
PROTO_PATH="./test/proto/"
# ebin路径
EBIN="ebin/"
# 代码路径
PROTO_SRC="src/proto/"
# 头文件路径
INC="inc/"
# Emakefile内容，编译PROTO_SRC路径的代码
GEN_PROTO="{[${PROTO_SRC}*],[debug_info,{i,${INC}},{outdir,${EBIN}}]}."
echo "Emakefile内容:"${GEN_PROTO}
erl -noshell -s make all [${GEN_PROTO}] -s init stop
# 编译PROTO_CFG_PATH路径下的所有erl文件
for file in `find ${PROTO_CFG_PATH} -type f -name "*.erl"` ; do
  echo "编译${file}文件"
  erlc -I ${INC} -o ${EBIN} ${file}
done
# 执行生成解析协议文件脚本
erl -noshell -pa ${EBIN} -I ${INC} -s proto main ${PROTO_CFG_PATH} ${PROTO_PATH} -s init stop && echo "生成解析协议文件完成"

