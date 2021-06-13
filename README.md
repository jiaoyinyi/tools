erlang开发工具
====

### 生成协议解析文件 proto 
1. 配置文件 ./proto/gen_proto.sh 
> 配置协议定义文件路径和协议解析输出路径
2. 生成协议配置模块
> ./gen_proto.sh gen_proto_cfg proto_name
3. 生成协议解析文件
> ./gen_proto.sh gen_proto

### 支持字段类型
1. 基础字段类型
> int8、uint8、int16、uint16、int32、uint32、int64、uint64、string、bool
2. 其他字段类型
> array、tuple、rec、map
3. map支持基础字段类型加默认值
```
#filed{type = map, name = test_map3, data = [
    #filed{type = uint32, name = id, desc = "角色ID", default = 0}
    , #filed{type = string, name = name, desc = "角色名", default = <<>>}
]}
```