#### 基于gen_tcp的聊天室

* 服务端运行说明

```
cd src
erl
c(chatserv).
chatserv:start_server().
```

* 客户端运行说明(开启多个终端)

```
cd src 
erl
c(chatclient).
chatclient:start_client().
```