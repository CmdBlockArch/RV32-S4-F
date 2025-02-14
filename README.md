# RV32-S7-F

一个 RISC-V 32 位处理器核 RTL 设计，采用 Chisel 编写

## 微架构
顺序七级单发射流水线
- BTB + RAS 动态分支预测
- ICache：二路组相联，LRU
- DCache：二路组相联，LRU，写直通
- Store Buffer：支持请求合并，异步写入存储器
- MMU：支持完整 Sv32 虚拟内存管理，取指访存共用 PTW
- TLB：iTLB 和 dTLB 分离，支持大页面，全相联随机替换

## 特性
RV32IMA SU
- 支持完整特权级和 Sv32 虚拟内存
- 能够启动支持虚拟内存管理的 Linux 操作系统
- 支持中断，包括软件中断、时钟中断、外部中断
