# RV32-S7-F

一个基于RISC-V架构的32位处理器核RTL设计，采用Chisel编写

## 微架构
顺序七级单发射流水线
- ICache：直接相联
- DCache：直接相联，写直通，带有 Store Buffer
- MMU：支持完整 Sv32 虚拟内存管理，取指访存共用 PTW
- TLB：iTLB 和 dTLB 分离，支持大页面，全相联随机替换

## 特性
RV32IMASU
  - 支持完整特权级和 Sv32 虚拟内存
  - 能够启动支持虚拟内存管理的 Linux 操作系统

## TODO
- [ ] 支持外设中断
- [ ] 动态分支预测
- [ ] 多路组相联的 ICache 和 DCache
- [ ] LRU 替换策略
