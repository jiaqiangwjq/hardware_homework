# hardware_homework


`第一次修改`

**按键介绍**
- 0 启动，1 停止，2 白天，3 夜间，4 清零，5 按喇叭
- 今天 PC1 ~ PC3 接反了，下次改过来
###### 本次改动内容如下
1. PA1 接 行 2
2. 目的是扩充按键，原来只有 4 个键，现在扩充至 8 个键可用
3. PC4 接行3，PC5 接行2
4. 8254 OUT1 接 PB0
5. PB1 接 LCD I 端
6. PB2 接 LCD E 端
7. PB3 接 

###### 改动时间：2018-11-22

---

`第二次修改`
###### 本次改动内容如下
1. 对判断按键部分进行了优化。只取、只查看 C 口高四位，无论 C 口低四位是什么
2. 查找哪一个键被按下时，先置指针指向数字 3, 原来指针是指向数字 3 后面一个地址
3. 加入了测 8254 OUT1 高低电平的代码, 用来延时

###### 改动时间：2018-11-27
---




`第三次修改`

1. 将空车和有客分为两个数组
2. 定义一个字的空间 my_stack 作为一个虚拟的栈来使用(判断具体哪个键的时候用的)
3. 确定了案件顺序：0 启动 -> 1 白天 -> 1 夜晚 -> 2 清零 -> 3喇叭  -> 0 停止

**改动时间: 2018-11-29**

---
`第四次修改`
1. 添加了喇叭的功能
2. 对白天和夜间的计价进行了优化，很笨蛋的优化方式：手动控制数字跳转，数字每跳一次，延时，然后送 LCD 显示




