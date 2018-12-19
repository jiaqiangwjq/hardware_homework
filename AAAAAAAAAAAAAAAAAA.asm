; ============= ;
; 中国矿业大学   ;
; HQFC-A 机箱   ;
; 硬件课程设计   ;
; 出租车计价器   ;
; ============= ;

;================================================================
; 8255: CS 接 288H ~ 28FH, PA0 ~ PA7 接 128x64 液晶屏的 D7 ~ D0,                      
; PC0 接液晶屏 D/I 端, PC1 接 RW 端, PC2 接 E 端,
; PB0 接 K1, PB1 接 8254 的 OUT0, PB2 接 K2, PB3 接 K3,
; PB6 接 K0               
; 所以: A、C 口输出, B 口输入, 方式字 82H                                               
;
; 8254: CS 接 280H ~ 287H, GATE0 接 +5V, CLK0 接 2MHZ, 
; OUT0 接 PB1
;
; 0832: CS 接 290H ~ 297H
;
; K0: 启停
; K1: 白天/夜间计价
; K2: 清零
; K3: 给高电平, 设置起步价
; 
;================================================================
 
 
 
;出租车计价器
PORT_8254_0   EQU 280H   
PORT_8254_CTL EQU 283H

PORT_8255_A   EQU 288H
PORT_8255_B   EQU 289H
PORT_8255_C   EQU 28AH
PORT_8255_CTL EQU 28BH

PORT_0832_A   EQU 290H

DATA SEGMENT             ;LCD显示字符内码

TABLE1      DW  0B0D7H,0A1A1H,0CCECH,0A3BAH
            ;白（空格）天：或晚（空格）上：
            DW 0A1A1H,0A1A1H
            ;
             DW 0A1A1H,0A1A1H
            ;
            DW 0C0EFH,0A1A1H,0B3CCH,0A3BAH
            ;    里  （空格）  程     ：    
            DW ?,?       ;路程数值显示    
            DW 0A3EBH,0A3EDH
            ;    k      m
            DW 0BDF0H,0A1A1H,0B6EEH,0A3BAH
            ;    金  （空格）   额     ：
            DW ?,?       ;金额数值显示
            DW 0D4AAH,0A1A1H
            ;    元  （空格）

     HZ_ADR DB ?            ;存放显示行起始端口地址

            ;    0      1      2      3      4      5      6      7      8      9
     NUMBER DW 0A3B0H,0A3B1H,0A3B2H,0A3B3H,0A3B4H,0A3B5H,0A3B6H,0A3B7H,0A3B8H,0A3B9H
            
     TIME DW 0B0D7H,0CCECH,0CDEDH,0C9CFH
            ;  白     天     晚     上

     T1  DB ?            ;路程十位
     T2  DB ?            ;路程个位
     T3  DB ?            ;金额十位
     T4  DB ?            ;金额个位
     DIST   DW 0         ;里程初始为 0km
     PAY    DW 8         ;白天起步价8元

DATA ENDS
   
CODE   SEGMENT
       ASSUME  CS:CODE,DS:DATA

START: MOV AX,DATA
       MOV DS,AX

;默认电动机不转
EE:  
       MOV DX,PORT_0832_A       
       MOV AL,80H
       OUT DX,AL

       MOV DX,PORT_8255_B
       IN AL,DX
       AND AL,40H       ; 判断启停
       TEST AL,40H
       JZ EE
       MOV AL,0FFH
       MOV DX,PORT_0832_A
       OUT DX,AL

      ;初始化8255，A口输出，B口输入，C口输出，A口、B口方式0
       MOV DX,PORT_8255_CTL
       MOV AL,10000010B  
       OUT DX,AL

       ;初始化8254，通道0，先低后高，方式3，BCD计数
       MOV DX,PORT_8254_CTL
       MOV AL,00110111B      
       OUT DX,AL
       MOV DX,PORT_8254_0
       MOV AL,00H            ;每1000个脉冲代表一公里
       OUT DX,AL            
       MOV AL,10H
       OUT DX,AL 

       MOV   DX, PORT_8255_B
       IN   AL,DX
       AND  AL,04H
       TEST AL,04H
       JZ   zero1    
       MOV  PAY,8
       MOV  DIST,0
      
zero1: IN   AL,DX
       AND  AL,08H
       TEST AL,08H
       JZ   JUDGE
       MOV PAY,0
       MOV DIST,0
       JMP A
        
;从8255读取脉冲信息
JUDGE: 
       IN   AL,DX
       AND  AL,02h
       TEST AL,02h           ;判断是否为高电平
       JNZ  JUDGE            ;若为高电平，继续检测
       INC  DIST             ;若不是高电平了，即有一个脉冲，路程加1km
    
 ;设置晚上起步价（在路程为1km时将起步价在原基础上加2）
          
       CMP  DIST,1            ;判断是否超过1km
       JA   A           
       IN   AL,DX
       AND  AL,01H     
       TEST AL,01H           ;判断开关状态，闭合高电平表示晚上
       JZ   A                ;若最低位为低电平,表示白天
       LEA BX,TIME
       MOV AX,[BX+4]
       LEA DI,TABLE1
       MOV WORD PTR[DI],AX

       LEA BX,TIME
       MOV AX,[BX+6]
       LEA DI,TABLE1
       MOV WORD PTR[DI+4],AX
       MOV PAY,10        ;否则是晚上，起步价由8元变成10元，故加2
;判断是否超过起步里程3km
A: 
       CMP  DIST,3            ;与起步里程比较，判断是否超过3km
       JBE  TRANS            ;小于3km则金额仍为起步价，转去求路程和金额的个位和十位
       IN   AL,DX
       AND  AL,01H
       TEST AL,01H           ;检查是否为晚上
       JZ   DAY              ;白天
       INC  PAY              ;晚上一公里三元

;白天一公里两元
DAY:   ADD   PAY,02H
;利用除法求十进制的个位和十位
TRANS: MOV AX,DIST               
       MOV BL,10             
       DIV BL
       MOV T2,AH          ;路程个位, 因为 AH 存储余数
       MOV T1,AL          ;路程十位, 因为 AL 存储商

       MOV AX,PAY
       MOV BL,10
       DIV BL
       MOV T4,AH          ;金额个位
       MOV T3,AL          ;金额十位

;将金额、路程的个位、十位分别转换为LCD上显示的内码
       LEA BX,NUMBER
       ADD BL,T1
       ADD BL,T1
       MOV AX,[BX]
       LEA DI,TABLE1
       MOV WORD PTR[DI+24],AX ;路程十位

       LEA BX,NUMBER
       ADD BL,T2
       ADD BL,T2
       MOV AX,[BX]
       LEA DI,TABLE1
       MOV WORD PTR[DI+26],AX ;路程个位

       LEA BX,NUMBER
       ADD BL,T3
       ADD BL,T3
       MOV AX,[BX]
       LEA DI,TABLE1
       MOV WORD PTR[DI+40],AX ;金额十位

       LEA BX,NUMBER
       ADD BL,T4
       ADD BL,T4
       MOV AX,[BX]
       LEA DI,TABLE1
       MOV WORD PTR[DI+42],AX ;金额个位

;路程价格输出
SHOW:  CALL CLEAR             ;LCD 清除
       LEA BX, TABLE1
       MOV CH,2               ;显示第2行信息 
       CALL  LCD_DISTP
       LEA BX, TABLE1
       MOV CH,3               ;显示第3行信息
       CALL LCD_DISTP
       LEA BX, TABLE1
        MOV CH,1
       CALL LCD_DISTP

TT:    JMP  START             ;TT

CLEAR  PROC
       MOV AL,0CH
       MOV DX, PORT_8255_A
       OUT DX,AL              ;设置CLEAR命令
       CALL CMD_SETUP         ;启动LCD执行命令
       RET
CLEAR  ENDP

FUNCUP PROC
       MOV AL, 34H            ;LCD显示状态命令
       OUT DX, AL
       CALL CMD_SETUP
       RET
FUNCUP ENDP

LCD_DISTP PROC
       LEA BX, TABLE1
       CMP CH,1
       JZ  DISTP_FIR
       CMP CH,2
       JZ DISTP_SEC
       CMP CH,3
       JZ DISTP_THI

DISTP_FIR:MOV BYTE PTR HZ_ADR,80H  ;第行起始端口地址 
          JMP NEXT      
DISTP_SEC:MOV BYTE PTR HZ_ADR,90H
           ADD BX,16
           JMP NEXT
DISTP_THI:MOV BYTE PTR HZ_ADR,88H  ;第三行起始端口地址
           ADD BX,32
           JMP NEXT
NEXT:    MOV CL,8   
    
CONTINUE:PUSH CX
         MOV AL,HZ_ADR
         MOV DX, PORT_8255_A
         OUT DX, AL
         CALL CMD_SETUP          ;设定DDRAM地址命令
         MOV AX,[BX]
         PUSH AX
         MOV AL,AH               ;先送汉字编码高位
         MOV DX,PORT_8255_A
         OUT DX,AL
         CALL DATA_SETUP         ;输出汉字编码高字节
         CALL DELAY              ;延迟
         POP AX
         MOV DX,PORT_8255_A
         OUT DX, AL
         CALL DATA_SETUP         ;输出汉字编码低字节
         CALL DELAY
         INC BX
         INC BX                  ;修改显示内码缓冲区指针
         INC BYTE PTR HZ_ADR     ;修改LCD显示端口地址
         POP CX
         DEC CL
         JNZ  CONTINUE
         RET
LCD_DISTP ENDP

CMD_SETUP PROC
         MOV DX,PORT_8255_A       ;指向8255端口控制端口
         ADD DX,2
         NOP
         MOV AL,00000000B         ;PC1置0,pc0置0 （LCD I端=0，W端＝0）
         OUT DX, AL
         CALL DELAY
         NOP
         MOV AL,00000100B         ;PC2置1 （LCD E端＝1）
         OUT DX, AL
         NOP
         CALL DELAY
         MOV AL, 00000000B        ;PC2置0,（LCD E端置0）
         OUT DX, AL
         CALL DELAY
RET

CMD_SETUP  ENDP

DATA_SETUP PROC
           MOV DX,PORT_8255_A     ;指向8255控制端口
           ADD DX,2
           MOV AL,00000001B       ;PC1置0，PC0=1 （LCD I端=1）
           OUT DX, AL
           NOP
           CALL DELAY
           MOV AL,00000101B       ;PC2置1 （LCD E端＝1）
           OUT DX, AL
           NOP
           CALL DELAY
           MOV AL, 00000001B      ;PC2置0,（LCD E端＝0）
           OUT DX, AL
           NOP
           CALL DELAY
           RET
DATA_SETUP ENDP

DELAY      PROC
           PUSH CX
           PUSH DX
           MOV CX, 0FFFH
X1:        LOOP   X1
           POP DX
           POP CX
           RET
DELAY      ENDP

JMP JUDGE

CODE ENDS
     END START 