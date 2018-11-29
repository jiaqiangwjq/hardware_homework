; =============;
; HQFC-A 机箱  ;
; 硬件课程设计  ;
; 出租车计价器  ;
; =============;

;================================================================
; 8255: CS 接 288H ~ 28FH, PA0 ~ PA7 接 128x64 液晶屏的 D7 ~ D0,  
; PB0 接键盘行 3, PC4 ~ PC7 接键盘列 0 ~ 3,                       
; PC0 接液晶屏 D/I 端, PC1 接 RW 端, PC2 接 E 端。                
; 所以方式字: 88H                                                 
;
; 8254: CS 接 280H ~ 287H, GATE0、GATE1 接 +5V, CLK0 接 2MHZ, 
; OUT0 接 CLK1
;
; 0832: CS 接 290H ~ 297H
;
; 数字 0: 启动/停止
; 数字 1: 白天/夜晚
; 数字 2: 清零
; 数字 3: 按喇叭(暂未实现)
;================================================================


IO_ADDRESS      equ  288h
PORT_A EQU 288H
PORT_B EQU 289H
PORT_C EQU 28AH
PORT_CTL EQU 28BH

PORT_0832A EQU 290H

PORT_8254_0 EQU 280H
PORT_8254_1 EQU 281H
PORT_8254_2 EQU 282H
PORT_8254_CTL EQU 283H

DATA            SEGMENT
                 ;  空格             空       车       0      0               空格
HZK_TAB          DW 0A3A0H, 0A3A0H, 0BFD5H, 0B3B5H, 0A3B0H, 0A3B0H, 0A3A0H, 0A3A0H
                 ;  空格             空格     空格    空格     空格           空格 
                DW 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H

                 ;  空格             空格     空格     空格    空格            空格
HZY_TAB         DW 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H
                 ;  空格             有       客       0      0               空格 
                DW 0A3A0H, 0A3A0H, 0D3D0H, 0BFCDH, 0A3B0H, 0A3B0H, 0A3A0H, 0A3A0H   

HZ_ADR          DB  ?                   ;存放显示行起始端口地址
                ;   0      1     2     3
TABLE           DB 070H, 0B0H, 0D0H, 0E0H
                ;   0        1       2       3        4      5        6       7       8       9                 
NUM             DW 0A3B0H, 0A3B0H, 0A3B0H, 0A3B0H, 0A3B0H, 0A3B0H, 0A3B0H, 0A3B0H, 0A3B0H, 0A3B0H

my_stack        dw 0
DATA            ENDS

code segment
   assume cs:code, ds:data
   
    ; 显示空车
START_K:  MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,88H
                OUT DX,AL                       ;8255初始化
               mov al,0ffh
               mov dx,300H
               out dx, al
               CALL CLEARK              ;LCD 清除

                LEA BX,  HZK_TAB
                MOV CH,2                        ;显示第2行信息 
                CALL  LCD_DISPK
                LEA BX, HZK_TAB
                MOV CH,3                  ;    显示第3行信息
                CALL LCD_DISPK
                
        l1:     jmp     START_1 ;l1
        CLEARK           PROC
                MOV AL,0CH
                MOV DX, IO_ADDRESS
                OUT DX,AL               ;设置CLEAR命令
                CALL CMD_SETUPK          ;启动LCD执行命令
                RET
CLEARK           ENDP

FUNCUPK          PROC

                MOV AL, 34H             ;LCD显示状态命令
                OUT DX, AL
                CALL CMD_SETUP
                RET
FUNCUPK           ENDP

LCD_DISPK        PROC
                LEA BX, HZK_TAB
                CMP CH, 2
                JZ  DISP_SECK
                MOV BYTE PTR HZ_ADR, 88H        ;第三行起始端口地址
                ADD BX,16                        ;指向第二行信息
                JMP  next
DISP_SECK:       MOV BYTE PTR HZ_ADR,90H
nextK:           mov cl,8
continueK:       push cx
                MOV AL,HZ_ADR
                MOV DX, IO_ADDRESS
                OUT DX, AL
                CALL CMD_SETUPK          ;设定DDRAM地址命令
                MOV AX,[BX]
                PUSH AX
                MOV AL,AH               ;先送汉字编码高位
                MOV DX,IO_ADDRESS
                OUT DX,AL
                CALL DATA_SETUPK         ;输出汉字编码高字节
                CALL DELAYK              ;延迟
                POP AX
                MOV DX,IO_ADDRESS
                OUT DX, AL
                CALL DATA_SETUPK         ;输出汉字编码低字节
                CALL DELAYK
                INC BX
                INC BX                  ;修改显示内码缓冲区指针
                INC BYTE PTR HZ_ADR     ;修改LCD显示端口地址
                POP CX
                DEC CL
                JNZ  continueK
                RET
LCD_DISPK   ENDP

CMD_SETUPK       PROC
                MOV DX,IO_ADDRESS                ;指向8255端口控制端口
                ADD DX,2
                NOP
                MOV AL,00000000B                ;PC1置0,pc0置0 （LCD I端=0，W端＝0）
                OUT DX, AL
                call DELAYK
                NOP
                MOV AL,00000100B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call DELAYK
                MOV AL, 00000000B               ;PC2置0,（LCD E端置0）
                OUT DX, AL
                call DELAYK

                RET
CMD_SETUPK       ENDP

DATA_SETUPK      PROC
                MOV DX,IO_ADDRESS                ;指向8255控制端口
                ADD DX,2
                MOV AL,00000001B                ;PC1置0，PC0=1 （LCD I端=1）
                OUT DX, AL
                NOP
                call DELAYK
                MOV AL,00000101B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call DELAYK
                MOV AL, 00000001B               ;PC2置0,（LCD E端＝0）
                OUT DX, AL
                NOP
                call DELAYK
                RET
DATA_SETUPK      ENDP

DELAYK           PROC
                push cx
                push dx
                MOV CX, 0fffh
 x1:           loop   x1
                pop dx
                pop cx
                RET
DELAYK           ENDP
                
                ;显示有客
START_Y:  MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,88H
                OUT DX,AL                       ;8255初始化
               mov al,0ffh
               mov dx,300H
               out dx, al
               CALL CLEARY              ;LCD 清除

                LEA BX,  HZY_TAB
                MOV CH,2                        ;显示第2行信息 
                CALL  LCD_DISPY
                LEA BX, HZY_TAB
                MOV CH,3                  ;    显示第3行信息
                CALL LCD_DISPY
        l2:     jmp     START_2 ;l2

CLEARY           PROC
                MOV AL,0CH
                MOV DX, IO_ADDRESS
                OUT DX,AL               ;设置CLEAR命令
                CALL CMD_SETUPY          ;启动LCD执行命令
                RET
CLEARY           ENDP

FUNCUPY          PROC

                MOV AL, 34H             ;LCD显示状态命令
                OUT DX, AL
                CALL CMD_SETUPY
                RET
FUNCUPY           ENDP

LCD_DISPY        PROC
                LEA BX, HZY_TAB
                CMP CH, 2
                JZ  DISP_SECY
                MOV BYTE PTR HZ_ADR, 88H        ;第三行起始端口地址
                ADD BX,16                        ;指向第二行信息
                JMP  next
DISP_SECY:       MOV BYTE PTR HZ_ADR,90H
nextY:           mov cl,8
continueY:       push cx
                MOV AL,HZ_ADR
                MOV DX, IO_ADDRESS
                OUT DX, AL
                CALL CMD_SETUPY          ;设定DDRAM地址命令
                MOV AX,[BX]
                PUSH AX
                MOV AL,AH               ;先送汉字编码高位
                MOV DX,IO_ADDRESS
                OUT DX,AL
                CALL DATA_SETUPY         ;输出汉字编码高字节
                CALL DELAYY              ;延迟
                POP AX
                MOV DX,IO_ADDRESS
                OUT DX, AL
                CALL DATA_SETUP         ;输出汉字编码低字节
                CALL DELAYY
                INC BX
                INC BX                  ;修改显示内码缓冲区指针
                INC BYTE PTR HZ_ADR     ;修改LCD显示端口地址
                POP CX
                DEC CL
                JNZ  continueY
                RET
LCD_DISPY   ENDP

CMD_SETUPY       PROC
                MOV DX,IO_ADDRESS                ;指向8255端口控制端口
                ADD DX,2
                NOP
                MOV AL,00000000B                ;PC1置0,pc0置0 （LCD I端=0，W端＝0）
                OUT DX, AL
                call DELAYY
                NOP
                MOV AL,00000100B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call DELAYY
                MOV AL, 00000000B               ;PC2置0,（LCD E端置0）
                OUT DX, AL
                call DELAYY

                RET
CMD_SETUPY       ENDP

DATA_SETUPY      PROC
                MOV DX,IO_ADDRESS                ;指向8255控制端口
                ADD DX,2
                MOV AL,00000001B                ;PC1置0，PC0=1 （LCD I端=1）
                OUT DX, AL
                NOP
                call DELAYY
                MOV AL,00000101B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call DELAYY
                MOV AL, 00000001B               ;PC2置0,（LCD E端＝0）
                OUT DX, AL
                NOP
                call DELAYY
                RET
DATA_SETUPY      ENDP

DELAYY           PROC
                push cx
                push dx
                MOV CX, 0fffh
 x1:           loop   x1
                pop dx
                pop cx
                RET
DELAYY           ENDP

START_1:
    pre_START: MOV DX, PORT_0832A                     ; 先不让电机转动
    MOV AL, 00H
    OUT DX, AL
    
    mov ax, 00h
    mov bx, 0
    mov my_stack[bx], ax

    MOV AL, 88H         ;8255 初始化 A 口方式 0 输出，上 C 口输入，下 C 口输出，B 口方式 0 输出
    MOV DX, PORT_CTL
    OUT DX, AL

    ;读列，查看是否所有键均松开
START_2: MOV AL, 88H         ;8255 初始化 A 口方式 0 输出，上 C 口输入，下 C 口输出，B 口方式 0 输出
    MOV DX, PORT_CTL
    OUT DX, AL
    MOV DX, PORT_C
WAIT_OPEN: IN AL, DX    ;从 C 口读状态
    AND AL, 0F0H     ;只查高 4 位，因为 PC7 ~ PC3 接 0123 列
    CMP AL, 0F0H     ;是否都为 1(各键均松开?)
    JNE WAIT_OPEN
    
    ;各键均已松开，再查列是否有 0，即是否有键压下
WAIT_PRES: IN AL, DX
    AND AL, 0F0H
    CMP AL, 0F0H
    JE WAIT_PRES
    
    ;有键压下，延时 20 ms, 消抖
    MOV CX, 16EAH
DELAY_1: LOOP DELAY_1

    ;再查列，看键是否仍被压着
    IN AL, DX
    AND AL, 0F0H
    CMP AL, 0F0H
    JE WAIT_PRES    ;已松开，转到等待压键状态
    
    ;键仍被压着，确定哪个键被压下(在 PB0 所在行找)
    MOV AL, 0FEH
    MOV DX, PORT_B
    OUT DX, AL
    MOV DX, PORT_C
    IN AL, DX
    AND AL, 0F0H
    CMP AL, 0F0H
    JNE ENCODE
    
ENCODE: MOV BX, 0003H   ;建立地址指针，现指向 3
    IN AL, DX
    AND AL,0F0H         ;只取高四位
NEXT_TRY: CMP AL, TABLE[BX]
    JE DONE
    DEC BX
    JNS NEXT_TRY
    MOV BX, 0003H       ;防止 BX 减越界了
    
DONE: CMP AL, 070H   ;启
    JE JUMP_ST
    CMP AL, 0B0H     ;白天
    JE JUMP_DN
    
    CMP AL, 0D0H
    call ZERO
    ;MOV AH, 00H
    ;MOV DX, PORT_B
    ;OUT DX,AL
    ;JMP START

JUMP_ST: mov bx,0
    mov ax,my_stack[bx]
    cmp al, 00h
    ;pop cx
    ;CMP CL, 00H
    JE ENGINE_ON
    JMP ENGINE_OFF

JUMP_DN: ;pop cx
    mov bx, 0
    mov ax, my_stack[bx]
    cmp ah, 00h
    ;CMP CH, 00H
    JE DAY
    JMP NIGHT

ENGINE_ON:
    ;MOV CL, 01H     ; CL 置 1, 说明机器在运行状态
    ;push cx
    mov bx,0
    mov al, 01h
    mov my_stack[bx], al    ;寄存器可以指明是字操作还是字节操作
    MOV DX, PORT_0832A   
    MOV AL, 0FFH
    OUT DX, AL
    JMP START_Y     ;启动后，跳转到有客

ENGINE_OFF:
    ;MOV CL, 00H     ; CL 置 0, 说明机器未运行
    ;push cx
    mov bx, 0
    mov al, 00h
    mov my_stack[bx], al
    MOV DX, PORT_0832A
    MOV AL, 00H
    OUT DX, AL
    JMP START_K       ;停止后，跳转到空车

DAY: ;MOV CH, 01H    ; CH 置 1 表示现在是白天,将要切换到夜晚
    ;push cx
    ;CALL CHARGE
    mov bx, 0
    mov ax, 11h
    mov my_stack[bx], ax
    MOV BX, 0BH
    MOV AX, HZY_TAB[BX]
    ADD AX, 0001H
    MOV HZY_TAB[BX], AX 
    ;MOV DS:[BX], AX
    MOV BX, 0DH
    MOV AX, HZY_TAB[BX]
    ADD AX, 0002H
    MOV HZY_TAB[BX], AX
    JMP START_Y       ; 要把数据送到显示屏上啊
   
NIGHT: ;MOV CH, 00H  ; CH 置 0 表示现在是夜晚,将要切换到白天      
    ;push cx
    ;CALL CHARGE
    mov bx, 0
    mov ax, 01h
    mov my_stack[bx], ax
    MOV BX, 0BH
    MOV AX, HZY_TAB[BX]
    ADD AX, 0001H
    MOV HZY_TAB[BX], AX 
    ;MOV DS:[BX], AX
    MOV BX, 0DH
    MOV AX, HZY_TAB[BX]
    ADD AX, 0002H
    MOV HZY_TAB[BX], AX
    JMP START_Y

ZERO: MOV BX, 0B0H   ; 清零
    MOV AX, 0A3B0H
    MOV HZY_TAB[BX], AX
    MOV BX, 0D0H
    MOV HZY_TAB[BX], AX
    JMP START_K
    
    ;8254 初始化:计时 2S
CHARGE:MOV AL, 35H    ;通道 0 方式字；先低后高；方式 2
    MOV DX, PORT_8254_CTL
    OUT DX, AL
    MOV AL, 00H
    MOV DX, PORT_8254_0
    OUT DX, AL
    MOV AL, 50H
    OUT DX, AL

    MOV AL, 77H    ;通道 1 方式字；先低后高；方式 3
    MOV DX, PORT_8254_CTL
    OUT DX, AL
    MOV AL, 00H
    MOV DX, PORT_8254_1
    OUT DX, AL
    MOV AL, 08H
    OUT DX, AL
    
    ; T1 检测由低电平到高电平，T2 检测由高电平到低电平
    MOV DX, PORT_CTL
    MOV AL, 10001010B
    OUT DX, AL
    MOV DX, PORT_B
    T1: IN AL,DX
        AND AL,01H
        JZ T1
    T2: IN AL,DX
        AND AL,01H
        JNZ T2

code ends
     end START_K