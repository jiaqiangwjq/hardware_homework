; =============;
; 中国矿业大学  ;
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
; OUT0 接 CLK1, OUT1 接 PB0
;
; 0832: CS 接 290H ~ 297H
;
; 数字 0: 启动/停止
; 数字 1: 白天/夜晚
; 数字 2: 清零
; 数字 3: 跑马灯
; 开关 K1 键: 按喇叭
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

;空车表                 ;  空格             空       车               0      0     空格
HZK_TAB          DW 0A3A0H, 0A3A0H, 0BFD5H, 0B3B5H, 0A3A0H, 0A3B0H, 0A3B0H, 0A3A0H
                 ;  空格             空格     空格    空格     空格           空格
                 DW 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H

;有客表                 ;  空格             空格     空格     空格    空格    空格
HZY_TAB         DW 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H
                 ;  空格             有       客               0      0      空格 
                DW 0A3A0H, 0A3A0H, 0D3D0H, 0BFCDH, 0A3A0H, 0A3B0H, 0A3B0H, 0A3A0H 
                
;公里价格表       ;  空格             公      里     空格       0       0     空格
HZX_TAB         DW 0A3A0H, 0A3A0H, 0B9ABH, 0C0EFH, 0A3A0H, 0A3B0H, 0A3B0H, 0A3A0H
                 ;  空格             价       格     空格      0       0      空格 
                DW 0A3A0H, 0A3A0H, 0BCDBH, 0B8F1H, 0A3A0H, 0A3B0H, 0A3B0H, 0A3A0H    

HZ_ADR          DB  ?                   ;存放显示行起始端口地址

;键盘按键表      ;   0      1     2     3
TABLEN           DB 070H, 0B0H, 0D0H, 0E0H

;自定义一栈空间
my_stack        dw 0, 0
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
                CALL CMD_SETUPK
                RET
FUNCUPK           ENDP

LCD_DISPK        PROC
                LEA BX, HZK_TAB
                CMP CH, 2
                JZ  DISP_SECK
                MOV BYTE PTR HZ_ADR, 88H        ;第三行起始端口地址
                ADD BX,16                        ;指向第二行信息
                JMP  nextK
DISP_SECK:       MOV BYTE PTR HZ_ADR,90H
nextK:           mov cl,8
continueK:       push cx
                MOV AL,HZ_ADR
                MOV DX, IO_ADDRESS
                OUT DX, AL
                call CMD_SETUPK          ;设定DDRAM地址命令
                MOV AX,[BX]
                PUSH AX
                MOV AL,AH               ;先送汉字编码高位
                MOV DX,IO_ADDRESS
                OUT DX,AL
                call DATA_SETUPK         ;输出汉字编码高字节
                call DELAYK              ;延迟
                POP AX
                MOV DX,IO_ADDRESS
                OUT DX, AL
                call DATA_SETUPK         ;输出汉字编码低字节
                call DELAYK
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
NEXT_TRY: CMP AL, TABLEN[BX]
    JE DONE
    DEC BX
    ;JNS NEXT_TRY
    ;MOV BX, 0003H       ;防止 BX 减越界了
    
DONE: CMP AL, 070H   ;启
    JE JUMP_ST
    CMP AL, 0B0H     ;白天
    JE JUMP_DN
    CMP AL, 0D0H
    JE ZZ
    CMP AL, 0E0H
    MOV CX, 2
    PUSH CX
    JMP LIGHT
    JMP START_1

       
LIGHT: MOV AL,82H
    MOV DX,PORT_CTL
    OUT DX,AL
    MOV AL, 01H
    AGAIN:ROL AL,1 
    MOV DX,PORT_A
    OUT DX,AL
    MOV BX,0FFFFH   
    DELAY_L1:MOV CX,0FH
    DELAY_L2:LOOP DELAY_L2
        DEC BX
        JNZ DELAY_L1
        ;POP CX
        JMP AGAIN     
        ;JMP START_2
ZZ: MOV BX, 19H
    mov AX, 0B0A3H
    MOV HZX_TAB[BX], AX             ;清零操作
    MOV BX, 1BH

    mov AX, 0B0A3H
    MOV HZX_TAB[BX], AX

    MOV BX, 0BH
    mov AX, 0B0A3H
    MOV HZX_TAB[BX], AX
    CALL START_X
    JMP START_2

ALARM: MOV DX, PORT_CTL
    MOV AL, 88H
    OUT DX, AL
    MOV DX, PORT_A
    MOV AL, 80H
    OUT DX, AL
    MOV AL, 80H
    OUT DX, AL
    MOV AL, 80H
    OUT DX, AL
    MOV AL, 80H
    OUT DX, AL 
    ;call CONTROL        ;2s 响铃
    MOV AL, 00H
    OUT DX, AL
    JMP START_2

JUMP_ST: mov bx,00h
    mov ax, my_stack[bx]
    cmp ax, 00h
    JE ENGINE_ON
    JMP ENGINE_OFF

JUMP_DN: mov bx, 02h
    mov ax, my_stack[bx]
    cmp ax, 00h
    JE DAY
    JMP NIGHT

ENGINE_ON:
    mov bx,00h
    mov ax, 01h
    mov my_stack[bx], ax    
    MOV DX, PORT_0832A   
    MOV AL, 0FFH
    OUT DX, AL
    JMP START_Y     ;启动后，跳转到有客

ENGINE_OFF: mov bx, 00h
    mov ax, 00h
    mov my_stack[bx], ax
    MOV DX, PORT_0832A
    MOV AL, 00H
    OUT DX, AL
    JMP START_K       ;停止后，跳转到空车

DAY: mov bx, 02h
    mov ax, 01h
    mov my_stack[bx], ax

    ;开始跳数
    MOV BX, 19H
    mov ax, HZX_TAB[BX]
    add ax, 0000h                           ;00
    mov HZX_TAB[BX],ax
    call START_X
    call CONTROL

    ;mov AX, 0B1A3H
    ;call CONTROL
    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                            ;01
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;02
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;03
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 0BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;这里公里数跳一下
    MOV HZX_TAB[BX], AX
    call START_X

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;04
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;05
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;06
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    ;MOV BX, 0BH
    ;mov ax, HZX_TAB[BX]
    ;add ax, 0100h                           ;这里公里数跳一下
    ;MOV HZX_TAB[BX], AX
    ;call START_X

    ;MOV BX, 1BH
    ;mov ax, HZX_TAB[BX]
    ;add ax, 0100h                           ;07
    ;MOV HZX_TAB[BX], AX
    ;call START_X
    ;call CONTROL

    ;MOV BX, 1BH
    ;mov ax, HZX_TAB[BX]
    ;add ax, 0100h                           ;08
    ;MOV HZX_TAB[BX], AX
    ;call START_X
   ; call CONTROL

    ;MOV BX, 1BH
    ;mov ax, HZX_TAB[BX]
   ; add ax, 0100h                           ;09
    ;MOV HZX_TAB[BX], AX
    
    MOV BX, 0BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;这里公里数跳一下
    MOV HZX_TAB[BX], AX
    call START_X
    ;call CONTROL           
    JMP START_2                     ;跳转到等待按键处
   
NIGHT: mov bx, 02h
    mov ax, 00h
    mov my_stack[bx], ax

    ;开始跳数
    MOV BX, 19H
    mov ax, HZX_TAB[BX]
    mov ax, 0B1A3h
    mov HZX_TAB[BX],ax
    MOV BX, 1BH
    mov ax, 0B0A3h                            ;10
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    ;mov AX, 0B1A3H
    ;call CONTROL
    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0200h                            ;12
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 0BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;这里公里数跳一下
    MOV HZX_TAB[BX], AX
    call START_X

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0200h                           ;14
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0200h                           ;16
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 0BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;这里公里数跳一下
    MOV HZX_TAB[BX], AX
    call START_X

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0200h                           ;18
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 19H
    mov ax, HZX_TAB[BX]
    MOV ax, 0B2A3H
    mov HZX_TAB[BX],ax
    MOV BX, 1BH
    mov ax, 0B0A3H                          ;20
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 0BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;这里公里数跳一下
    MOV HZX_TAB[BX], AX
    call START_X

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0200h                            ;22
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0200h                           ;24
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 0BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;这里公里数跳一下
    MOV HZX_TAB[BX], AX
    call START_X

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0200h                           ;26
    MOV HZX_TAB[BX], AX
    call START_X
    call CONTROL

    MOV BX, 1BH
    mov ax, HZX_TAB[BX]
    add ax, 0200h                           ;28
    MOV HZX_TAB[BX], AX

    MOV BX, 0BH
    mov ax, HZX_TAB[BX]
    add ax, 0100h                           ;这里公里数跳一下
    MOV HZX_TAB[BX], AX
    call START_X     
    JMP START_2                     ;跳转到等待按键处


    
    ;8254 初始化:计时 2S
CONTROL    PROC 
    MOV AL, 35H    ;通道 0 方式字；先低后高；方式 2
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
    RET
CONTROL ENDP
        
                ;显示有客
START_Y:    MOV AX,DATA
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
                JMP  nextY
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
                CALL DELAY              ;延迟
                POP AX
                MOV DX,IO_ADDRESS
                OUT DX, AL
                CALL DATA_SETUPY         ;输出汉字编码低字节
                CALL DELAY
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
                call DELAY
                NOP
                MOV AL,00000100B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call DELAY
                MOV AL, 00000000B               ;PC2置0,（LCD E端置0）
                OUT DX, AL
                call DELAY

                RET
CMD_SETUPY       ENDP

DATA_SETUPY      PROC
                MOV DX,IO_ADDRESS                ;指向8255控制端口
                ADD DX,2
                MOV AL,00000001B                ;PC1置0，PC0=1 （LCD I端=1）
                OUT DX, AL
                NOP
                call DELAY
                MOV AL,00000101B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call DELAY
                MOV AL, 00000001B               ;PC2置0,（LCD E端＝0）
                OUT DX, AL
                NOP
                call DELAY
                RET
DATA_SETUPY      ENDP

DELAY           PROC
                push cx
                push dx
                MOV CX, 0fffh
 x2:           loop   x2
                pop dx
                pop cx
                RET
DELAY           ENDP

                ;显示有客
START_X    PROC
               MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,88H
                OUT DX,AL                       ;8255初始化
               mov al,0ffh
               mov dx,300H
               out dx, al
               CALL CLEARX            ;LCD 清除

                LEA BX,  HZX_TAB
                MOV CH,2                        ;显示第2行信息 
                CALL  LCD_DISPX
                LEA BX, HZX_TAB
                MOV CH,3                  ;    显示第3行信息
                CALL LCD_DISPX
        ;l3:     jmp     START_X ;l3

CLEARX           PROC
                MOV AL,0CH
                MOV DX, IO_ADDRESS
                OUT DX,AL               ;设置CLEAR命令
                CALL CMD_SETUPX          ;启动LCD执行命令
                RET
CLEARX           ENDP

FUNCUPX          PROC

                MOV AL, 34H             ;LCD显示状态命令
                OUT DX, AL
                CALL CMD_SETUPX
                RET
FUNCUPX           ENDP

LCD_DISPX        PROC
                LEA BX, HZX_TAB
                CMP CH, 2
                JZ  DISP_SECX
                MOV BYTE PTR HZ_ADR, 88H        ;第三行起始端口地址
                ADD BX,16                        ;指向第二行信息
                JMP  nextX
DISP_SECX:       MOV BYTE PTR HZ_ADR,90H
nextX:           mov cl,8
continueX:       push cx
                MOV AL,HZ_ADR
                MOV DX, IO_ADDRESS
                OUT DX, AL
                CALL CMD_SETUPX          ;设定DDRAM地址命令
                MOV AX,[BX]
                PUSH AX
                MOV AL,AH               ;先送汉字编码高位
                MOV DX,IO_ADDRESS
                OUT DX,AL
                CALL DATA_SETUPX         ;输出汉字编码高字节
                CALL DELAX              ;延迟
                POP AX
                MOV DX,IO_ADDRESS
                OUT DX, AL
                CALL DATA_SETUPX         ;输出汉字编码低字节
                CALL DELAX
                INC BX
                INC BX                  ;修改显示内码缓冲区指针
                INC BYTE PTR HZ_ADR     ;修改LCD显示端口地址
                POP CX
                DEC CL
                JNZ  continueX
                RET
LCD_DISPX   ENDP

CMD_SETUPX       PROC
                MOV DX,IO_ADDRESS                ;指向8255端口控制端口
                ADD DX,2
                NOP
                MOV AL,00000000B                ;PC1置0,pc0置0 （LCD I端=0，W端＝0）
                OUT DX, AL
                call DELAX
                NOP
                MOV AL,00000100B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call DELAX
                MOV AL, 00000000B               ;PC2置0,（LCD E端置0）
                OUT DX, AL
                call DELAX

                RET
CMD_SETUPX       ENDP

DATA_SETUPX      PROC
                MOV DX,IO_ADDRESS                ;指向8255控制端口
                ADD DX,2
                MOV AL,00000001B                ;PC1置0，PC0=1 （LCD I端=1）
                OUT DX, AL
                NOP
                call DELAX
                MOV AL,00000101B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call DELAX
                MOV AL, 00000001B               ;PC2置0,（LCD E端＝0）
                OUT DX, AL
                NOP
                call DELAX
                RET
DATA_SETUPX      ENDP

DELAX           PROC
                push cx
                push dx
                MOV CX, 0fffh
 x3:           loop   x3
                pop dx
                pop cx
                RET
DELAX           ENDP
        RET
START_X ENDP
              
code ends
     end START_K