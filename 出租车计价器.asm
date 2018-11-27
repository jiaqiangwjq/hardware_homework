; =========== ;
; HQFC-A 机箱 ;
; 硬件课程设计 ;
; 出租车计价器 ;
; =========== ;

;===============================================
; 8255: CS 接 288H ~ 28FH, PA0 ~ PA7 接 128x64 液晶屏的 D7 ~ D0, PB0 接键盘行 3, PC4 ~ PC7 接键盘列 0 ~ 3,
; PC0 接液晶屏 D/I 端, PC1 接 RW 端, PC2 接 E 端。所以方式字: 88H
;
; 8254: CS 接 280H ~ 287H, GATE0、GATE1 接 +5V, 
;
;
;
;
;
;
;================================================


IO_ADDRESS      equ  288h

DATA            SEGMENT
HZ_TAB          DW 0A3B3H,0A3B2H,0CEBBH,0CEA2H,0BBFAH,0BDCCH,0D1A7H,0CAB5H
                DW 0D1E9H,0CFB5H,0CDB3H,0D5FDH,0D4DAH,0D1DDH,0CABEH,0D6D0H                 
HZ_ADR          DB  ?                   ;存放显示行起始端口地址

DATA            ENDS

code segment
   assume cs:code,ds:data
START:  MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,80H
                OUT DX,AL                       ;8255初始化
               mov al,0ffh
               mov dx,300H
               out dx, al
                CALL CLEAR              ;LCD 清除
          ;     CALL FUNCUP              ;LCD 功能设置
                LEA BX,  HZ_TAB
                MOV CH,2                        ;显示第2行信息 
                CALL  LCD_DISP
                LEA BX, HZ_TAB
                MOV CH,3                  ;    显示第3行信息
                CALL LCD_DISP
        l1:     jmp     start ;l1

        CLEAR           PROC
                MOV AL,0CH
                MOV DX, IO_ADDRESS
                OUT DX,AL               ;设置CLEAR命令
                CALL CMD_SETUP          ;启动LCD执行命令
                RET
CLEAR           ENDP

FUNCUP          PROC
         ;      MOV AL, 0fH             ;LCD功能设置命令
         ;      OUT DX, AL
         ;      CALL CMD_SETUP
                MOV AL, 34H             ;LCD显示状态命令
                OUT DX, AL
                CALL CMD_SETUP
                RET
FUNCUP           ENDP

LCD_DISP        PROC
                LEA BX, HZ_TAB
                CMP CH, 2
                JZ  DISP_SEC
                MOV BYTE PTR HZ_ADR, 88H        ;第三行起始端口地址
                ADD BX,16                        ;指向第二行信息
                JMP  next
DISP_SEC:       MOV BYTE PTR HZ_ADR,90H
next:           mov cl,8
continue:       push cx
                MOV AL,HZ_ADR
                MOV DX, IO_ADDRESS
                OUT DX, AL
                CALL CMD_SETUP          ;设定DDRAM地址命令
                MOV AX,[BX]
                PUSH AX
                MOV AL,AH               ;先送汉字编码高位
                MOV DX,IO_ADDRESS
                OUT DX,AL
                CALL DATA_SETUP         ;输出汉字编码高字节
                CALL DELAY              ;延迟
                POP AX
                MOV DX,IO_ADDRESS
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
LCD_DISP   ENDP

CMD_SETUP       PROC
                MOV DX,IO_ADDRESS                ;指向8255端口控制端口
                ADD DX,2
                NOP
                MOV AL,00000000B                ;PC1置0,pc0置0 （LCD I端=0，W端＝0）
                OUT DX, AL
                call delay
                NOP
                MOV AL,00000100B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call delay
                MOV AL, 00000000B               ;PC2置0,（LCD E端置0）
                OUT DX, AL
                call delay

                RET
CMD_SETUP       ENDP

DATA_SETUP      PROC
                MOV DX,IO_ADDRESS                ;指向8255控制端口
                ADD DX,2
                MOV AL,00000001B                ;PC1置0，PC0=1 （LCD I端=1）
                OUT DX, AL
                NOP
                call delay
                MOV AL,00000101B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call delay
                MOV AL, 00000001B               ;PC2置0,（LCD E端＝0）
                OUT DX, AL
                NOP
                call delay
                RET
DATA_SETUP      ENDP

DELAY           PROC
                push cx
                push dx
                MOV CX, 0fffh
 x1:           loop   x1
                pop dx
                pop cx
                RET
DELAY           ENDP


code ends
     end start