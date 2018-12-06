; =============;
; HQFC-A ����  ;
; Ӳ���γ����  ;
; ���⳵�Ƽ���  ;
; =============;

;================================================================
; 8255: CS �� 288H ~ 28FH, PA0 ~ PA7 �� 128x64 Һ������ D7 ~ D0,  
; PB0 �Ӽ����� 3, PC4 ~ PC7 �Ӽ����� 0 ~ 3,                       
; PC0 ��Һ���� D/I ��, PC1 �� RW ��, PC2 �� E �ˡ�                
; ���Է�ʽ��: 88H                                                 
;
; 8254: CS �� 280H ~ 287H, GATE0��GATE1 �� +5V, CLK0 �� 2MHZ, 
; OUT0 �� CLK1, OUT1 �� PB0
;
; 0832: CS �� 290H ~ 297H
;
; ���� 0: ����/ֹͣ
; ���� 1: ����/ҹ��
; ���� 2: ����
; ���� 3: ������(��δʵ��)
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

;�ճ���                 ;  �ո�             ��       ��               0      0     �ո�
HZK_TAB          DW 0A3A0H, 0A3A0H, 0BFD5H, 0B3B5H, 0A3A0H, 0A3B0H, 0A3B0H, 0A3A0H
                 ;  �ո�             �ո�     �ո�    �ո�     �ո�           �ո�
                 DW 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H

;�пͱ�                 ;  �ո�             �ո�     �ո�     �ո�    �ո�    �ո�
HZY_TAB         DW 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H, 0A3A0H
                 ;  �ո�             ��       ��               0      0      �ո� 
                DW 0A3A0H, 0A3A0H, 0D3D0H, 0BFCDH, 0A3A0H, 0A3B0H, 0A3B0H, 0A3A0H 
                
;����۸��       ;  �ո�             ��      ��     �ո�       0       0     �ո�
HZX_TAB         DW 0A3A0H, 0A3A0H, 0B9ABH, 0C0EFH, 0A3A0H, 0A3B0H, 0A3B0H, 0A3A0H
                 ;  �ո�             ��       ��     �ո�      0       0      �ո� 
                DW 0A3A0H, 0A3A0H, 0BCDBH, 0B8F1H, 0A3A0H, 0A3B0H, 0A3B0H, 0A3A0H    

HZ_ADR          DB  ?                   ;�����ʾ����ʼ�˿ڵ�ַ

;���̰�����      ;   0      1     2     3
TABLEN           DB 070H, 0B0H, 0D0H, 0E0H

;�Զ���һջ�ռ�
my_stack        dw 0, 0
DATA            ENDS

code segment
   assume cs:code, ds:data
   
    ; ��ʾ�ճ�
START_K:  MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,88H
                OUT DX,AL                       ;8255��ʼ��
               mov al,0ffh
               mov dx,300H
               out dx, al
               CALL CLEARK              ;LCD ���

                LEA BX,  HZK_TAB
                MOV CH,2                        ;��ʾ��2����Ϣ 
                CALL  LCD_DISPK
                LEA BX, HZK_TAB
                MOV CH,3                  ;    ��ʾ��3����Ϣ
                CALL LCD_DISPK
                
        l1:     jmp     START_1 ;l1
        CLEARK           PROC
                MOV AL,0CH
                MOV DX, IO_ADDRESS
                OUT DX,AL               ;����CLEAR����
                CALL CMD_SETUPK          ;����LCDִ������
                RET
CLEARK           ENDP

FUNCUPK          PROC

                MOV AL, 34H             ;LCD��ʾ״̬����
                OUT DX, AL
                CALL CMD_SETUPK
                RET
FUNCUPK           ENDP

LCD_DISPK        PROC
                LEA BX, HZK_TAB
                CMP CH, 2
                JZ  DISP_SECK
                MOV BYTE PTR HZ_ADR, 88H        ;��������ʼ�˿ڵ�ַ
                ADD BX,16                        ;ָ��ڶ�����Ϣ
                JMP  nextK
DISP_SECK:       MOV BYTE PTR HZ_ADR,90H
nextK:           mov cl,8
continueK:       push cx
                MOV AL,HZ_ADR
                MOV DX, IO_ADDRESS
                OUT DX, AL
                call CMD_SETUPK          ;�趨DDRAM��ַ����
                MOV AX,[BX]
                PUSH AX
                MOV AL,AH               ;���ͺ��ֱ����λ
                MOV DX,IO_ADDRESS
                OUT DX,AL
                call DATA_SETUPK         ;������ֱ�����ֽ�
                call DELAYK              ;�ӳ�
                POP AX
                MOV DX,IO_ADDRESS
                OUT DX, AL
                call DATA_SETUPK         ;������ֱ�����ֽ�
                call DELAYK
                INC BX
                INC BX                  ;�޸���ʾ���뻺����ָ��
                INC BYTE PTR HZ_ADR     ;�޸�LCD��ʾ�˿ڵ�ַ
                POP CX
                DEC CL
                JNZ  continueK
                RET
LCD_DISPK   ENDP

CMD_SETUPK       PROC
                MOV DX,IO_ADDRESS                ;ָ��8255�˿ڿ��ƶ˿�
                ADD DX,2
                NOP
                MOV AL,00000000B                ;PC1��0,pc0��0 ��LCD I��=0��W�ˣ�0��
                OUT DX, AL
                call DELAYK
                NOP
                MOV AL,00000100B                ;PC2��1 ��LCD E�ˣ�1��
                OUT DX, AL
                NOP
                call DELAYK
                MOV AL, 00000000B               ;PC2��0,��LCD E����0��
                OUT DX, AL
                call DELAYK

                RET
CMD_SETUPK       ENDP

DATA_SETUPK      PROC
                MOV DX,IO_ADDRESS                ;ָ��8255���ƶ˿�
                ADD DX,2
                MOV AL,00000001B                ;PC1��0��PC0=1 ��LCD I��=1��
                OUT DX, AL
                NOP
                call DELAYK
                MOV AL,00000101B                ;PC2��1 ��LCD E�ˣ�1��
                OUT DX, AL
                NOP
                call DELAYK
                MOV AL, 00000001B               ;PC2��0,��LCD E�ˣ�0��
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
    pre_START: MOV DX, PORT_0832A                     ; �Ȳ��õ��ת��
    MOV AL, 00H
    OUT DX, AL
    
    mov ax, 00h
    mov bx, 0
    mov my_stack[bx], ax

    MOV AL, 88H         ;8255 ��ʼ�� A �ڷ�ʽ 0 ������� C �����룬�� C �������B �ڷ�ʽ 0 ���
    MOV DX, PORT_CTL
    OUT DX, AL

    ;���У��鿴�Ƿ����м����ɿ�
START_2: MOV AL, 88H         ;8255 ��ʼ�� A �ڷ�ʽ 0 ������� C �����룬�� C �������B �ڷ�ʽ 0 ���
    MOV DX, PORT_CTL
    OUT DX, AL
    MOV DX, PORT_C
WAIT_OPEN: IN AL, DX    ;�� C �ڶ�״̬
    AND AL, 0F0H     ;ֻ��� 4 λ����Ϊ PC7 ~ PC3 �� 0123 ��
    CMP AL, 0F0H     ;�Ƿ�Ϊ 1(�������ɿ�?)
    JNE WAIT_OPEN
    
    ;���������ɿ����ٲ����Ƿ��� 0�����Ƿ��м�ѹ��
WAIT_PRES: IN AL, DX
    AND AL, 0F0H
    CMP AL, 0F0H
    JE WAIT_PRES
    
    ;�м�ѹ�£���ʱ 20 ms, ����
    MOV CX, 16EAH
DELAY_1: LOOP DELAY_1

    ;�ٲ��У������Ƿ��Ա�ѹ��
    IN AL, DX
    AND AL, 0F0H
    CMP AL, 0F0H
    JE WAIT_PRES    ;���ɿ���ת���ȴ�ѹ��״̬
    
    ;���Ա�ѹ�ţ�ȷ���ĸ�����ѹ��(�� PB0 ��������)
    MOV AL, 0FEH
    MOV DX, PORT_B
    OUT DX, AL
    MOV DX, PORT_C
    IN AL, DX
    AND AL, 0F0H
    CMP AL, 0F0H
    JNE ENCODE
    
ENCODE: MOV BX, 0003H   ;������ַָ�룬��ָ�� 3
    IN AL, DX
    AND AL,0F0H         ;ֻȡ����λ
NEXT_TRY: CMP AL, TABLEN[BX]
    JE DONE
    DEC BX
    ;JNS NEXT_TRY
    ;MOV BX, 0003H       ;��ֹ BX ��Խ����
    
DONE: CMP AL, 070H   ;��
    JE JUMP_ST
    CMP AL, 0B0H     ;����
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
    MOV HZX_TAB[BX], AX             ;�������
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
    ;call CONTROL        ;2s ����
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
    JMP START_Y     ;��������ת���п�

ENGINE_OFF: mov bx, 00h
    mov ax, 00h
    mov my_stack[bx], ax
    MOV DX, PORT_0832A
    MOV AL, 00H
    OUT DX, AL
    JMP START_K       ;ֹͣ����ת���ճ�

DAY: mov bx, 02h
    mov ax, 01h
    mov my_stack[bx], ax

    ;��ʼ����
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
    add ax, 0100h                           ;���﹫������һ��
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
    ;add ax, 0100h                           ;���﹫������һ��
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
    add ax, 0100h                           ;���﹫������һ��
    MOV HZX_TAB[BX], AX
    call START_X
    ;call CONTROL           
    JMP START_2                     ;��ת���ȴ�������
   
NIGHT: mov bx, 02h
    mov ax, 00h
    mov my_stack[bx], ax

    ;��ʼ����
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
    add ax, 0100h                           ;���﹫������һ��
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
    add ax, 0100h                           ;���﹫������һ��
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
    add ax, 0100h                           ;���﹫������һ��
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
    add ax, 0100h                           ;���﹫������һ��
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
    add ax, 0100h                           ;���﹫������һ��
    MOV HZX_TAB[BX], AX
    call START_X     
    JMP START_2                     ;��ת���ȴ�������


    
    ;8254 ��ʼ��:��ʱ 2S
CONTROL    PROC 
    MOV AL, 35H    ;ͨ�� 0 ��ʽ�֣��ȵͺ�ߣ���ʽ 2
    MOV DX, PORT_8254_CTL
    OUT DX, AL
    MOV AL, 00H
    MOV DX, PORT_8254_0
    OUT DX, AL
    MOV AL, 50H
    OUT DX, AL

    MOV AL, 77H    ;ͨ�� 1 ��ʽ�֣��ȵͺ�ߣ���ʽ 3
    MOV DX, PORT_8254_CTL
    OUT DX, AL
    MOV AL, 00H
    MOV DX, PORT_8254_1
    OUT DX, AL
    MOV AL, 08H
    OUT DX, AL
    
    ; T1 ����ɵ͵�ƽ���ߵ�ƽ��T2 ����ɸߵ�ƽ���͵�ƽ
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
        
                ;��ʾ�п�
START_Y:    MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,88H
                OUT DX,AL                       ;8255��ʼ��
               mov al,0ffh
               mov dx,300H
               out dx, al
               CALL CLEARY              ;LCD ���

                LEA BX,  HZY_TAB
                MOV CH,2                        ;��ʾ��2����Ϣ 
                CALL  LCD_DISPY
                LEA BX, HZY_TAB
                MOV CH,3                  ;    ��ʾ��3����Ϣ
                CALL LCD_DISPY
        l2:     jmp     START_2 ;l2

CLEARY           PROC
                MOV AL,0CH
                MOV DX, IO_ADDRESS
                OUT DX,AL               ;����CLEAR����
                CALL CMD_SETUPY          ;����LCDִ������
                RET
CLEARY           ENDP

FUNCUPY          PROC

                MOV AL, 34H             ;LCD��ʾ״̬����
                OUT DX, AL
                CALL CMD_SETUPY
                RET
FUNCUPY           ENDP

LCD_DISPY        PROC
                LEA BX, HZY_TAB
                CMP CH, 2
                JZ  DISP_SECY
                MOV BYTE PTR HZ_ADR, 88H        ;��������ʼ�˿ڵ�ַ
                ADD BX,16                        ;ָ��ڶ�����Ϣ
                JMP  nextY
DISP_SECY:       MOV BYTE PTR HZ_ADR,90H
nextY:           mov cl,8
continueY:       push cx
                MOV AL,HZ_ADR
                MOV DX, IO_ADDRESS
                OUT DX, AL
                CALL CMD_SETUPY          ;�趨DDRAM��ַ����
                MOV AX,[BX]
                PUSH AX
                MOV AL,AH               ;���ͺ��ֱ����λ
                MOV DX,IO_ADDRESS
                OUT DX,AL
                CALL DATA_SETUPY         ;������ֱ�����ֽ�
                CALL DELAY              ;�ӳ�
                POP AX
                MOV DX,IO_ADDRESS
                OUT DX, AL
                CALL DATA_SETUPY         ;������ֱ�����ֽ�
                CALL DELAY
                INC BX
                INC BX                  ;�޸���ʾ���뻺����ָ��
                INC BYTE PTR HZ_ADR     ;�޸�LCD��ʾ�˿ڵ�ַ
                POP CX
                DEC CL
                JNZ  continueY
                RET
LCD_DISPY   ENDP

CMD_SETUPY       PROC
                MOV DX,IO_ADDRESS                ;ָ��8255�˿ڿ��ƶ˿�
                ADD DX,2
                NOP
                MOV AL,00000000B                ;PC1��0,pc0��0 ��LCD I��=0��W�ˣ�0��
                OUT DX, AL
                call DELAY
                NOP
                MOV AL,00000100B                ;PC2��1 ��LCD E�ˣ�1��
                OUT DX, AL
                NOP
                call DELAY
                MOV AL, 00000000B               ;PC2��0,��LCD E����0��
                OUT DX, AL
                call DELAY

                RET
CMD_SETUPY       ENDP

DATA_SETUPY      PROC
                MOV DX,IO_ADDRESS                ;ָ��8255���ƶ˿�
                ADD DX,2
                MOV AL,00000001B                ;PC1��0��PC0=1 ��LCD I��=1��
                OUT DX, AL
                NOP
                call DELAY
                MOV AL,00000101B                ;PC2��1 ��LCD E�ˣ�1��
                OUT DX, AL
                NOP
                call DELAY
                MOV AL, 00000001B               ;PC2��0,��LCD E�ˣ�0��
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

                ;��ʾ�п�
START_X    PROC
               MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,88H
                OUT DX,AL                       ;8255��ʼ��
               mov al,0ffh
               mov dx,300H
               out dx, al
               CALL CLEARX            ;LCD ���

                LEA BX,  HZX_TAB
                MOV CH,2                        ;��ʾ��2����Ϣ 
                CALL  LCD_DISPX
                LEA BX, HZX_TAB
                MOV CH,3                  ;    ��ʾ��3����Ϣ
                CALL LCD_DISPX
        ;l3:     jmp     START_X ;l3

CLEARX           PROC
                MOV AL,0CH
                MOV DX, IO_ADDRESS
                OUT DX,AL               ;����CLEAR����
                CALL CMD_SETUPX          ;����LCDִ������
                RET
CLEARX           ENDP

FUNCUPX          PROC

                MOV AL, 34H             ;LCD��ʾ״̬����
                OUT DX, AL
                CALL CMD_SETUPX
                RET
FUNCUPX           ENDP

LCD_DISPX        PROC
                LEA BX, HZX_TAB
                CMP CH, 2
                JZ  DISP_SECX
                MOV BYTE PTR HZ_ADR, 88H        ;��������ʼ�˿ڵ�ַ
                ADD BX,16                        ;ָ��ڶ�����Ϣ
                JMP  nextX
DISP_SECX:       MOV BYTE PTR HZ_ADR,90H
nextX:           mov cl,8
continueX:       push cx
                MOV AL,HZ_ADR
                MOV DX, IO_ADDRESS
                OUT DX, AL
                CALL CMD_SETUPX          ;�趨DDRAM��ַ����
                MOV AX,[BX]
                PUSH AX
                MOV AL,AH               ;���ͺ��ֱ����λ
                MOV DX,IO_ADDRESS
                OUT DX,AL
                CALL DATA_SETUPX         ;������ֱ�����ֽ�
                CALL DELAX              ;�ӳ�
                POP AX
                MOV DX,IO_ADDRESS
                OUT DX, AL
                CALL DATA_SETUPX         ;������ֱ�����ֽ�
                CALL DELAX
                INC BX
                INC BX                  ;�޸���ʾ���뻺����ָ��
                INC BYTE PTR HZ_ADR     ;�޸�LCD��ʾ�˿ڵ�ַ
                POP CX
                DEC CL
                JNZ  continueX
                RET
LCD_DISPX   ENDP

CMD_SETUPX       PROC
                MOV DX,IO_ADDRESS                ;ָ��8255�˿ڿ��ƶ˿�
                ADD DX,2
                NOP
                MOV AL,00000000B                ;PC1��0,pc0��0 ��LCD I��=0��W�ˣ�0��
                OUT DX, AL
                call DELAX
                NOP
                MOV AL,00000100B                ;PC2��1 ��LCD E�ˣ�1��
                OUT DX, AL
                NOP
                call DELAX
                MOV AL, 00000000B               ;PC2��0,��LCD E����0��
                OUT DX, AL
                call DELAX

                RET
CMD_SETUPX       ENDP

DATA_SETUPX      PROC
                MOV DX,IO_ADDRESS                ;ָ��8255���ƶ˿�
                ADD DX,2
                MOV AL,00000001B                ;PC1��0��PC0=1 ��LCD I��=1��
                OUT DX, AL
                NOP
                call DELAX
                MOV AL,00000101B                ;PC2��1 ��LCD E�ˣ�1��
                OUT DX, AL
                NOP
                call DELAX
                MOV AL, 00000001B               ;PC2��0,��LCD E�ˣ�0��
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