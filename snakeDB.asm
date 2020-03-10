;wasd控制前进方向，esc退出，r重启，p暂停

;----------------------------------------------------------------宏&常数--------------------------------------------------------------------------
%define esc 1bh			;esc按键的ascii
snake_color EQU  00000101B		;蛇身颜色	
snake_head_color EQU  00001101B	;蛇头颜色	

displayBase EQU  0b800h		;显示区基地址	
PORT_KEY_DAT EQU 0x60		;键盘数据接口
PORT_KEY_STA EQU 0x64		;键盘控制接口

KEYBUFF_DS  	EQU 0040H	;键盘缓冲段地址
KEYBUFF_HEAD 	EQU 001AH	;键盘缓冲头指针偏移地址（0040:001AH）
KEYBUFF_TAIL 	EQU 001CH	;键盘缓冲尾指针偏移地址（0040:001CH）
KEYBUFF_FIRST 	EQU 001EH	;键盘缓冲首地址	（0040:001EH,左闭）
KEYBUFF_LAST 	EQU 003EH	;键盘缓冲尾地址	（0040:003EH,右闭）

%define HDIRBUFF_DS  	es		;hdir缓冲段地址
%define HDIRBUFF_HEAD 	hdir_head		;hdir缓冲头指针偏移地址
%define HDIRBUFF_TAIL 	hdir_tail		;hdir缓冲尾指针偏移地址
%define HDIRBUFF_FIRST 	hdir_buff		;hdir缓冲首地址	
%define HDIRBUFF_LAST  	hdir_buff+2*20	;hdir缓冲尾地址	

%define TDIRBUFF_DS  	es		;tdir缓冲段地址
%define TDIRBUFF_HEAD 	tdir_head		;tdir缓冲头指针偏移地址
%define TDIRBUFF_TAIL 	tdir_tail		;tdir缓冲尾指针偏移地址
%define TDIRBUFF_FIRST 	tdir_buff		;tdir缓冲首地址	
%define TDIRBUFF_LAST  	tdir_buff+2*80	;tdir缓冲尾地址

%define incW(x)   	inc word [es:x]	;宏函数，字x加1
%define clearW(x)  	mov  [es:x],word 0	;宏函数，字x清0
%define setW(x,y)  	mov  [es:x],word y	;宏函数，字x设为y
%define incB(x)   	inc byte [es:x]	;宏函数，字节x加1
%define decB(x)   	dec byte [es:x]	;宏函数，字节x加1
%define clearB(x)  	mov  [es:x],byte 0	;宏函数，字节x清0
%define setB(x,y)  	mov  [es:x],byte y	;宏函数，字节x设为y
%define notB(x) 	NOTB x		;宏函数，bool变量x取反（x只能是0或1）	

%macro NOTB 1			;宏函数，bool变量取反
	cmp byte [es:%1],0
	jnz ISONE
	setB(%1,1)
	jmp NOTDONE
ISONE:
	clearB(%1)
NOTDONE:
%endmacro

%define setDri(x,y) mov [es:x+2],word y	;宏函数，设置x方向为y(x必须是snake_head或snake_tail)
%define getDir(x)	word [es:x+2]	;宏函数，获取x方向(x必须是snake_head或snake_tail)

%define lineAddr(x) displayBase+x*0ah	;宏函数，获取屏幕x行首地址，x必须是常数
%define getXY(x,y) (x<<8) | y		;宏函数，屏幕上坐标（x,y）拼成一个字，xy必须是常数

%macro showFrames 1		;宏函数，按参数颜色刷新边框
	mov ax,%1	
	push ax	
	call show_all_frames
	add sp,2	;平衡堆栈
%endmacro

%macro setDS 1			;宏函数，设置ds寄存器为参数值
	push ax
	mov ax,%1
	mov ds,ax
	pop ax	
%endmacro

%macro reCount 0 			;宏函数，计数器重装载
	push ax
	mov al,[es:recount]	
	mov [es:count],al
	pop ax	
%endmacro

%macro setAddrHT 1  	;宏函数，设置ds和bx，使[ds:bx]指向某点，参数必须是snake_head 或 snake_tail 或 某字存储空间
	push ax
	push dx
	
	mov bx,[es:%1]        ;bh是列号，bl是行号		
	xor ax,ax		;行号乘0ah找到行首地址偏移
	mov al,bl
	mov dl,0ah
	mul dl

	add ax,displayBase	;加上显示区基地址得到行地址，给ds
	mov ds,ax

	mov bl,bh
	xor bh,bh		;清bh，bx存列号
	shl bx,1		;列号乘2得到点在列中的偏移地址
	
	pop dx
	pop ax
%endmacro

%macro printStr 2  		;宏函数，在%2位置打印字符串%1（参数必须是score_str，score_str_pos 或 replay_str，replay_str_pos）
	push si		;此函数需要手动保存和恢复ds:bx !

	mov si,%1
	setAddrHT %2
	call show_str

	pop si
%endmacro

%macro printDieInfo 0  	;宏函数，打印死亡提示信息
	push ds
	push bx
	printStr score_str, score_str_pos
	mov ax,[es:score]
	call print_num
	printStr replay_str, replay_str_pos
	printStr exit_str, exit_str_pos
	pop bx
	pop ds	
%endmacro
	

;----------------------------------------------------------------主函数--------------------------------------------------------------------------
segment code 
      	org 100H	
	 
;=================设置段寄存器=========================
	mov ax,cs			;cs,ds一样，不分段
	mov ds, ax		
		
	mov ax,0			;es指向0，准备更改9号中断向量表指向自定义的处理函数(如果es值不对，效果上没啥变化，会调用默认的9号中断服务程序)
	mov es,ax

;=================准备中断向量表=========================
	push word [es:9*4]		;将原来的int 9中断例程的入口地址保存在data段
	pop word  [data]
	push word [es:9*4+2]
	pop word  [data+2]

	push word [es:1ch*4]	;将原来的int 1ch中断例程的入口地址保存在data段
	pop word  [data+4]
	push word [es:1ch*4+2]
	pop word  [data+6]

	CLI
	mov word [es:9*4], int09h_handler	;在中断向量表中设置新的int 9中断例程的入口地址（设置过程关闭中断，避免被打断）
	mov [es:9*4+2], cs
 	mov word [es:1ch*4], int1ch_handler	;在中断向量表中设置新的int 1ch中断例程的入口地址（设置过程关闭中断，避免被打断）
	mov [es:1ch*4+2], cs
	STI	
	
	mov ax,cs		
	mov es,ax			;程序运行过程中用es来寻址全局变量	

	mov ax,[es:stack]
	mov ss,ax			;ss指向数据区的stack
	
;================游戏初始化===============================
game_init:
	;初始化全局变量
	clearW(score)		;得分
	;setB(spd,'6')		;速度选择
	;setB(recount,5)		;定时器重装载值	
	;setB(count,1)		;定时器计数值
	setW(snake_head,getXY(7,8))	;蛇头信息
	setW(snake_head+2,'d')
	setW(snake_tail,getXY(5,8))	;蛇尾信息
	setW(snake_tail+2,'d')	
	clearB(die_flag)		;复位各标志
	clearB(get_food_flag)
	clearB(pause_flag)	

	;每次重启游戏，改变边框和提示信息的颜色
	incB(frame_color)			;边框frame_color前景色+1（00001001~00001111循环）
	cmp byte[es:frame_color],00001111b
	jbe frame_color_seted
	setB(frame_color,00001001b)
frame_color_seted:				;提示信息info_color（00001111~00001001循环）	
	mov al,24			
	sub al,[es:frame_color]	
	mov [es:info_color],al

	;初始化窗口	
	call clear_window

	;用frame_color颜色显示所有边框 
	showFrames [es:frame_color]

	;初始化蛇
	call snake_init
	
	;初始化食物
	call draw_food	

	;初始化循环队列
	call Queue_clear


;=============主循环中进行设置键和死亡检测========================
key_and_death:	
	cmp byte [es:die_flag],0	;检查死亡情况
	jnz DIE			;死了，转移

	mov ah, 01h		;查询按键缓冲是否为空
	int 16h
	jnz KEYCHEAK		;不空，检查按键
	jmp key_and_death		;空，循环检查死亡情况
	
KEYCHEAK:
	mov ah,0			;取一个键
	int 16h
R:				;r键，用来重启游戏
	cmp al,'r'
	jnz P
	jmp game_init
P:
	cmp al,'p'
	jnz SPD
	
	notB(pause_flag)
	jmp key_and_death
SPD:				;F1~F10，速度设置
	cmp al,'0'
	jb ESC
	cmp al,'9'
	ja ESC
	
	;刷新显示速度
	setB(spd,al)
	call show_score_and_spd		

	;更新recount值
	sub al,'0'
	xor bx,bx
	mov bl,al	
	mov al,[es:speed+bx]
	setB(recount,al)

	jmp key_and_death
ESC:
	cmp al,esc 		;esc键，用来退出游戏
	jz program_end
	;showFrames 02h

	jmp key_and_death

;=========================死亡界面=========================
DIE:	
	call clear_window		;刷新窗口
	showFrames [es:frame_color]

	printDieInfo		;打印提示信息

restart_or_exit:
	mov ah,0			;取一个键
	int 16h
restart_cheak:			;r键，用来重启游戏
	cmp al,'r'
	jnz exit_cheak
	jmp game_init
exit_cheak:
	cmp al,esc 		;esc键，用来退出户游戏
	jz program_end
	jmp restart_cheak
	
;=========================程序出口=========================
program_end:
	;恢复int 9和int 1ch中断例程
	mov ax,cs
	mov ds,ax
	mov ax,0
	mov es,ax
	push word [data]
	pop word [es:9*4]
	push word [data+2]
	pop word [es:9*4+2]
	
	push word [data+4]
	pop word [es:1ch*4]
	push word [data+6]
	pop word [es:1ch*4+2]

	;返回dos
	mov ax,4c00h
	int 21h




;----------------------------------------------------------------时钟相关-------------------------------------------------------------------------
;功能:自定义的1ch号定时器中断
;参数:无
;返回:无
int1ch_handler:
	cmp byte [es:die_flag],0	;死了，不要刷新蛇（不能把暂停放在前面，否则暂停的时候就不能退出或者重启了）
	jnz RETURN

	cmp byte [es:pause_flag],0	;暂停状态，不刷新
	jnz RETURN

	dec byte [es:count]
	JZ FLUSH			;计数减到0时刷新蛇位置
	jmp RETURN
FLUSH:
	reCount			;重装载计数器
	call snake_head_flush	;刷新蛇头
	
	cmp byte[es:get_food_flag],1	;吃到食物，这一轮蛇尾不要动
	jz RETURN
	call snake_tail_flush		;没吃到，刷新蛇尾
RETURN:	
	mov al,20h		;通知中断控制器8259A
	out 20h,al			;当前中断处理已经结束
	iret			;中断返回

;----------------------------------------------------------------蛇显示相关--------------------------------------------------------------------------
;功能:初始化显示蛇
;参数:无
;返回:无
snake_init:
	push ax
	push bx	
	push dx
	push cx
	
	setAddrHT snake_head	;使[ds:bx]指向蛇头点

	mov ah,snake_head_color	;画蛇头
	mov al,2ah		;*号
	mov [ds:bx],ax		
	sub bx,2			

	mov ah,snake_color		;从蛇头开始向左画2格，代表初始蛇
	mov cx,2			
PRINTSNAKE:	
	mov [ds:bx],ax	
	sub bx,2
	loop PRINTSNAKE
	
	pop cx
	pop dx
	pop bx
	pop ax
	ret

;功能：刷新显示蛇头（根据HDIRBUFF移动蛇头，画*）
;参数：无
;返回：设置全局变量get_food_flag和die_flag
snake_head_flush:
	push ax
	push bx
	push ds

	mov si,[es:HDIRBUFF_HEAD]	;如果hdir缓冲为空，不用改snake_head
	cmp si,[es:HDIRBUFF_TAIL]	
	jz DIRSETTED_H		
				
	mov si,[es:HDIRBUFF_HEAD]	;如果hdir缓冲非空，取队首的键数据
	mov ax,[es:si]		;这个es前缀千万别忘了（bp/sp缺省ss，其他缺省ds）
	
;==========判断当前操作是否合法（不能掉头）重设snake_head=============
GET_W:	
	cmp al,'w'
	jnz GET_A
	cmp getDir(snake_head),'s'	;不能直接调头
	jz POP_hdir
	jmp RESET_HEAD
GET_A:
	cmp al,'a'
	jnz GET_S
	cmp getDir(snake_head),'d'	;不能直接调头
	jz POP_hdir
	jmp RESET_HEAD
GET_S:
	cmp al,'s'
	jnz GET_D
	cmp getDir(snake_head),'w'	;不能直接调头
	jz POP_hdir
	jmp RESET_HEAD
GET_D:
	cmp getDir(snake_head),'a'	;不能直接调头
	jz POP_hdir	

RESET_HEAD:
	xor ah,ah
	setDri(snake_head,ax)	;重设蛇头方向

	mov dx,[es:snake_head]	;此刻snake_head高字存储转向点，ax存储将要转向的方向，在此对TDIRBUFF进行入队
	call Enqueue_tdir

POP_hdir:
	call Dequeue_hdir		;队首元素出队

;=======至此snake_head中方向数据已经更新或保持，下面找到新的蛇头位置=================

DIRSETTED_H:
	mov dx,[es:snake_head]	;取得蛇头数据中“点位置”部分数据，根据前进方向进行修改
MOVE_W:
	cmp getDir(snake_head),'w'
	jnz MOVE_A
	dec dl
	jmp MOVE_DONE
MOVE_A:
	cmp getDir(snake_head),'a'
	jnz MOVE_S
	dec dh
	jmp MOVE_DONE
MOVE_S:
	cmp getDir(snake_head),'s'
	jnz MOVE_D
	inc dl
	jmp MOVE_DONE
MOVE_D:
	inc dh	

;=======至此dx已经存储了新蛇头位置，更新snake_head画新蛇头，并进行新食物生成=================
MOVE_DONE:
	clearB(get_food_flag)	;复位get_food_flag	
	
	setAddrHT snake_head	;修改原蛇头颜色为蛇身颜色
	mov ah,snake_color		;颜色
	mov [ds:bx+1],ah			
	
	mov [es:snake_head],dx	;更新snake_head
	setAddrHT snake_head	;设置ds:bx为蛇头点地址
	
	cmp byte[ds:bx],'#'		;如果蛇头撞到了非#或空格的点，而且现在蛇头和蛇尾不重合，那要么是撞到蛇身，要么是撞墙，设die标志
	jz DIECHEAKED
	cmp byte	[ds:bx],' '	
	jz DIECHEAKED
	mov ax,[es:snake_head]
	cmp ax,[es:snake_tail]
	jz DIECHEAKED
	setB(die_flag,1)

DIECHEAKED:
	cmp byte [ds:bx],'#'		;比较蛇头位置和当前食物位置
	jnz DRAWHEAD		;如果没吃到，转DRAWHEAD

	setB(get_food_flag,1)	;吃到了，设标志get_food_flag
	call draw_food		;画新食物	
	add word[es:score],10	;得分增加
	call show_score_and_spd	;刷新得分显示
DRAWHEAD:
	mov ah,snake_head_color	;颜色
	mov al,2ah		;*号
	mov [ds:bx],ax		;画新蛇头	
	
	pop ds
	pop bx
	pop ax	
	ret	


;功能：刷新显示蛇尾(先画空格刷掉蛇身，再根据TDIRBUFF移动蛇尾)
;参数：无
;返回：无
snake_tail_flush:
	push ax
	push bx
	
	push ds			;清原蛇尾点
	setAddrHT snake_tail	;设置ds:bx为蛇尾点地址
	mov ah,1
	mov al,' '			;显示一个空格
	mov [ds:bx],ax		
	pop ds

	mov si,[es:TDIRBUFF_HEAD]	;如果tdir缓冲为空，不用改snake_tail
	cmp si,[es:TDIRBUFF_TAIL]	
	jz NOTURN		
				
	mov si,[es:TDIRBUFF_HEAD]	;如果tdir缓冲非空，取队首的键数据到BX:AX
	mov bx,[es:si]		;这个es前缀千万别忘了（bp/sp缺省ss，其他缺省ds）
	mov ax,[es:si+2]	

	cmp bx,[es:snake_tail]	;判断snake_tail中位置（高字），如果蛇尾到达转向点了，就修改snake_tail中的方向参数（低字）
	jnz NOTURN
	
	setDri(snake_tail,ax)	;修改蛇尾	
	call Dequeue_tdir		;出队

NOTURN:
	mov bx,[es:snake_tail]	;取得蛇尾数据中“点位置”部分数据，根据前进方向进行修改
MOVE_W_T:
	cmp getDir(snake_tail),'w'
	jnz MOVE_A_T
	dec bl
	jmp MOVE_DONE_T
MOVE_A_T:
	cmp getDir(snake_tail),'a'
	jnz MOVE_S_T
	dec bh
	jmp MOVE_DONE_T
MOVE_S_T:
	cmp getDir(snake_tail),'s'
	jnz MOVE_D_T
	inc bl
	jmp MOVE_DONE_T
MOVE_D_T:
	inc bh	
MOVE_DONE_T:
	mov [es:snake_tail],bx	;更新snake_tail

	pop bx
	pop ax
	ret

;----------------------------------------------------------------键盘输入相关--------------------------------------------------------------------------
;功能:自定义的9号键盘中断
;参数:无
;返回:无
int09h_handler:
	pusha			;保护通用reg

	mov al,0adh
	out PORT_KEY_STA,al	;禁止键盘发送数据到接口（准备接受键盘发送到接口的数据）
	
	in al, PORT_KEY_DAT	;读取键盘发来的按键扫描码
	
	sti			;开中断
	call int09h_fun		;完成相关功能
	cli			;关中断

	mov al,0aeh		;允许发送数据到接口
	out PORT_KEY_STA,al	

	mov al,20h		;通知中断控制器8259A
	out 20h,al			;当前中断处理已经结束

	popa			;恢复通用reg
	iret			;中断返回

;功能:自定义的9号键盘中断功能函数
;参数:无
;返回:无
int09h_fun:

CHEAK_ESC:	
	cmp al, 81h		;esc松开
	jnz CHEAK_R
	
	mov ah,al
	mov al,esc 	
	jmp Save2keyBuff	
	
CHEAK_R:
	cmp al,13h		;r按下
	jnz CHEAK_P
	
	mov ah,al
	mov al,'r' 	
	jmp Save2keyBuff

CHEAK_P:
	;如果是死亡状态，只检测 r/esc（避免入队多余的键）
	cmp byte [es:die_flag],0
	jnz int9_DONE

	cmp al,19h		;p按下
	jnz CHEAK_W

	mov ah,[es:pause_flag]	;注意这里，确保每一次按下P后存入keyBuff的ax都不同，否则不能入队（这样处理可以避免按住p时重复入队）
	mov al,'p' 	
	jmp Save2keyBuff

CHEAK_W:

	;如果是暂停状态，只检测 r/esc/p（避免入队多余的键）
	cmp byte [es:pause_flag],0
	jnz int9_DONE

	cmp al,11h	;w按下
	jnz CHEAK_A
	
	mov ah,al
	mov al,'w' 		
	jmp Save2dirBuff
CHEAK_A:
	cmp al,1eh	;a按下
	jnz CHEAK_S

	mov ah,al
	mov al,'a' 	
	jmp Save2dirBuff	
CHEAK_S:
	cmp al,1fh	;s按下
	jnz CHEAK_D
	
	mov ah,al
	mov al,'s' 	
	jmp Save2dirBuff
CHEAK_D:
	cmp al,20h	;d按下
	jnz CHEAK_SPD
	
	mov ah,al
	mov al,'d' 	
	jmp Save2dirBuff


CHEAK_SPD:		;F1~F10 (键值'0'~'9')
	cmp al,3bh
	jb int9_DONE
	cmp al,44h
	ja int9_DONE

	mov ah,al
	sub al,3bh	;从scan code转ascII
	add al,'0'
	jmp Save2keyBuff
Save2dirBuff:
	call Enqueue_hdir	;存入hdir缓冲，每个字数据高字节是扫描码，低字节是ascII
	jmp int9_DONE
Save2keyBuff:
	call Enqueue_key
int9_DONE:
	ret	

;功能:把设置按键数据存入环形队列缓冲区
;参数:ax
;返回:无
Enqueue_key:
	push ds		
	push bx		

	setDS KEYBUFF_DS		;缓冲区段地址 		
				
	mov bx,[KEYBUFF_TAIL]	;取队列的尾指针 		
	mov si,bx			;si=队列尾指针		
	add si,2			;si指向下一个可能的位置
	cmp si,KEYBUFF_LAST	;越出缓冲区吗？		
	jb EN_OK1		;没有越界，转移
	mov si,KEYBUFF_FIRST	;越界了，循环到缓冲区头部 
EN_OK1:
	cmp si,[KEYBUFF_HEAD]	;和队列头指针比较		
	jz EnqueueDONE1		;相等表示缓冲已满，不再存储数据
	cmp [bx-2],ax
	jz EnqueueDONE_h		;如果按键和上次一样，也不存了，这样可以避免按住不放时重复存入按键，操作更灵敏			
	
	mov [bx],ax		;按键数据存入队列	
	mov [KEYBUFF_TAIL],si	;保存队列尾指针
EnqueueDONE1:
	
	pop bx
	pop ds
	ret

;功能:把hdir按键数据存入环形队列缓冲区
;参数:ax存储要保存的数据
;返回:无
Enqueue_hdir:
	push ds		
	push bx		

	setDS HDIRBUFF_DS	;缓冲区段地址 		
				
	mov bx,[HDIRBUFF_TAIL]	;取队列的尾指针 		
	mov si,bx		                ;si=队列尾指针		
	add si,2			;si指向下一个可能的位置
	cmp si,HDIRBUFF_LAST 	;越出缓冲区吗？		
	jb EN_OK_h		;没有越界，转移
	mov si,HDIRBUFF_FIRST	;越界了，循环到缓冲区头部 
EN_OK_h:
	cmp si,[HDIRBUFF_HEAD]	;和队列头指针比较		
	jz EnqueueDONE_h		;相等表示缓冲已满，不再存储数据
	cmp [bx-2],ax
	jz EnqueueDONE_h		;如果按键和上次一样，也不存了，这样可以避免按住不放时重复存入按键，操作更灵敏

	mov [bx],ax		;按键数据存入队列	
	mov [HDIRBUFF_TAIL],si	;保存队列尾指针
EnqueueDONE_h:

	pop bx
	pop ds
	ret

;功能:hdir环形队列出队一个（队首指针后移）
;参数:无
;返回:无
Dequeue_hdir:
	push ds		

	setDS HDIRBUFF_DS
	mov si,[HDIRBUFF_HEAD]
	add si,2
	cmp si,HDIRBUFF_LAST 
	jb .LABh
	mov si,HDIRBUFF_FIRST
.LABh:
	mov [HDIRBUFF_HEAD],si
	pop ds
	ret	


;功能:把tdir按键数据存入环形队列缓冲区
;参数: DX:AX存储要保存的数据
;返回:无
Enqueue_tdir:
	push ds			
	push bx			

	setDS TDIRBUFF_DS	;缓冲区段地址 		
				
	mov bx,[TDIRBUFF_TAIL]	;取队列的尾指针 		
	mov si,bx		                ;si=队列尾指针		
	add si,4			;si指向下一个可能的位置
	cmp si,TDIRBUFF_LAST 	;越出缓冲区吗？		
	jb EN_OK_t		;没有越界，转移
	mov si,TDIRBUFF_FIRST	;越界了，循环到缓冲区头部 
EN_OK_t:
	cmp si,[TDIRBUFF_HEAD]	;和队列头指针比较		
	jz EnqueueDONE_t		;相等表示缓冲已满，不再存储数据

	mov [bx],dx		;按键数据存入队列	
	mov [bx+2],ax
	mov [TDIRBUFF_TAIL],si	;保存队列尾指针
EnqueueDONE_t:

	pop bx
	pop ds
	ret

;功能:tdir环形队列出队一个（队首指针后移）
;参数:无
;返回:无
Dequeue_tdir:
	push ds		

	setDS TDIRBUFF_DS
	mov si,[TDIRBUFF_HEAD]
	add si,4
	cmp si,TDIRBUFF_LAST 
	jb .LABt
	mov si,TDIRBUFF_FIRST
.LABt:
	mov [TDIRBUFF_HEAD],si
	pop ds
	ret	

;功能:清空所有循环队列
;参数:无
;返回:无
Queue_clear:
	push ax

	push es
	mov ax,KEYBUFF_DS 
	mov es,ax
	mov ax,KEYBUFF_FIRST
	mov [es:KEYBUFF_HEAD],ax
	mov [es:KEYBUFF_TAIL],ax
	
	pop es
	mov ax,HDIRBUFF_FIRST
	mov [es:HDIRBUFF_HEAD],ax
	mov [es:HDIRBUFF_TAIL],ax
	
	mov ax,TDIRBUFF_FIRST
	mov [es:TDIRBUFF_HEAD],ax
	mov [es:TDIRBUFF_TAIL],ax	

	pop ax
	ret


;----------------------------------------------------------------------边框显示相关--------------------------------------------------------------------------
;功能:初始化窗口（清屏）
;参数:无
;返回:无
clear_window:
	push ax
	push ds
	push cx
	push bx
	 
	setDS lineAddr(0)		;ds指向第1行起始
	mov ah,00000111B		;屏幕所有点清成空格
	mov al,' '
	mov cx,25		;0~24行（最后一次循环指针已经指向25行了，但没赋值）
CLEARWINOW:
	push cx			;保护外层循环计数
	mov cx,80
	mov bx,0	

CLEARLINE:	
	mov word [ds:bx],ax
	add bx,2			;最右边的
	loop CLEARLINE

	pop cx			;恢复外层循环计数
	mov bx,ds		;转向下一行
	add bx,0ah
	mov ds,bx
	loop CLEARWINOW

	pop bx
	pop cx
	pop ds
	pop ax	
	ret

;功能:显示所有边框
;参数: 堆栈一个字低字节存颜色
;返回: 无
show_all_frames:
	push bp
	mov bp,sp

	push ds
	push bx
	push cx
	push ax

	;显示横向边框
	setDS lineAddr(0)	;ds指向第0行起始
	mov bx,0		;偏移初始化为0	
	mov ah,[bp+4]	;颜色
	mov al,2ah	;*号

	mov cx,80	;ds开始80个字（正好一行）填入边框字符
show_up_frame:
	mov [ds:bx],ax	
	add bx,2		
	loop show_up_frame

	setDS  lineAddr(24)	;ds指向第24行起始
	mov bx,0		;偏移初始化为0

	mov cx,80	;ds开始80个字（正好一行）填入边框字符
show_down_frame:
	mov ah,[bp+4]	;颜色
	mov al,2ah	;*号
	mov [ds:bx],ax	
	add bx,2		
	loop show_down_frame

	;显示纵向边框
	setDS lineAddr(1)  	;ds指向第24行起始	
	mov cx,23	;1~23行（最后一次循环指针已经指向24行了，但没赋值）
	mov ah,[bp+4]	;颜色
	mov al,2ah	;*号
show_lengthwise_frame:
	mov bx,0		;最左边的
	mov [ds:bx],ax
	add bx,160-2	;最右边的
	mov [ds:bx],ax
	mov bx,ds	;转向下一行
	add bx,0ah
	mov ds,bx
	loop show_lengthwise_frame
	
	call show_score_and_spd		;显示得分	

	pop ax
	pop cx
	pop bx
	pop ds

	pop bp
	ret

;功能:显示得分和当前设置的速度
;参数: 数据段score，spd
;返回: 无
show_score_and_spd:
	push bp
	mov bp,sp

	push ds
	push ax
	push bx	
	push cx	
	push dx

	
        	;清空score显示位置
	setDS lineAddr(0)		
	mov bx,68		
	mov ah,[es:info_color]	;颜色
	mov al,'-' 			;清成'-'（由于判断撞墙的机制，不可以清成空格）	
	mov cx,7			;清7格
CLEARSCORE:
	mov [ds:bx],ax
	add bx,2
	loop CLEARSCORE 	
	
       	;清空spd显示位置
	setDS lineAddr(24)	
	mov bx,68	
	mov cx,7			;清7格
CLEARSPD:
	mov [ds:bx],ax
	add bx,2
	loop CLEARSPD	
	
	;打印提示"spd:"和spd值
	mov ah,[es:info_color]	;颜色
	mov bx,70	
	mov al,'s'
	mov [ds:bx],ax
	add bx,2
	mov al,'p'
	mov [ds:bx],ax
	add bx,2
	mov al,'d'
	mov [ds:bx],ax
	add bx,2
	mov al,':'
	mov [ds:bx],ax
	add bx,2
	mov al,[es:spd]
	mov [ds:bx],ax

	;打印score的十进制值
	setDS cs	
	mov ax,[score]

	setDS lineAddr(0)		;设置score的位置
	mov bx,72	
	call print_num
	
	pop dx
	pop cx
	pop bx
	pop ax
	pop ds

	pop bp
	ret

;功能:显示一个数的十进制值
;参数: 被显示数ax，ds:bx已经指向显示位置
;返回: 无
print_num:
	push cx
	push bx
	push dx
	push ax
	push si	
	
	mov si,bx 		

	mov cx, -1
	mov bx,10
PRINTSCORE1:
	xor dx, dx
	div bx
	push dx
	cmp ax, 0
	loopne PRINTSCORE1

	not cx
PRINTSCORE2:
	pop ax
	mov ah,[es:info_color]	;颜色
	add al, '0'
	
	mov [ds:si],ax
	add si,2
	LOOP PRINTSCORE2

	pop si
	pop ax
	pop dx
	pop bx
	pop cx	
	ret

;--------------------------------------------支持函数------------------------------------------------
;功能: 生成一个新食物
;参数: 无
;返回: 无
draw_food:
	push ax
	push cx
	push dx	
	push bx
	push ds

	;读实时种（CH:CL=时:分 DH:DL=秒:1/100秒，设dh存的秒为x1,dl存的百分之秒为x2
	mov ah,2ch
	int 21h
	
	;算出当前实时秒个位，加1后压栈（1~10）
	xor ax,ax
	mov al,dh	
	mov bl,10
	div bl
	mov al,ah
	xor ah,ah
	inc ax
	push ax	
	
	;判断实时秒是奇还是偶，生成随机数ax
	xor ax,ax
	mov al,dh
	mov bl,2
	div bl
	cmp ah,0
	jz EVEN	
		
	pop ax
	mul dl	;实时秒是奇数：ax=(x1+1)*x2

	jmp cheakDone
EVEN:
	mov ax,11
	pop bx
	sub ax,bx
	mul dl	;实时秒是偶数：ax=(10-x1)*x2		
	
cheakDone:

	;用ax对行数和列数取模，得到随机食物位置
	push ax		;暂存随机数
	xor dx,dx
	mov bx,78	;dx:ax除bx，商在ax，余数在dx
	div bx
	add dx,1		;1~78，x坐标（每列2个字节）

	pop ax		;恢复随机数
	push dx		;暂存x坐标
	xor dx,dx
	mov bx,23	;dx:ax除bx，商在ax，余数在dx
	div bx
	inc dx		;1~23，y坐标
	
	;根据食物位置设置ds和bx，令[ds:bx]指向food所在点
	mov ax,displayBase		
	mov cx,dx
getYAddr:
	add ax,0ah		
	loop getYAddr	

	mov ds,ax		;至此ds指向food所在行首
			
	pop bx			;恢复x坐标
	add bx,bx			;至此[ds:bx]指向food所在点
	
	;如果随机到蛇身或者其他食物点，调整点位置（向右一直移动直到合适为止，注意换行）
	cmp byte[ds:bx],'*'	
	jz ADJUST
	cmp byte[ds:bx],'#'	
	jnz ADJUSTDONE

ADJUST:
	add bx,2
	cmp bx,156		;列>78，重置到1
	jb ADJUSTDONE
	mov bx,2		

	mov ds,ax
	add ax,0ah
	cmp ax,lineAddr(25)	;行>=25，重置到1
	jb ADJUSTDONE
	
	mov ax,lineAddr(1)
	mov ds,ax

ADJUSTDONE:
	;如果移动后还不行，继续移动直到可以
	cmp byte[ds:bx],'*'	
	jz ADJUST
	cmp byte[ds:bx],'#'	
	jz ADJUST


	;画食物
	mov ah,[es:food_color]
	mov al,'#'			;#号
	mov [ds:bx],ax
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop ax
        	ret

;在窗口指定位置显示字符串，字符串以'$'结束
;参数:si指向字符串首地址,ds:bx指向屏上显示起始位置
;返回:无
show_str:
	push ax
show_str_start:
	mov al,byte [es:si]
	cmp al,'$'
	jz show_str_end

	mov ah,byte [es:info_color]
	mov [ds:bx],ax
	inc si
	add bx,2
	jmp show_str_start
	
show_str_end:
	pop ax
	ret

;--------------------------------------数据区---------------------------------------------------------------
stack times 128 db 0 		;常规操作所用的堆栈
data times 4 dw 0 			;(int 9中断例程ip)、(int 9中断例程cs)、(int 1ch中断例程ip)、(int 1ch中断例程cs)

frame_color db 00001000B		;边框颜色
info_color db 0			;提示信息颜色（由边框颜色确定）
food_color db 10001011b		;食物颜色（闪烁亮蓝色）	

score dw 0   			;得分

speed db 25,20,16,14,10,8,5,3,2,1	;速度对应的计数器周期
spd db '6'				;速度选择

recount db 5			;定时器重装载值
count db 1			;定时器计数值

hdir_buff times 20 dw 0		;蛇头方向（hdir）按键缓冲区（循环队列），每个字数据：高字节是scan code,低字节是ascII（仅wasd四种）
hdir_head dw hdir_buff 	
hdir_tail dw hdir_buff

tdir_buff times 80 dw 0		;蛇尾方向（hdir）按键缓冲区（循环队列），每个双字数据：高节是转向位置,低节是ascII（仅wasd四种），因为蛇尾总是更晚进行转向，这个要开长一点
tdir_head dw tdir_buff 	
tdir_tail dw tdir_buff

snake_head dw getXY(7,8), 'd'		;存储蛇头位置和下一格移动方向（hdir），缺省为（5 8，'d'）
snake_tail dw getXY(5,8), 'd'		;存储蛇尾位置和下一格移动方向（tdir），缺省为（5 6，'d'）

get_food_flag db 0			;吃到食物标志
die_flag db 0			;死亡标志
pause_flag db 0			;暂停标志

score_str_pos dw getXY(25,10)		;信息位置
replay_str_pos dw getXY(27,11)		;信息位置
exit_str_pos dw getXY(27,12)			;信息位置
score_str db "game over! your score: ", '$' 	;游戏结束显示信息
replay_str db "press R to replay!",'$' 		;游戏结束显示信息
exit_str db "press ESC to exit!",'$' 		;游戏结束显示信息



