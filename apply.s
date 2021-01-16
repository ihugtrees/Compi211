apply:
	push rbp
	mov rbp, rsp

	mov rbx, [rbp + WORD_SIZE * 3]
	mov r9, [rbp + WORD_SIZE * (3 + rbx)]
	xor r8, r8
	push_list:
		cmp r9, SOB_NIL_ADDRESS
		je end_push_list
		CAR rbx, r9
		push rbx
		inc r8
		CDR r9, r9
		jmp push_list
	end_push_list:

	mov r15, r8
	dec r8
	mov rdx, r8
	shr rdx, 1
	xor rcx, rcx
	reverse:
		cmp rcx, rdx
		jae end_reverse
		mov r10, [rsp + WORD_SIZE * r8]
		mov r11, [rsp + WORD_SIZE * rcx]
		mov [rsp + WORD_SIZE * rcx], r10
		mov [rsp + WORD_SIZE * r8], r11
		inc rcx
		dec r8
		jmp reverse
	end_reverse:

	mov rcx, [rbp + WORD_SIZE * 3]
	sub rcx, 2
	push_args:
		cmp rcx, 0
		je end_push_args
		push qword [rbp + WORD_SIZE * (4+rcx)]
		dec rcx
		inc r15
		jmp push_args
	end_push_args:

	push r15  ; push new argc
	mov rax, [rbp + WORD_SIZE * 4]
	CLOSURE_ENV rbx, rax
	push rbx
	push qword[rbp+8*1]   ;old ret addr
	add r15, 2
	FIX_APPLICTP_STACK r15
	CLOSURE_CODE rbx, rax
	jmp rbx

	; call rbx
	; add rsp, 8 * 1
	; pop rbx
	; shl rbx, 3
	; add rsp, rbx

	; pop rbp
	; ret