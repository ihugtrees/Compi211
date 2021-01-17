apply:
    push rbp
    mov rbp, rsp

    mov rbx, ARGC
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
    cmp r8, 1
    jbe end_reverse
    mov rdx, r8
    shr rdx, 1
    dec r8
    xor r9, r9
    mov rcx, rdx
    
    reverse:
   

      mov r10, [rsp + WORD_SIZE * r8]
      mov r11, [rsp + WORD_SIZE * r9]
      mov [rsp + WORD_SIZE * r9], r10
      mov [rsp + WORD_SIZE * r8], r11
      inc r9
      dec r8
      loop reverse
    end_reverse:

    mov rcx, ARGC
    sub rcx, 2
    cmp rcx, 0
    je end_push_args
    push_args:
      push qword [rbp + WORD_SIZE * (4+rcx)]
      dec rcx
      inc r15
      loop push_args
    end_push_args:

    push r15 
    mov rax, PVAR(0)
    CLOSURE_ENV rbx, rax
    push rbx
    push qword[rbp+8*1]  
    add r15, 2
    FIX_APPLICTP_STACK r15
    CLOSURE_CODE rbx, rax
    jmp rbx
