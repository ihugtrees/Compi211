apply:
    push rbp
    mov rbp, rsp
    mov rax, [rbp + 8 * 3]      ; rax = argc
    dec rax
    mov rax, PVAR(rax)          ; rax = last arg = list
    mov rdx, 0                  ; rdx = list_size

    push_args:
      cmp byte[rax], T_NIL
      je end_push_args
      CAR rbx, rax              ; rbx = car
      push rbx
      CDR rax, rax              ; rax = cdr
      inc rdx
      jmp push_args
    end_push_args:

    mov rsi,rdx                   ; rsi = list_size backup
    mov rcx, 0                    ; i = 0
    mov rbx, rdx                  ; rbx = list_size
    shr rbx, 1                    ; rbx = list_size/2
    dec rdx                       ; rdx = list_size -1

    _revert_args:
      cmp rcx, rbx
      jae end_revert_args
      mov rax, [rsp + 8 * (rdx)]  ; rax = [rsp + 8*(list_size - i -1)]
      mov rdi,[rsp+8*rcx]
      mov [rsp + 8 * rdx], rdi
      mov [rsp + 8 * rcx],  rax
      dec rdx
      inc rcx
      jmp _revert_args
    end_revert_args:

    mov rax, [rbp + 8 * 3]      ;rax = argc
    mov rdi, rax                ;rdi = index
    add rdi,2
    push_objs:
      cmp rdi, 4
      jbe end_push_objs
      push qword [rbp + 8 * rdi]
      inc rsi
      dec rdi
      jmp push_objs
    end_push_objs:

    push rsi                    ;push number of args
    mov rax, PVAR(0)            ; rax = closure of the procedure
    CLOSURE_ENV rbx, rax
    push rbx
    CLOSURE_CODE rbx, rax
    call rbx
    add rsp, 8 * 1
    pop rbx
    shl rbx, 3
    add rsp, rbx

    pop rbp
    ret