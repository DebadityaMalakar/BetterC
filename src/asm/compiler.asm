section .data
    read_msg db "Reading file...", 10, 0
    tokenize_msg db "Tokenizing...", 10, 0
    compiled_msg db "Compilation complete.", 10, 0
    error_msg db "Error reading file!", 10, 0
    tokens_msg db "Tokens: %s", 10, 0
    file_ext db ".out", 0

section .bss
    file_name resb 256
    file_content resb 4096
    tokenized_output resb 1024

section .text
    global Compile
    extern ReadFile, Tokenize, printf, WriteFile

Compile:
    ; Print "Reading file..."
    mov rdi, read_msg   ; First argument (printf)
    call printf

    ; Call ReadFile(FileName)
    mov rdi, [rsp+8]    ; Get filename argument (passed on stack)
    call ReadFile
    test rax, rax
    jz error_exit       ; If NULL, exit with error
    mov [file_content], rax  ; Store file content

    ; Print "Tokenizing..."
    mov rdi, tokenize_msg
    call printf

    ; Call Tokenize(file_content)
    mov rdi, file_content
    call Tokenize
    mov [tokenized_output], rax  ; Store tokenized output

    ; Print tokens
    mov rdi, tokens_msg  ; 1st argument (format string)
    mov rsi, tokenized_output  ; 2nd argument
    call printf

    ; Write the compiled output file
    mov rdi, file_name
    mov rsi, file_content
    call WriteFile

    ; Print success message
    mov rdi, compiled_msg
    call printf
    ret

error_exit:
    mov rdi, error_msg
    call printf
    ret
