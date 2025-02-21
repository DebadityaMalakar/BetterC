section .data
    read_msg db "Reading file...", 10, 0
    tokenize_msg db "Tokenizing...", 10, 0
    compiled_msg db "Compilation complete.", 10, 0
    error_msg db "Error reading file!", 10, 0
    tokens_msg db "Tokens: %s", 10, 0
    file_ext db ".out", 0
    file_name resb 256

section .bss
    file_content resb 4096
    tokenized_output resb 1024

section .text
    global Compile
    extern ReadFile, Tokenize, printf, WriteFile

Compile:
    ; Print "Reading file..."
    push read_msg
    call printf
    add esp, 4

    ; Call ReadFile(FileName)
    push dword [esp+4]  ; Get filename argument
    call ReadFile
    add esp, 4
    test eax, eax
    jz error_exit       ; If NULL, exit with error
    mov [file_content], eax  ; Store file content

    ; Print "Tokenizing..."
    push tokenize_msg
    call printf
    add esp, 4

    ; Call Tokenize(file_content)
    push file_content
    call Tokenize
    add esp, 4
    mov [tokenized_output], eax  ; Store tokenized output

    ; Print tokens
    push tokenized_output
    push tokens_msg
    call printf
    add esp, 8

    ; Write the compiled output file
    push file_content
    push file_name
    call WriteFile
    add esp, 8

    ; Print success message
    push compiled_msg
    call printf
    add esp, 4
    ret

error_exit:
    push error_msg
    call printf
    add esp, 4
    ret
