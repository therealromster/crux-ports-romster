bits 64

global _start

message: db 'CRUX version 3.1', 0xA

message_length: equ $-message

section .text
_start:
mov rax, 1
mov rdi, 1
mov rsi, message
mov rdx, message_length
syscall
mov rax, 60
mov rdi, 0
syscall
