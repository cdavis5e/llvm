; RUN: llc < %s | FileCheck %s
target datalayout = "e-m:o-p32:32:32-A32-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.13.0-wine32"

@output_makefile_name = internal unnamed_addr addrspace(32) global i8* addrspacecast (i8 addrspace(32)* getelementptr inbounds ([9 x i8], [9 x i8] addrspace(32)* @.str.46, i32 0, i32 0) to i8*), align 8
@.str.46 = private unnamed_addr addrspace(32) constant [9 x i8] c"Makefile\00", align 1

; CHECK: _output_makefile_name:
; CHECK: .quad	[[STR:.*]]
; CHECK: [[STR]]:
; CHECK: .asciz	"Makefile"
