; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-linux-wine32 -O0 | FileCheck %s

define void @foo(i32 addrspace(32)* addrspace(32)* %p) {
; CHECK-LABEL: foo:
; CHECK:       # %bb.0:
; CHECK-NEXT:    leaq -{{[0-9]+}}(%esp), %rax
; CHECK-NEXT:    addq $16, %rax
; CHECK-NEXT:    movl %rax, (%edi)
; CHECK-NEXT:    retq
  %a = alloca i32, i32 10, addrspace(32)
  %addr = getelementptr i32, i32 addrspace(32)* %a, i32 4
  store i32 addrspace(32)* %addr, i32 addrspace(32)* addrspace(32)* %p
  ret void
}

