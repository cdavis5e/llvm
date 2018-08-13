; RUN: llc < %s -mtriple=x86_64-apple-darwin17-wine32 | FileCheck %s
; RUN: llc < %s -mtriple=x86_64-apple-darwin17-wine32 -fast-isel | FileCheck %s

define x86_64_c32cc i64 @foo(i32 %a, i64 %b, i8 addrspace(32)* %c) {
; CHECK-LABEL: _foo:
  %1 = load i8, i8 addrspace(32)* %c
; CHECK: movl 32(%esp), %eax
; CHECK-NEXT: movl %eax, %eax
  %2 = zext i8 %1 to i32
; CHECK-NEXT: movzbl (%rax), %eax
  %3 = add i32 %2, %a
; CHECK-NEXT: addl 20(%esp), %eax
  %4 = sext i32 %3 to i64
; CHECK-NEXT: cltq
  %5 = sub i64 %4, %b
; CHECK-NEXT: subq 24(%esp), %rax
  ret i64 %5
; CHECK-NEXT: movq %rax, %rdx
; CHECK-NEXT: shrq $32, %rdx
; CHECK: retq{{$}}
}

define x86_stdcallcc i64 @bar(i32 %a, i64 %b, i8 addrspace(32)* %c) {
; CHECK-LABEL: _bar:
  %1 = load i8, i8 addrspace(32)* %c
; CHECK: movl 32(%esp), %eax
; CHECK-NEXT: movl %eax, %eax
  %2 = zext i8 %1 to i32
; CHECK-NEXT: movzbl (%rax), %eax
  %3 = add i32 %2, %a
; CHECK-NEXT: addl 20(%esp), %eax
  %4 = sext i32 %3 to i64
; CHECK-NEXT: cltq
  %5 = sub i64 %4, %b
; CHECK-NEXT: subq 24(%esp), %rax
  ret i64 %5
; CHECK-NEXT: movq %rax, %rdx
; CHECK-NEXT: shrq $32, %rdx
; CHECK: retq{{$}}
}

define x86_fastcallcc i64 @baz(i32 inreg %a, i64 inreg %b, i8 addrspace(32)* %c) {
; CHECK-LABEL: _baz:
  %1 = load i8, i8 addrspace(32)* %c
; CHECK: movl 28(%esp), %eax
; CHECK-NEXT: movl %eax, %eax
  %2 = zext i8 %1 to i32
; CHECK-NEXT: movzbl (%rax), %eax
  %3 = add i32 %2, %a
; CHECK-NEXT: addl %ecx, %eax
  %4 = sext i32 %3 to i64
; CHECK-NEXT: cltq
  %5 = sub i64 %4, %b
; CHECK-NEXT: subq 20(%esp), %rax
  ret i64 %5
; CHECK-NEXT: movq %rax, %rdx
; CHECK-NEXT: shrq $32, %rdx
; CHECK: retq{{$}}
}

define x86_thiscallcc i64 @quux(i32 inreg %a, i64 %b, i8 addrspace(32)* %c) {
; CHECK-LABEL: _quux:
  %1 = load i8, i8 addrspace(32)* %c
; CHECK: movl 28(%esp), %eax
; CHECK-NEXT: movl %eax, %eax
  %2 = zext i8 %1 to i32
; CHECK-NEXT: movzbl (%rax), %eax
  %3 = add i32 %2, %a
; CHECK-NEXT: addl %ecx, %eax
  %4 = sext i32 %3 to i64
; CHECK-NEXT: cltq
  %5 = sub i64 %4, %b
; CHECK-NEXT: subq 20(%esp), %rax
  ret i64 %5
; CHECK-NEXT: movq %rax, %rdx
; CHECK-NEXT: shrq $32, %rdx
; CHECK: retq{{$}}
}
