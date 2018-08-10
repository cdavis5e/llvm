; RUN: llc < %s -mtriple=x86_64-apple-darwin17-wine32 | FileCheck %s
; RUN: llc < %s -mtriple=x86_64-apple-darwin17-wine32 -fast-isel | FileCheck %s

; Test call function pointer with function argument
;
; void bar (void * h, void (*foo) (void *))
;    {
;      foo (h);
;      foo (h);
;    }


define void @bar(i8 addrspace(32)* %h, void (i8 addrspace(32)*) addrspace(32)* nocapture %foo) nounwind {
entry:
  tail call addrspace(32) void %foo(i8 addrspace(32)* %h) nounwind
; CHECK: movl	%esi, %[[REG:e[^,]+|r[0-9]+d]]
; CHECK: movl	%esi, -24(%esp)
; Note: 27 == 0x001b
; CHECK: movw	$27, -20(%esp)
; CHECK: lcalll	*-24(%esp)
  tail call addrspace(32) void %foo(i8 addrspace(32)* %h) nounwind
; CHECK: movl	%[[REG]], -24(%esp)
; CHECK: movw	$27, -20(%esp)
; CHECK: lcalll	*-24(%esp)
  ret void
}
