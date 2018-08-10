; RUN: llc < %s -mtriple=x86_64-linux-wine32  | FileCheck %s
; RUN: llc < %s -mtriple=x86_64-linux-wine32 -fast-isel | FileCheck %s

; Test for 32-bit function pointer tail call

@foo1 = external global void (i8 addrspace(32)*) addrspace(32)*
@foo2 = external global void (i8 addrspace(32)*) addrspace(32)*

define void @bar(i8 addrspace(32)* %h) nounwind uwtable {
entry:
  %0 = load void (i8 addrspace(32)*) addrspace(32)*, void (i8 addrspace(32)*) addrspace(32)** @foo1, align 4
; For some reason, FastISel makes a direct reference instead of a RIP-relative
; one (FIXME?).
; CHECK: movl	foo1{{(\(%rip\))?}}, %[[REG:e[^,]*|r[0-9]+d]]
  tail call addrspace(32) void %0(i8 addrspace(32)* %h) nounwind
; CHECK: movl	%[[REG]], -24(%esp)
; Note: 35 == 0x0023 (__USER32_CS on Linux)
; CHECK: movw	$35, -20(%esp)
; CHECK: lcalll	*-24(%esp)
  %1 = load void (i8 addrspace(32)*) addrspace(32)*, void (i8 addrspace(32)*) addrspace(32)** @foo2, align 4
; CHECK: movl	foo2{{(\(%rip\))?}}, %[[REG2:e[^,]*|r[0-9]+d]]
  tail call addrspace(32) void %1(i8 addrspace(32)* %h) nounwind
; This tail call can't be optimized. If this were a function we knew to be
; callable from 32-bit code, we maybe might be able to TCO this, but we can't.
; CHECK: movl	%[[REG2]], -24(%esp)
; CHECK: movw	$35, -20(%esp)
; CHECK: lcalll	*-24(%esp)
  ret void
}
