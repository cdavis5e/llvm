; RUN: llc < %s -mtriple=x86_64-linux-wine32  | FileCheck %s
; RUN: llc < %s -mtriple=x86_64-linux-wine32 -fast-isel | FileCheck %s

; Test for 32-bit function pointers with 32-bit calling conventions

@foo = external global void (i8 addrspace(32)*, i64) addrspace(32)*
@bar = external global void (i8 addrspace(32)*, i64) addrspace(32)*
@baz = external global void (i8 addrspace(32)*, i32, i64) addrspace(32)*
@quux = external global void (i8 addrspace(32)*, i64) addrspace(32)*

define void @test(i8 addrspace(32)* %h) nounwind uwtable {
entry:
  %0 = load void (i8 addrspace(32)*, i64) addrspace(32)*, void (i8 addrspace(32)*, i64) addrspace(32)** @foo, align 4
; For some reason, FastISel makes a direct reference instead of a RIP-relative
; one (FIXME?).
; CHECK: movl	foo{{(\(%rip\))?}}, %[[REG:e[^,]*|r[0-9]+d]]
  tail call x86_64_c32cc addrspace(32) void %0(i8 addrspace(32)* %h, i64 0) nounwind
; CHECK: movl	%esp, %e[[PTR:.*]]
; CHECK: movl	%edi, (%r[[PTR]])
; CHECK: movq	$0, 4(%r[[PTR]])
; CHECK: movl	%[[REG]], -24(%esp)
; CHECK: movw	$35, -20(%esp)
; CHECK: lcalll	*-24(%esp)
  %1 = load void (i8 addrspace(32)*, i64) addrspace(32)*, void (i8 addrspace(32)*, i64) addrspace(32)** @bar, align 4
; CHECK: movl	bar{{(\(%rip\))?}}, %[[REG2:e[^,]*|r[0-9]+d]]
  tail call x86_stdcallcc addrspace(32) void %1(i8 addrspace(32)* %h, i64 0) nounwind
; The callee will pop only 12 bytes from the stack. Make sure the stack
; gets readjusted after the call.
; CHECK: movl	%esp, %e[[PTR:.*]]
; CHECK: movl	%edi, (%r[[PTR]])
; CHECK: movq	$0, 4(%r[[PTR]])
; CHECK: movl	%[[REG2]], -24(%esp)
; CHECK: movw	$35, -20(%esp)
; CHECK: lcalll	*-24(%esp)
; CHECK: subl	$12, %esp
  %2 = load void (i8 addrspace(32)*, i32, i64) addrspace(32)*, void (i8 addrspace(32)*, i32, i64) addrspace(32)** @baz, align 4
; CHECK: movl	baz{{(\(%rip\))?}}, %[[REG3:e[^,]*|r[0-9]+d]]
  tail call x86_fastcallcc addrspace(32) void %2(i8 addrspace(32)* inreg %h, i32 inreg 0, i64 0) nounwind
; CHECK: movl	%esp, %e[[PTR:.*]]
; CHECK: movq	$0, (%r[[PTR]])
; CHECK: xorl	%edx, %edx
; CHECK: movl	%edi, %ecx
; CHECK: movl	%[[REG3]], -24(%esp)
; CHECK: movw	$35, -20(%esp)
; CHECK: lcalll	*-24(%esp)
; CHECK: subl	$8, %esp
  %3 = load void (i8 addrspace(32)*, i64) addrspace(32)*, void (i8 addrspace(32)*, i64) addrspace(32)** @quux, align 4
; CHECK: movl	quux{{(\(%rip\))?}}, %[[REG4:e[^,]*|r[0-9]+d]]
  tail call x86_thiscallcc addrspace(32) void %3(i8 addrspace(32)* inreg %h, i64 0) nounwind
; CHECK: movl	%esp, %e[[PTR:.*]]
; CHECK: movq	$0, (%r[[PTR]])
; CHECK: movl	%edi, %ecx
; CHECK: movl	%[[REG4]], -24(%esp)
; CHECK: movw	$35, -20(%esp)
; CHECK: lcalll	*-24(%esp)
; We should only have to pop 16 bytes, since the callee popped 8 bytes.
; CHECK: addl	$16, %esp
  ret void
}
