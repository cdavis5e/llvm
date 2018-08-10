; RUN: llc < %s -mtriple=x86_64-linux-wine32 | FileCheck %s
; RUN: llc < %s -mtriple=x86_64-linux-wine32 -fast-isel | FileCheck %s

; Test calling function pointer passed in struct

;    The function argument `h' in

;    struct foo {
;      void (* __ptr32 f) (void);
;      int i;
;    };
;    void
;    bar (struct foo h)
;    {
;      h.f ();
;    }

;    is passed in the 64-bit %rdi register.  The `f' field is in the lower 32
;    bits of %rdi register and the `i' field is in the upper 32 bits of %rdi
;    register.

define void @bar(i64 %h.coerce) nounwind {
entry:
  %h.sroa.0.0.extract.trunc = trunc i64 %h.coerce to i32
  %0 = inttoptr i32 %h.sroa.0.0.extract.trunc to void () addrspace(32)*
  tail call addrspace(32) void %0() nounwind "based-far-segment"="0xf00d"
; CHECK: movl	%edi, -24(%esp)
; Note: 61453 == 0xf00d
; CHECK: movw	$61453, -20(%esp)
; CHECK: lcalll	*-24(%esp)
  ret void
}
