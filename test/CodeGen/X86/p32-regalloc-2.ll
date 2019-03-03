; RUN: llc < %s | FileCheck %s
target datalayout = "e-m:o-p32:32:32-A32-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.13.0-wine32"

; Function Attrs: nounwind
declare noalias i8* @malloc() local_unnamed_addr #2

; Function Attrs: nounwind ssp uwtable
define dso_local void @wpp_default_lookup() #1 {
entry:
  %call.i = tail call i8* @malloc() #2
  store i8 0, i8* %call.i, align 1
  br label %for.body46

for.body46:                                       ; preds = %for.body46, %entry
  br label %for.body46
}

; CHECK: pushq	%rbp
; CHECK: movl	%esp, %ebp
; CHECK: callq	_malloc
; CHECK: movb	$0, (%rax)

attributes #1 = { nounwind ssp "disable-tail-calls"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "stack-protector-buffer-size"="8" }
attributes #2 = { nounwind }

