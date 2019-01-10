; RUN: llc < %s -mtriple=x86_64-apple-darwin17-wine32 | FileCheck %s
; RUN: llc < %s -mtriple=x86_64-apple-darwin17-wine32 -fast-isel | FileCheck %s

%struct.__thunk_data = type { i64, i64, i64 }

define x86_64_c32cc i64 @foo(%struct.__thunk_data addrspace(32)* thunkdata %td, i32 %a, i64 %b, i8 addrspace(32)* %c) {
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

define void @call_foo() {
; CHECK-LABEL: _call_foo:
  %td = alloca %struct.__thunk_data, align 8, addrspace(32)
; CHECK: subl $56, %esp
  %1 = call x86_64_c32cc i64 @foo(%struct.__thunk_data addrspace(32)* %td, i32 0, i64 0, i8 addrspace(32)* null)
; CHECK-DAG: movl $0, 12(%{{rax|esp}})
; CHECK-DAG: movq $0, 16(%{{rax|esp}})
; CHECK-DAG: movl $0, 24(%{{rax|esp}})
; CHECK-DAG: leal 32(%rsp), %eax
; CHECK: callq _foo
; CHECK: addl $56, %esp
; CHECK: retq
  ret void
}

define x86_stdcallcc i64 @bar(%struct.__thunk_data addrspace(32)* thunkdata %td, i32 %a, i64 %b, i8 addrspace(32)* %c) {
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

define void @call_bar() {
; CHECK-LABEL: _call_bar:
  %td = alloca %struct.__thunk_data, align 8, addrspace(32)
; CHECK: subl $56, %esp
  %1 = call x86_stdcallcc i64 @bar(%struct.__thunk_data addrspace(32)* %td, i32 0, i64 0, i8 addrspace(32)* null)
; CHECK-DAG: movl $0, 12(%{{rax|esp}})
; CHECK-DAG: movq $0, 16(%{{rax|esp}})
; CHECK-DAG: movl $0, 24(%{{rax|esp}})
; CHECK-DAG: leal 32(%rsp), %eax
; CHECK: callq _bar
; CHECK: addl $56, %esp
; CHECK: retq
  ret void
}

define x86_fastcallcc i64 @baz(%struct.__thunk_data addrspace(32)* thunkdata %td, i32 inreg %a, i64 inreg %b, i8 addrspace(32)* %c) {
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

define void @call_baz() {
; CHECK-LABEL: _call_baz:
  %td = alloca %struct.__thunk_data, align 8, addrspace(32)
; CHECK: subl $56, %esp
  %1 = call x86_fastcallcc i64 @baz(%struct.__thunk_data addrspace(32)* %td, i32 inreg 0, i64 inreg 0, i8 addrspace(32)* null)
; CHECK-DAG: xorl %ecx, %ecx
; CHECK-DAG: movq $0, 12(%{{rax|esp}})
; CHECK-DAG: movl $0, 20(%{{rax|esp}})
; CHECK-DAG: leal 32(%rsp), %eax
; CHECK: callq _baz
; CHECK: addl $56, %esp
; CHECK: retq
  ret void
}

define x86_thiscallcc i64 @quux(%struct.__thunk_data addrspace(32)* thunkdata %td, i32 inreg %a, i64 %b, i8 addrspace(32)* %c) #1 {
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

define void @call_quux() {
; CHECK-LABEL: _call_quux:
  %td = alloca %struct.__thunk_data, align 8, addrspace(32)
; CHECK: subl $56, %esp
  %1 = call x86_thiscallcc i64 @quux(%struct.__thunk_data addrspace(32)* %td, i32 inreg 0, i64 0, i8 addrspace(32)* null)
; CHECK-DAG: xorl %ecx, %ecx
; CHECK-DAG: movq $0, 12(%{{rax|esp}})
; CHECK-DAG: movl $0, 20(%{{rax|esp}})
; CHECK-DAG: leal 32(%rsp), %eax
; CHECK: callq _quux
; CHECK: addl $56, %esp
; CHECK: retq
  ret void
}

module asm "___i386_on_x86_64_thunk64_bar:
  lretl"
module asm "___i386_on_x86_64_thunk32_baz:
  retq"

; Check that thunks are generated and that they have the correct form.
; CHECK-LABEL: ___i386_on_x86_64_thunk64_foo:
; CHECK:         callq _foo
; CHECK-NEXT:    lretl

; Every 32-bit thunk must be immediately preceded by a magic number. The
; extra goop is to ensure that the magic number directly abuts the thunk, and
; that the thunk and the magic number are on the same page.
; CHECK: .p2align 5, 0x90
; CHECK: .quad _foo-[[PB:.*]]
; CHECK: .quad 8595522607861216050
; CHECK-LABEL: ___i386_on_x86_64_thunk32_foo:
; CHECK:         movl %edi, %edi
; CHECK-NEXT:    callq [[PB]]
; CHECK:       [[PB]]:
; CHECK-NEXT:    popl %eax
; CHECK:         pushq [[FOO_P:[^-]*]]-[[PB]](%eax)
; CHECK-NEXT:    movl L___i386_on_x86_64_cs64$non_lazy_ptr-[[PB]](%eax), %eax
; CHECK-NEXT:    movw (%eax), %ax
; CHECK-NEXT:    addl $4, %esp
; CHECK-NEXT:    movw %ax, -4(%esp)
; CHECK-NEXT:    lcalll *(%esp)
; CHECK-NEXT:    retq

; Check that thunks *aren't* generated if we already defined them in inline
; assembly.
; CHECK-NOT: ___i386_on_x86_64_thunk64_bar:

; CHECK: .p2align 5, 0x90
; CHECK: .quad _bar-[[PB:.*]]
; CHECK: .quad 8595522607861216050
; CHECK-LABEL: ___i386_on_x86_64_thunk32_bar:
; CHECK:         movl %edi, %edi
; CHECK-NEXT:    callq [[PB]]
; CHECK:       [[PB]]:
; CHECK-NEXT:    popl %eax
; CHECK:         pushq [[BAR_P:[^-]*]]-[[PB]](%eax)
; CHECK-NEXT:    movl L___i386_on_x86_64_cs64$non_lazy_ptr-[[PB]](%eax), %eax
; CHECK-NEXT:    movw (%eax), %ax
; CHECK-NEXT:    addl $4, %esp
; CHECK-NEXT:    movw %ax, -4(%esp)
; CHECK-NEXT:    lcalll *(%esp)
; CHECK-NEXT:    retq $16

; CHECK-LABEL: ___i386_on_x86_64_thunk64_baz:
; CHECK:         callq _baz
; CHECK-NEXT:    lretl

; CHECK-NOT: ___i386_on_x86_64_thunk32_baz:

; CHECK-LABEL: ___wine32_thunk64_quux:
; CHECK:         callq _quux
; CHECK-NEXT:    lretl

; CHECK: .p2align 5, 0x90
; CHECK: .quad _quux-[[PB:.*]]
; CHECK: .quad 8595522607861216050
; CHECK-LABEL: ___wine32_thunk32_quux:
; CHECK:         movl %edi, %edi
; CHECK-NEXT:    callq [[PB]]
; CHECK:       [[PB]]:
; CHECK-NEXT:    popl %eax
; CHECK:         pushq [[QUUX_P:[^-]*]]-[[PB]](%eax)
; CHECK-NEXT:    movl L___wine32_cs64$non_lazy_ptr-[[PB]](%eax), %eax
; CHECK-NEXT:    movw (%eax), %ax
; CHECK-NEXT:    addl $4, %esp
; CHECK-NEXT:    movw %ax, -4(%esp)
; CHECK-NEXT:    lcalll *(%esp)
; CHECK-NEXT:    retq $12

; CHECK: [[FOO_P]]:
; CHECK-NEXT: .quad ___i386_on_x86_64_thunk64_foo
; CHECK: [[BAR_P]]:
; CHECK-NEXT: .quad ___i386_on_x86_64_thunk64_bar
; CHECK: [[QUUX_P]]:
; CHECK-NEXT: .quad ___wine32_thunk64_quux

attributes #1 = { "thunk-prefix"="__wine32_" "thunk-cs64-name"="__wine32_cs64" }
