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

define x86_thiscallcc i64 @quux(%struct.__thunk_data addrspace(32)* thunkdata %td, i32 inreg %a, i64 %b, i8 addrspace(32)* %c) {
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
