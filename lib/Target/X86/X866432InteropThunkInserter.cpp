//===----- X866432InteropThunkInserter.cpp - Insert 64/32 interop thunks --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a module pass that generates 32-bit and 64-bit thunks
// for all functions with a 32-bit calling convention for which thunks do
// not already exist.
//
//===----------------------------------------------------------------------===//

#include "X86.h"
#include "X86InstrBuilder.h"
#include "X86InstrInfo.h"
#include "X86Subtarget.h"
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Regex.h"

using namespace llvm;

namespace {

class X866432InteropThunkInserter : public ModulePass {
public:
  X866432InteropThunkInserter() : ModulePass(ID) {}

  bool runOnModule(Module &M) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override;

private:
  // The set of all symbols defined in inline asm.
  StringSet<> ExistingAsmSymbols;

  void scanExistingAsmSymbols(Module &M);
  void generateThunks(Module &M, Function &Fn);
  Constant &generateThunk64Side(Module &M, Function &Fn, StringRef Prefix);
  void generateThunk32Side(Module &M, Constant &Thunk64, Function &Fn,
                           StringRef Prefix, CallingConv::ID CC);

  MachineModuleInfo *MMI;
  Mangler Mang;

  const X86Subtarget *STI;
  const TargetInstrInfo *TII;

  bool FoundInteropFn = false;

  StringRef getPassName() const override {
    return "X86 64/32 Interop Thunk Inserter";
  }
  static char ID;
};

char X866432InteropThunkInserter::ID = 0;

} // end anonymous namespace

ModulePass *llvm::createX866432InteropThunkInserter() {
  return new X866432InteropThunkInserter();
}

void X866432InteropThunkInserter::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<MachineModuleInfo>();
  AU.setPreservesCFG();
}

void X866432InteropThunkInserter::scanExistingAsmSymbols(Module &M) {
  static Regex SymbolRE("[^[:digit:]].*:");

  // It's not enough to getOrInsertFunction() and check that it isn't defined.
  // Thunks may be defined in file-scope inline asm, which isn't really
  // accounted for in LLVM IR's symbol table. This bit scans all the file-scope
  // inline asm and grabs all defined symbols.
  StringRef InlineAsm = M.getModuleInlineAsm();
  SmallVector<StringRef, 4> Lines;
  InlineAsm.split(Lines, "\n");
  for (StringRef Line : Lines) {
    // Strip away any comment.
    Line = Line.split("#").first.trim();
    // If it matches the form /[^[:digit:]].*:/, it's a symbol definition.
    if (SymbolRE.match(Line)) {
      Line = Line.drop_back().trim();
      // If surrounded by quotes, strip those, too.
      if ((Line.front() == '"' && Line.back() == '"') ||
          (Line.front() == '\'' && Line.back() == '\''))
        Line = Line.drop_front().drop_back();
      ExistingAsmSymbols.insert(Line);
    }
  }
}

void X866432InteropThunkInserter::generateThunks(Module &M, Function &Fn) {
  CallingConv::ID CC = Fn.getCallingConv();
  if (CC != CallingConv::X86_64_C32 && CC != CallingConv::X86_StdCall &&
      CC != CallingConv::X86_FastCall && CC != CallingConv::X86_ThisCall)
    return;

  FoundInteropFn = true;

  StringRef Prefix = "__i386_on_x86_64_thunk_";
  if (Fn.hasFnAttribute("thunk-prefix"))
    Prefix = Fn.getFnAttribute("thunk-prefix").getValueAsString();

  Constant &Thunk64 = generateThunk64Side(M, Fn, Prefix);
  generateThunk32Side(M, Thunk64, Fn, Prefix, CC);
}

Constant &X866432InteropThunkInserter::generateThunk64Side(
    Module &M, Function &Fn, StringRef Prefix) {
  std::string ThunkName = (Prefix + "64_" + Fn.getName()).str();

  // If it's already defined, don't define it again.
  Function *ThunkFn = M.getFunction(ThunkName);
  if (ThunkFn && !ThunkFn->empty())
    return *ThunkFn;

  if (!ThunkFn)
    ThunkFn = cast<Function>(M.getOrInsertFunction(
        ThunkName, Fn.getFunctionType()));

  ThunkFn->setLinkage(GlobalValue::ExternalLinkage);

  // Make sure this isn't one of the symbols defined in inline asm.
  SmallString<32> Mangled;
  Mang.getNameWithPrefix(Mangled, ThunkName, M.getDataLayout());
  if (ExistingAsmSymbols.count(Mangled)) {
    return *ThunkFn;
  }

  // Give the function a body so it will get emitted.
  auto *BB = BasicBlock::Create(M.getContext(), "", ThunkFn);
  new UnreachableInst(M.getContext(), BB);

  // Skip generating IR. Instead, just generate a MachineFunction directly.
  MachineFunction &MF = MMI->getOrCreateMachineFunction(*ThunkFn);
  auto *MBB = MF.CreateMachineBasicBlock();
  MF.push_back(MBB);
  STI = &MF.getSubtarget<X86Subtarget>();
  TII = STI->getInstrInfo();

  // Do the call.
  BuildMI(MBB, DebugLoc(), TII->get(X86::CALL64pcrel32))
      .addGlobalAddress(&Fn);
  // Return far. This needs to be a 32-bit return, since we were called by
  // 32-bit code.
  BuildMI(MBB, DebugLoc(), TII->get(X86::LRETL));
  return *ThunkFn;
}

static unsigned getTypeStackSize(const DataLayout &DL, Type *Ty) {
  return alignTo(DL.getTypeStoreSize(Ty), 4);
}

void X866432InteropThunkInserter::generateThunk32Side(
    Module &M, Constant &Thunk64, Function &Fn, StringRef Prefix,
    CallingConv::ID CC) {
  std::string ThunkName = (Prefix + "32_" + Fn.getName()).str();

  // If it's already defined, don't define it again.
  Function *ThunkFn = M.getFunction(ThunkName);
  if (ThunkFn && !ThunkFn->empty())
    return;

  if (!ThunkFn)
    ThunkFn = cast<Function>(M.getOrInsertFunction(
        ThunkName, Fn.getFunctionType()));

  ThunkFn->setLinkage(GlobalValue::ExternalLinkage);

  // Make sure this isn't one of the symbols defined in inline asm.
  SmallString<32> Mangled;
  Mang.getNameWithPrefix(Mangled, ThunkName, M.getDataLayout());
  if (ExistingAsmSymbols.count(Mangled)) {
    return;
  }

  // Give the function a body so it will get emitted.
  auto *BB = BasicBlock::Create(M.getContext(), "", ThunkFn);
  new UnreachableInst(M.getContext(), BB);

  // Mark this function as one we need to generate prefix data for.
  ThunkFn->addFnAttr("thunk-32bit-side");

  // Get a global variable to hold the address of the 64-bit thunk.
  auto *GV = new GlobalVariable(M, Thunk64.getType(), true,
                                GlobalValue::PrivateLinkage, &Thunk64,
                                "", nullptr, GlobalVariable::NotThreadLocal,
                                32);

  // Get the external variable holding the segment selector to use.
  StringRef CSName = "__i386_on_x86_64_cs64";
  if (Fn.hasFnAttribute("thunk-cs64-name"))
    CSName = Fn.getFnAttribute("thunk-cs64-name").getValueAsString();
  auto *TargetCS = M.getNamedValue(CSName);
  if (!TargetCS)
    TargetCS = new GlobalVariable(M, IntegerType::get(M.getContext(), 16),
                                  false, GlobalValue::ExternalLinkage, nullptr,
                                  CSName, nullptr,
                                  GlobalVariable::NotThreadLocal, 32,
                                  /*isExternallyInitialized=*/true);

  // Skip generating IR. Instead, just generate a MachineFunction directly.
  MachineFunction &MF = MMI->getOrCreateMachineFunction(*ThunkFn);
  auto *MBB = MF.CreateMachineBasicBlock();
  MF.push_back(MBB);
  STI = &MF.getSubtarget<X86Subtarget>();
  TII = STI->getInstrInfo();

  // Insert a magic 'mov edi, edi' (8b ff) instruction so this can be
  // hotpatched. The prefix data will get overwritten by the hotpatch jump.
  BuildMI(MBB, DebugLoc(), TII->get(X86::MOV32rr_REV), X86::EDI)
      .addReg(X86::EDI);

  // Get the current instruction pointer to use as a PIC base.
  BuildMI(MBB, DebugLoc(), TII->get(X86::MOVPC32r), X86::EAX).addImm(0);

  // Move the offset part of the far pointer onto the stack.
  BuildMI(MBB, DebugLoc(), TII->get(X86::PUSH64rmm))
      .addReg(X86::EAX)                                    // Base
      .addImm(1)                                           // Scale
      .addReg(0)                                           // Index
      .addGlobalAddress(GV, 0, X86II::MO_PIC_BASE_OFFSET)  // Displacement
      .addReg(0);

  // Get the segment part of the far pointer.
  // Since this global might be defined in another image, we have to use a
  // GOT reference here. We can't use GOTPCREL here, though--that only works
  // in 64-bit code.
  unsigned OpFlag = STI->isTargetDarwin()
      ? X86II::MO_DARWIN_NONLAZY_PIC_BASE : X86II::MO_GOT;
  BuildMI(MBB, DebugLoc(), TII->get(X86::MOV32rm), X86::EAX)
      .addReg(X86::EAX, getKillRegState(true))
      .addImm(1)
      .addReg(0)
      .addGlobalAddress(TargetCS, 0, OpFlag)
      .addReg(0);
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::MOV16rm), X86::AX),
               X86::EAX, /*isKill=*/true, 0);
  // While we're at it, readjust the stack pointer.
  BuildMI(MBB, DebugLoc(), TII->get(X86::ADD32ri8), X86::ESP)
      .addReg(X86::ESP)
      .addImm(4);

  // Move the segment selector onto the stack.
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::MOV16mr)),
               X86::ESP, /*isKill=*/false, -4)
      .addReg(X86::AX, getKillRegState(true));

  // Call the function.
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::FARCALL32m)),
               X86::ESP, /*isKill=*/false, 0);

  // If this is a cdecl thunk, we can just return now.
  if (CC == CallingConv::X86_64_C32) {
    // This is really a 32-bit return, but in 64-bit mode we can't directly
    // encode 32-bit branch instructions.
    BuildMI(MBB, DebugLoc(), TII->get(X86::RETQ));
    return;
  }

  // This follows one of the callee-pop conventions. We need to use a popping
  // return. But first we have to work out how many bytes to pop.
  bool FoundECX = false, FoundEDX = false;
  unsigned PopAmt = 0;
  FunctionType *FnTy = Fn.getFunctionType();
  for (unsigned i = 0; i < FnTy->getNumParams(); ++i) {
    // Ignore the 'thunkdata' parameter; that's a hidden parameter that is
    // always passed in a register. It's only used when calling a 32-bit
    // function pointer.
    if (Fn.hasParamAttribute(i, Attribute::ThunkData))
      continue;
    Type *ParamTy = FnTy->getParamType(i);
    const DataLayout &DL = M.getDataLayout();
    // For fastcall, the first two integers that fit in 4 bytes are in ECX and
    // EDX. For thiscall, only the first such argument is in ECX.
    if ((CC == CallingConv::X86_FastCall || CC == CallingConv::X86_ThisCall) &&
        !FoundECX && ParamTy->isIntegerTy() &&
        DL.getTypeStoreSize(ParamTy) <= 4) {
      FoundECX = true;
      continue;
    }
    if (CC == CallingConv::X86_FastCall && !FoundEDX &&
        ParamTy->isIntegerTy() && DL.getTypeStoreSize(ParamTy) <= 4) {
      FoundEDX = true;
      continue;
    }
    // Otherwise, it's passed on the stack.
    PopAmt += getTypeStackSize(DL, ParamTy);
  }

  // Now we can pop the required number of bytes.
  BuildMI(MBB, DebugLoc(), TII->get(X86::RETIQ)).addImm(PopAmt);
}

bool X866432InteropThunkInserter::runOnModule(Module &M) {
  MMI = &getAnalysis<MachineModuleInfo>();

  scanExistingAsmSymbols(M);
  for (auto &Fn : M)
    generateThunks(M, Fn);

  return FoundInteropFn;
}
