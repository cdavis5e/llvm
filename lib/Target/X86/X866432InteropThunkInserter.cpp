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
  bool generateThunks(Module &M, Function &Fn);
  Constant &generateThunk64Side(Module &M, Function &Fn, StringRef Prefix);
  void generateThunk32Side(Module &M, Constant &Thunk64, Function &Fn,
                           StringRef Prefix, CallingConv::ID CC);
  Function *getOrInsertFarCallHelper(Module &M, unsigned PopAmount,
                                     StringRef Prefix, StringRef CS32Name,
                                     StringRef CS64Name);
  GlobalValue *getExternalObject(Module &M, StringRef Name);
  bool expandFarCallPseudos(Module &M, Function &Fn);
  bool expandMBB(Module &M, MachineBasicBlock &MBB, StringRef Prefix,
                 StringRef CS32Name, StringRef CS64Name);
  bool expandMI(Module &M, MachineBasicBlock &MBB,
                MachineBasicBlock::iterator MBBI, StringRef Prefix,
                StringRef CS32Name, StringRef CS64Name);

  MachineModuleInfo *MMI;
  Mangler Mang;

  const X86Subtarget *STI;
  const TargetInstrInfo *TII;

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

bool X866432InteropThunkInserter::generateThunks(Module &M, Function &Fn) {
  CallingConv::ID CC = Fn.getCallingConv();
  if (CC != CallingConv::X86_64_C32 && CC != CallingConv::X86_StdCall &&
      CC != CallingConv::X86_FastCall && CC != CallingConv::X86_ThisCall)
    return false;

  StringRef Prefix = "__i386_on_x86_64_";
  if (Fn.hasFnAttribute("thunk-prefix"))
    Prefix = Fn.getFnAttribute("thunk-prefix").getValueAsString();

  Constant &Thunk64 = generateThunk64Side(M, Fn, Prefix);
  generateThunk32Side(M, Thunk64, Fn, Prefix, CC);
  return true;
}

Constant &X866432InteropThunkInserter::generateThunk64Side(
    Module &M, Function &Fn, StringRef Prefix) {
  std::string ThunkName = (Prefix + "thunk64_" + Fn.getName()).str();

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
  std::string ThunkName = (Prefix + "thunk32_" + Fn.getName()).str();

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
  auto *TargetCS = getExternalObject(M, CSName);

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

Function *X866432InteropThunkInserter::getOrInsertFarCallHelper(
    Module &M, unsigned PopAmount, StringRef Prefix,
    StringRef CS32Name, StringRef CS64Name) {
  FunctionType *HelperTy = llvm::FunctionType::get(
      llvm::IntegerType::get(M.getContext(), 32),
      llvm::PointerType::get(llvm::IntegerType::get(M.getContext(), 8), 32),
      /*isVarArg=*/true);
  std::string Helper64Name = (Prefix + "invoke32_64_" +utostr(PopAmount)).str();
  std::string Helper32Name = (Prefix + "invoke32_32").str();

  Function *Helper64 = M.getFunction(Helper64Name);
  Function *Helper32 = M.getFunction(Helper32Name);
  // If the function is already defined, there's nothing to do.
  if (Helper64 && !Helper64->empty())
    return Helper64;

  // If we have to define the helpers ourselves, make sure they're of the right
  // type.
  if (Helper64 && Helper64->getFunctionType() != HelperTy)
    M.getFunctionList().erase(Helper64);
  Helper64 = cast<Function>(M.getOrInsertFunction(Helper64Name, HelperTy));

  if (Helper32 && Helper32->empty() && Helper32->getFunctionType() != HelperTy){
    M.getFunctionList().erase(Helper32);
    Helper32 = nullptr;
  }
  if (!Helper32)
    Helper32 = cast<Function>(M.getOrInsertFunction(Helper32Name, HelperTy));

  Helper64->setLinkage(GlobalValue::LinkOnceAnyLinkage);
  // Give the function a body so it will get emitted.
  auto *BB = BasicBlock::Create(M.getContext(), "", Helper64);
  new UnreachableInst(M.getContext(), BB);

  MachineFunction &MF64 = MMI->getOrCreateMachineFunction(*Helper64);
  auto *MBB = MF64.CreateMachineBasicBlock();
  MF64.push_back(MBB);
  STI = &MF64.getSubtarget<X86Subtarget>();
  TII = STI->getInstrInfo();

  auto *Call64MBB = MF64.CreateMachineBasicBlock();
  auto *Call32MBB = MF64.CreateMachineBasicBlock();
  MF64.push_back(Call64MBB);
  MF64.push_back(Call32MBB);

  auto *CS32 = getExternalObject(M, CS32Name);
  auto *CS64 = getExternalObject(M, CS64Name);

  // Get the target address.
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::MOV32rm), X86::R8D),
               X86::EAX, /*isKill=*/false, 8);

  // First, check that the hotpatch signature is untouched (8b ff).
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::CMP16mi)),
               X86::R8D, /*isKill=*/false, 0)
      .addImm(0xff8b);
  // If it was hotpatched, then we need to invoke the hotpatched code.
  BuildMI(MBB, DebugLoc(), TII->get(X86::JNE_1)).addMBB(Call32MBB);

  // Now, check that the signature marking this as a thunk is present.
  // There's no CMP64mi with a 64-bit operand, so we need to load the
  // signature into a register first.
  BuildMI(MBB, DebugLoc(), TII->get(X86::MOV64ri), X86::R9)
      .addImm(0x77496e4554683332 /* 'wInETh32' */);
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::CMP64mr)),
               X86::R8D, /*isKill=*/false, -8)
      .addReg(X86::R9);
  // If this isn't one of ours, we can't invoke it 64-bit.
  BuildMI(MBB, DebugLoc(), TII->get(X86::JNE_1)).addMBB(Call32MBB);

  // Otherwise, this is a 64-bit function, and we can invoke it as such.
  //BuildMI(MBB, DebugLoc(), TII->get(X86::JMP_1)).addMBB(Call64MBB);

  // Now we can prepare the 64-bit call. Get the address of the actual
  // function. To do *that*, first we need to get the address of the 64-bit
  // side of the thunk. Exactly 10 bytes into the thunk is the offset of the
  // pointer from the thunk's PIC base, which is itself 7 bytes in. We should
  // be able to get the full address with one LEA.
  addRegOffset(BuildMI(Call64MBB, DebugLoc(), TII->get(X86::MOV32rm), X86::EAX),
               X86::R8D, /*isKill=*/false, 10);
  BuildMI(Call64MBB, DebugLoc(), TII->get(X86::LEA64_32r), X86::R8D)
      .addReg(X86::R8D, getKillRegState(true))
      .addImm(1)
      .addReg(X86::EAX)
      .addImm(7)
      .addReg(0);

  // Now, the RIP-relative offset to the actual function is precisely one
  // byte into the 64-bit side. Note that this offset is from the instruction
  // *following* the call--hence the '5'.
  addRegOffset(BuildMI(Call64MBB, DebugLoc(), TII->get(X86::MOV32rm), X86::EAX),
               X86::R8D, /*isKill=*/false, 1);
  BuildMI(Call64MBB, DebugLoc(), TII->get(X86::LEA64_32r), X86::R8D)
      .addReg(X86::R8D, getKillRegState(true))
      .addImm(1)
      .addReg(X86::EAX)
      .addImm(5)
      .addReg(0);

  // The target address is now in R8.
  // What happens next depends on whether or not we're expected to pop the
  // arguments.
  if (PopAmount) {
    // Here, we need to pop the stack, because the caller is expecting the
    // *32-bit callee* to do that, so we can't tail-call. So, adjust the stack,
    // call the target normally, then do a popping return.
    BuildMI(Call64MBB, DebugLoc(), TII->get(X86::SUB32ri8), X86::ESP)
        .addReg(X86::ESP)
        .addImm(4);
    BuildMI(Call64MBB, DebugLoc(), TII->get(X86::CALL64r)).addReg(X86::R8);
    BuildMI(Call64MBB, DebugLoc(), TII->get(X86::ADD32ri8), X86::ESP)
        .addReg(X86::ESP)
        .addImm(4);
    BuildMI(Call64MBB, DebugLoc(), TII->get(X86::RETIQ)).addImm(PopAmount);
  } else {
    // Adjust the stack, then tail-call the function.
    addRegOffset(BuildMI(Call64MBB,DebugLoc(),TII->get(X86::MOV64rm),X86::RAX),
                 X86::ESP, /*isKill=*/false, 8);
    BuildMI(Call64MBB, DebugLoc(), TII->get(X86::SUB32ri8), X86::ESP)
        .addReg(X86::ESP)
        .addImm(4);
    BuildMI(Call64MBB, DebugLoc(), TII->get(X86::PUSH64r))
        .addReg(X86::RAX, getKillRegState(true));
    BuildMI(Call64MBB, DebugLoc(), TII->get(X86::JMP64r)).addReg(X86::R8);
    // No need for a RET here.
  }

  // Now for the 32-bit invoke. But first, make sure we're inside the VM.
  // Otherwise, we can't make a 32-bit call.
  // FIXME: If we're not in the VM, then we should try to enter it.
  BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV16rs), X86::R8W)
      .addReg(X86::CS);
  unsigned OpFlags = STI->classifyGlobalReference(CS64);
  unsigned BaseReg = STI->isPICStyleRIPRel() ? X86::RIP : 0;
  if (!isGlobalStubReference(OpFlags)) {
    BuildMI(Call32MBB, DebugLoc(), TII->get(X86::CMP16rm), X86::R8W)
        .addReg(BaseReg)
        .addImm(1)
        .addReg(0)
        .addGlobalAddress(CS64, 0, OpFlags)
        .addReg(0);
  } else {
    BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV64rm), X86::R9)
        .addReg(BaseReg)
        .addImm(1)
        .addReg(0)
        .addGlobalAddress(CS64, 0, OpFlags)
        .addReg(0);
    BuildMI(Call32MBB, DebugLoc(), TII->get(X86::CMP16rm), X86::R8W)
        .addReg(X86::R9)
        .addImm(1)
        .addReg(0)
        .addImm(0)
        .addReg(0);
  }
  BuildMI(Call32MBB, DebugLoc(), TII->get(X86::JNE_1)).addMBB(Call64MBB);

  // We need to save the real return address, as well as RBX, RSI, and RDI.
  // The 32-bit code will likely overwrite their 32-bit counterparts, and
  // thereby clear their upper halves.
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::POP64rmm)),
               X86::EAX, /*isKill=*/false, 16);
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV64mr)),
               X86::EAX, /*isKill=*/false, 24)
      .addReg(X86::RBX);
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV64mr)),
               X86::EAX, /*isKill=*/false, 32)
      .addReg(X86::RSI);
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV64mr)),
               X86::EAX, /*isKill=*/false, 40)
      .addReg(X86::RDI);

  // Fetch both parts of the target address.
  BuildMI(Call32MBB, DebugLoc(), TII->get(X86::LEA64_32r), X86::R8D)
      .addReg(BaseReg)
      .addImm(1)
      .addReg(0)
      .addGlobalAddress(Helper32)
      .addReg(0);
  OpFlags = STI->classifyGlobalReference(CS32);
  if (!isGlobalStubReference(OpFlags)) {
    BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV16rm), X86::R9W)
        .addReg(BaseReg)
        .addImm(1)
        .addReg(0)
        .addGlobalAddress(CS32, 0, OpFlags)
        .addReg(0);
  } else {
    BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV64rm), X86::R9)
        .addReg(BaseReg)
        .addImm(1)
        .addReg(0)
        .addGlobalAddress(CS32, 0, OpFlags)
        .addReg(0);
    BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV16rm), X86::R9W)
        .addReg(X86::R9)
        .addImm(1)
        .addReg(0)
        .addImm(0)
        .addReg(0);
  }
  // Save the thunk data pointer while we're at it. This is a good time to
  // do it while the CPU waits for the memory accesses to complete.
  BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV32rr), X86::EBX)
      .addReg(X86::EAX);
  // Put the target far address onto the stack.
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV32mr)),
               X86::ESP, /*isKill=*/false, -4)
      .addReg(X86::R8D);
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV16mr)),
               X86::ESP, /*isKill=*/false, 0)
      .addReg(X86::R9W);

  // Make the call.
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::FARCALL32m)),
               X86::ESP, /*isKill=*/false, 0);

  // Restore the registers we saved, and the proper return address.
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::PUSH64rmm)),
               X86::EBX, /*isKill=*/false, 16);
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV64rm), X86::RDI),
               X86::EBX, /*isKill=*/false, 40);
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV64rm), X86::RSI),
               X86::EBX, /*isKill=*/false, 32);
  addRegOffset(BuildMI(Call32MBB, DebugLoc(), TII->get(X86::MOV64rm), X86::RBX),
               X86::EBX, /*isKill=*/true, 24);

  // Return to caller.
  BuildMI(Call32MBB, DebugLoc(), TII->get(X86::RETQ));

  // If we already defined the 32-bit part, we can return now.
  if (!Helper32->empty())
    return Helper64;

  Helper32->setLinkage(GlobalValue::LinkOnceAnyLinkage);
  // Give the function a body so it will get emitted.
  BB = BasicBlock::Create(M.getContext(), "", Helper32);
  new UnreachableInst(M.getContext(), BB);

  MachineFunction &MF32 = MMI->getOrCreateMachineFunction(*Helper32);
  MBB = MF32.CreateMachineBasicBlock();
  MF32.push_back(MBB);
  STI = &MF32.getSubtarget<X86Subtarget>();
  TII = STI->getInstrInfo();

  // Fetch the far return address, and save it in the thunk data.
  // We use "64-bit" instructions here, even though this is 32-bit code,
  // because we're still in 64-bit mode and we can't use the corresponding
  // 32-bit instructions here. Don't worry; these will encode the same as
  // their 32-bit counterparts, so this should work.
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::POP64rmm)),
               X86::EBX, /*isKill=*/false, 0);
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::POP64rmm)),
               X86::EBX, /*isKill=*/false, 4);

  // Now we're ready to near-call the target function.
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::CALL64m)),
               X86::EBX, /*isKill=*/false, 8);

  // Restore the original far return address. Luckily for us, the 64-bit
  // side was kind enough to stash the thunk data pointer in the non-volatile
  // EBX register.
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::PUSH64rmm)),
               X86::EBX, /*isKill=*/false, 4);
  addRegOffset(BuildMI(MBB, DebugLoc(), TII->get(X86::PUSH64rmm)),
               X86::EBX, /*isKill=*/false, 0);

  // Now return.
  BuildMI(MBB, DebugLoc(), TII->get(X86::LRETL));

  return Helper64;
}

GlobalValue *X866432InteropThunkInserter::getExternalObject(
    Module &M, StringRef Name) {
  auto *Value = M.getNamedValue(Name);
  if (Value)
    return Value;
  return new GlobalVariable(M, IntegerType::get(M.getContext(), 16),
                            false, GlobalValue::ExternalLinkage, nullptr,
                            Name, nullptr, GlobalVariable::NotThreadLocal, 32,
                            /*isExternallyInitialized=*/true);
}

bool X866432InteropThunkInserter::expandFarCallPseudos(Module &M, Function &Fn){
  MachineFunction &MF = MMI->getOrCreateMachineFunction(Fn);
  bool Modified = false;

  STI = &MF.getSubtarget<X86Subtarget>();
  TII = STI->getInstrInfo();

  StringRef Prefix = "__i386_on_x86_64_";
  if (Fn.hasFnAttribute("thunk-prefix"))
    Prefix = Fn.getFnAttribute("thunk-prefix").getValueAsString();
  StringRef CS32Name = "__i386_on_x86_64_cs32";
  if (Fn.hasFnAttribute("thunk-cs32-name"))
    CS32Name = Fn.getFnAttribute("thunk-cs32-name").getValueAsString();
  StringRef CS64Name = "__i386_on_x86_64_cs64";
  if (Fn.hasFnAttribute("thunk-cs64-name"))
    CS64Name = Fn.getFnAttribute("thunk-cs64-name").getValueAsString();

  for (auto &MBB : MF)
    Modified |= expandMBB(M, MBB, Prefix, CS32Name, CS64Name);
  return Modified;
}

bool X866432InteropThunkInserter::expandMBB(
    Module &M, MachineBasicBlock &MBB, StringRef Prefix,
    StringRef CS32Name, StringRef CS64Name) {
  bool Modified = false;

  // MBBI may be invalidated by the expansion.
  MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
  while (MBBI != E) {
    MachineBasicBlock::iterator NMBBI = std::next(MBBI);
    Modified |= expandMI(M, MBB, MBBI, Prefix, CS32Name, CS64Name);
    MBBI = NMBBI;
  }

  return Modified;
}

bool X866432InteropThunkInserter::expandMI(
    Module &M, MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
    StringRef Prefix, StringRef CS32Name, StringRef CS64Name) {
  MachineInstr &MI = *MBBI;
  unsigned Opcode = MI.getOpcode();

  if (Opcode != X86::FARCALL6432r && Opcode != X86::FARCALL6432m)
    return false;

  DebugLoc DL = MBBI->getDebugLoc();
  MachineOperand &JumpTarget = MBBI->getOperand(0);
  unsigned PopAmount =
      MBBI->getOperand(Opcode == X86::FARCALL6432m ? 5 : 1).getImm();

  // Jump to label or value in register.
  unsigned JumpAddrReg;
  if (Opcode == X86::FARCALL6432m) {
    MachineInstrBuilder MIB = BuildMI(MBB, MBBI, DL, TII->get(X86::MOV32rm));
    JumpAddrReg = X86::R8D;
    MIB.addReg(JumpAddrReg, RegState::Define);
    for (unsigned i = 0; i != 5; ++i)
      MIB.add(MBBI->getOperand(i));
  } else {
    JumpAddrReg = JumpTarget.getReg();
  }

  // Write the offset to the thunk data.
  addRegOffset(BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64mr)),
               X86::EAX, /*isKill=*/false, 8)
      .addReg(getX86SubSuperRegister(JumpAddrReg, 64), RegState::Kill);
  // If the helper doesn't exist, create it.
  Function *Helper = getOrInsertFarCallHelper(
      M, PopAmount, Prefix, CS32Name, CS64Name);
  // Call the helper.
  BuildMI(MBB, MBBI, DL, TII->get(X86::CALL64pcrel32))
      .addGlobalAddress(Helper);

  MachineInstr &NewMI = *std::prev(MBBI);
  NewMI.copyImplicitOps(*MBBI->getParent()->getParent(), *MBBI);

  // Delete the pseudo instruction.
  MBB.erase(MBBI);

  return true;
}

bool X866432InteropThunkInserter::runOnModule(Module &M) {
  bool Modified = false;

  MMI = &getAnalysis<MachineModuleInfo>();

  scanExistingAsmSymbols(M);
  for (auto &Fn : M) {
    // Don't use '||'. That'll short circuit if the first function returns
    // true.
    Modified |= generateThunks(M, Fn);
    Modified |= expandFarCallPseudos(M, Fn);
  }

  return Modified;
}
