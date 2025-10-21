#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

    struct SkeletonPass : public PassInfoMixin<SkeletonPass> {
        PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
            bool Changed = false;
            SmallVector<Instruction*, 16> ToErase;
            for (auto &F : M) {
                for (auto& B : F) {
                    for (auto& I : B) {
                        if (auto* op = dyn_cast<BinaryOperator>(&I)) {
                            if (op->getOpcode() == Instruction::And) {
                                IRBuilder<> builder(op);

                                Value* lhs = op->getOperand(0);
                                Value* rhs = op->getOperand(1);

                                Value* xor1 = builder.CreateXor(lhs, rhs);
                                Value* or1 = builder.CreateOr(lhs, rhs);
                                Value* xor2 = builder.CreateXor(xor1, or1);

                                op->replaceAllUsesWith(xor2);
                                ToErase.push_back(op);
                                Changed = true;
                            }
                        }
                    }
                }
            }
            for (Instruction *Inst : ToErase) 
                Inst->eraseFromParent();
            for (auto &F : M) {
                for (auto& B : F) {
                    for (auto& I : B) {
                        errs() << "Instruction: " << I << "!\n";
                    }
                }
            }
            //return PreservedAnalyses::all();
            return (Changed ? PreservedAnalyses::none() : PreservedAnalyses::all());
        };
    };

}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
    return {
        .APIVersion = LLVM_PLUGIN_API_VERSION,
            .PluginName = "Skeleton pass",
            .PluginVersion = "v0.1",
            .RegisterPassBuilderCallbacks = [](PassBuilder &PB) {
                PB.registerPipelineStartEPCallback(
                        [](ModulePassManager &MPM, OptimizationLevel Level) {
                        MPM.addPass(SkeletonPass());
                        });
            }
    };
}
