import sys
from typing import Callable
from dataclasses import dataclass
import regex

def reCompile(p:str, flags:regex.RegexFlag=regex.DOTALL) -> regex.Pattern:
    return regex.compile(p, flags)

NAME_CHAR_RE_STR = r"[A-Za-z0-9_]"
FUNCALL_RE = reCompile(r"([A-Za-z_][A-Za-z0-9_]*)\(((?:[^()]+|(?0))*)\)")
DEFINED_LITERAL_RE = regex.compile(r"^#define (.+?) (.+?)$", regex.MULTILINE)
VAR_TYPE_RE_STR = VAR_NAME_RE_STR = r"[A-Za-z_][A-Za-z0-9_\(\)]*(?:\*+)?"
VAR_TYPE_AND_NAME_RE_STR = VAR_TYPE_RE_STR + r"\." + VAR_NAME_RE_STR
TO_DEFINE_ARGS_RE_STR = r"(?:{0}(?:,{0})*)?".format(VAR_TYPE_AND_NAME_RE_STR)
LITERAL_RE = reCompile(r"\"(?:\\.|[^\"\\])*\"|'(?:\\.|[^'\\])*'|(?<![A-Za-z_\"])\\d+(?![A-Za-z_])")

FIRST_SEPARATIONS_FOR_LITERALS_STR = r"(?<!"+NAME_CHAR_RE_STR+")"
SECOND_SEPARATIONS_FOR_LITERALS_STR = r"(?!"+NAME_CHAR_RE_STR+")"
FUNC_BODY_STR = r"(?P<body>(?:[^{}]|{(?&body)})*)"
VOID_VOID_FUNC_RE = reCompile(r"{"+FUNC_BODY_STR+r"}")
FUNC_RE = reCompile(
    r"(" + VAR_TYPE_RE_STR + r"):\((" + TO_DEFINE_ARGS_RE_STR + r")\){" + FUNC_BODY_STR + r"}"
)
MAIN_FUNC_RE = reCompile(
    r"main:"+FUNC_RE.pattern
)
IMPORT_RE = reCompile(r"import\((.*?)\)")
COMMENT_RE = reCompile(r"//.*?$|/\*(.*?)\*/", regex.MULTILINE|regex.DOTALL)

@dataclass
class SplitResult:
    text:str
    splitTarget:str

class Func:
    def __init__(self, name:str, minArgsLength:int, body:str, dontAddSemicolon:bool) -> None:
        self._name:str = name
        self._minArgsLength:int = minArgsLength
        self._body:Callable[[str], str|None] = body
        self._dontAddSemicolon:bool = dontAddSemicolon
    def getName(self) -> str:
        return self._name
    def getMinArgsLength(self) -> int:
        return self._minArgsLength
    def getBody(self) -> Callable[[str], str|None]:
        return self._body
    def getDontAddSemicolon(self) -> bool:
        return self._dontAddSemicolon

class Funcs:
    _funcs: list[Func] = []
    @classmethod
    def addFunc(cls, func:Func) -> None:
        cls._funcs.append(func)
    @classmethod
    def getFuncs(cls) -> list[Func]:
        return cls._funcs
    @classmethod
    def getFuncByName(cls, name:str) -> Func|None:
        for f in cls._funcs:
            if regex.compile(f.getName()).fullmatch(name):
                return f
    @classmethod
    def getFuncByBlock(cls, block:str) -> Func|None:
        if m := FUNCALL_RE.fullmatch(block):
            return cls.getFuncByName(m.group(1))
        else:
            raise ValueError("Invalid function call block")

class Compiled:
    baseCode:str = ""
    nowCode:str = ""
    literalDefineds:str = ""
    funcDefineds:str = ""
    nowLiteralId:int = 0
    nowFuncId:int = 0
    importFiles:list[str] = []
    includes:list[str] = []
    funcLinkeds:str = ""
    structDefineds:str = ""
    output:str = ""
    @classmethod
    def getAndCountUpNewLiteralId(cls):
        cls.nowLiteralId += 1
        return cls.nowLiteralId - 1
    @classmethod
    def getAndCountUpNewFuncId(cls):
        cls.nowFuncId += 1
        return cls.nowFuncId - 1

class BlockOptimizer:
    @classmethod
    def splitBlocks(cls, code:str) -> list[str]:
        blocks = []
        depth = 0
        appended = False
        for c in code:
            if depth == 0 and not appended:
                blocks.append("")
                appended = True
            if c in ["{", "("]:
                depth += 1
                appended = False
            elif c in ["}", ")"]:
                depth -= 1
            blocks[-1] += c
        return blocks
    @classmethod
    def parse(cls, block:str, dontAddSemicolon:bool=False) -> str:
        block = block.strip()
        if block == "":
            return ""
        try:
            func = Funcs.getFuncByBlock(block)
            if not func:
                raise ValueError("Function not found")
            m = FUNCALL_RE.match(block)
            args = [arg.strip() for arg in m.group(2).split(",")]
            if len(args) < func.getMinArgsLength():
                raise ValueError(f"In func {func.getName()} Expected {func.getMinArgsLength()} arguments, got {len(args)}")
            body = func.getBody()
            r = body(block)
            dontAddSemicolon = dontAddSemicolon or func.getDontAddSemicolon()
            block = r
            return block + (";\n" if not dontAddSemicolon else "")
        except ValueError as e:
            pass
        blocks = cls.splitST(block, pattern=r"(\~|\.)")
        if len(blocks) > 1:
            block = ""
            for b in blocks:
                block += cls.parse(b.text, dontAddSemicolon=True) + b.splitTarget
        block = block.replace("~", "->").replace(">", ".")
        if m := FUNCALL_RE.match(block):
            funcName = m.group(1)
            args = [cls.parse(arg.strip(), dontAddSemicolon=True) for arg in cls.getArgs(m.group(0))]
            block = f"{funcName}(" + (", ".join(args)) + ")"
        return block + (";\n" if not dontAddSemicolon else "")
    @classmethod
    def parses(cls, code:str) -> str:
        return "".join([pd for pd in [cls.parse(b) for b in cls.splitBlocks(regex.sub(r"(\A{|}\Z)", "", code.replace(" ", ""))) if b] if pd != ""])
    @classmethod
    def optimizeLiterals(cls, code:str) -> tuple[str, str]:
        defines = ""
        for m in LITERAL_RE.finditer(code):
            lit = m.group(0)
            literalName = f"LITERAL_{Compiled.getAndCountUpNewLiteralId()}"
            defines += f"#define {literalName} {lit}\n"
            code = regex.sub(rf"{FIRST_SEPARATIONS_FOR_LITERALS_STR}({regex.escape(lit)}){SECOND_SEPARATIONS_FOR_LITERALS_STR}", literalName, code)
        return code, defines
    @classmethod
    def optimizeFuncs(cls, code:str, funcId:int|str=None) -> tuple[str, str]:
        def gN() -> str:
            nonlocal funcId
            i = Compiled.getAndCountUpNewFuncId() if funcId is None else funcId
            return ("FUNC_" if isinstance(i, int) else "") + str(i)
        finds = []
        funcDefineds = ""
        mainFuncDefined = ""
        for f in MAIN_FUNC_RE.finditer(code):
            code = code.replace(f.group(0), "")
            c, ds = cls.optimizeFuncs(regex.sub("^main:", "", f.group(0)), "main")
            mainFuncDefined = ds
        for f in FUNC_RE.finditer(code):
            if f.group(0) not in finds:
                finds.append(f.group(0))
            else:
                continue
            funcName = gN()
            returnType = cls.typeOrValNameParse(cls.parse(f.group(1), dontAddSemicolon=True))
            argsVars = [cls.typeOrValNameParse(cls.parse(v, dontAddSemicolon=True)) for v in f.group(2).split(",") if v]
            c, ds = cls.optimizeFuncs(f.group(3))
            funcDefineds += ds
            content = cls.parses(c)
            funcDefineds += f"{returnType} {funcName}({', '.join(argsVars)}) "+"{"+content+"}\n"
            code = code.replace(f.group(0), f"{funcName}")
        return code, funcDefineds+mainFuncDefined
    @classmethod
    def splitST(cls, s:str, pattern:str=",") -> list[SplitResult]:
        sts = []
        current = []
        depth = 0
        for ch in s:
            if ch == '(':
                depth += 1
            elif ch == ')':
                depth -= 1
            elif regex.match(pattern, ch) and depth == 0:
                sts.append(SplitResult(''.join(current), ch))
                current = []
                continue
            current.append(ch)
        sts.append(SplitResult(''.join(current), ""))
        return sts
    @classmethod
    def getArgs(cls, block:str, minLength:int=0) -> list[str]:
        m = FUNCALL_RE.match(block)
        if not m:
            raise ValueError("Not a function call")
        args = [b.text for b in BlockOptimizer.splitST(m.group(2))]
        if len(args) == 1 and args[0] == "":
            args = []
        if len(args) < minLength:
            raise ValueError(f"In func {m.group(1)} Expected {minLength} arguments, got {len(args)}")
        return args
    @classmethod
    def separateVarTypeAndName(cls, s:str) -> tuple[str, str]:
        sp = s.split(".")
        return ".".join(sp[0:-1]), sp[-1]
    @classmethod
    def typeOrValNameParse(cls, s:str) -> str:
        return s.replace(".", " ")
    @classmethod
    def getDefinedFuncByName(cls, name:str) -> str|None:
        m = regex.search(rf"(\w+\s+)+{regex.escape(name)}\s*\((?:[^\(\)]|(?0))*\)\s*\{{(?:[^{{}}]|(?0))*\}}", Compiled.funcDefineds)
        if m:
            return m.group(0)
        return None
    @classmethod
    def optimizeImports(cls, code:str) -> str:
        while True:
            m = IMPORT_RE.search(code)
            if not m:
                break
            fileName = m.group(1).strip()
            if fileName in Compiled.importFiles:
                code = code[:m.start()] + code[m.end():]
                continue
            Compiled.importFiles.append(fileName)
            try:
                with open(fileName, mode="r") as f:
                    importedCode = f.read()
            except FileNotFoundError:
                raise FileNotFoundError(f"File not found: {fileName}")
            code = code[:m.start()] + importedCode + code[m.end():]
            code = cls.deleteComments(code)
        return code
    @classmethod
    def deleteComments(cls, code:str) -> str:
        return regex.sub(COMMENT_RE, "", code)

class BuiltInFuncs:
    @staticmethod
    def cinclude(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        includeFile = args[0]
        if includeFile not in Compiled.includes:
            Compiled.includes.append(includeFile)
        return ""
    @staticmethod
    def cfunc(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        funcName = args[0]
        if len(args) > 1:
            args = [BlockOptimizer.parse(a, dontAddSemicolon=True) for a in args[1:]]
            return f"{funcName}("+(", ".join(args)) + ")"
        else:
            return f"{funcName}()"
    @staticmethod
    def lifunc(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        linkedName, sourceName = args
        Compiled.funcLinkeds += f"#define {linkedName} {sourceName}\n"
        return ""
    @staticmethod
    def devar(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        var = BlockOptimizer.parse(args[0], dontAddSemicolon=True)
        varType, varName = BlockOptimizer.separateVarTypeAndName(var)
        varType = BlockOptimizer.typeOrValNameParse(varType)
        if len(args) >= 2:
            initVal = BlockOptimizer.parse(args[1], dontAddSemicolon=True)
            return f"{varType} {varName} = {initVal}"
        else:
            return f"{varType} {varName}"
    @staticmethod
    def toeq(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        varName = BlockOptimizer.parse(args[0], dontAddSemicolon=True)
        value = BlockOptimizer.parse(args[1], dontAddSemicolon=True)
        return f"{varName} = {value}"
    @staticmethod
    def whileF(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        cond = BlockOptimizer.parse(args[0], dontAddSemicolon=True)
        body = BlockOptimizer.parses(args[1])
        return f"while({cond})"+"{"+body+"}"
    @staticmethod
    def forF(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        init = BlockOptimizer.parse(args[0], dontAddSemicolon=True)
        cond = BlockOptimizer.parse(args[1], dontAddSemicolon=True)
        step = BlockOptimizer.parse(args[2], dontAddSemicolon=True)
        body = BlockOptimizer.parses(args[3])
        return f"for({init}; {cond}; {step})"+"{"+body+"}"
    @staticmethod
    def returnF(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        if len(args) >= 1:
            retVal = ", ".join([BlockOptimizer.parse(v, dontAddSemicolon=True) for v in args])
            return f"return ({retVal})"
        else:
            return "return"
    @staticmethod
    def cast(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        toType = BlockOptimizer.typeOrValNameParse(BlockOptimizer.parse(args[0], dontAddSemicolon=True))
        varName = BlockOptimizer.parse(args[1], dontAddSemicolon=True)
        return f"(({toType})({varName}))"
    @staticmethod
    def operator(block:str) -> str|None:
        args = BlockOptimizer.getArgs(block)
        a = BlockOptimizer.parse(args[0], dontAddSemicolon=True)
        funcName = block.split("(")[0]
        if len(args) == 1:
            match funcName:
                case "bnot":
                    return f"(~{a})"
                case "ptr":
                    return f"*{a}"
                case "refptr":
                    return f"&{a}"
                case "deptr":
                    return f"{a}*"
        args = BlockOptimizer.getArgs(block, 2)
        b = BlockOptimizer.parse(args[1], dontAddSemicolon=True)
        op = {
            "band": "&",
            "bor": "|",
            "bxor": "^",
            "blshift": "<<",
            "brshift": ">>",
            "iseq": "==",
            "islt": "<",
            "isgt": ">",
        }.get(funcName, None)
        if op:
            return f"({a} {op} {b})"
        op = {"add": "+",
            "sub": "-",
            "mul": "*",
            "div": "/",
            "mod": "%"
        }.get(funcName, None)
        if op:
            parseds = [BlockOptimizer.parse(arg, dontAddSemicolon=True) for arg in args]
            return f"("+(f" {op} ".join(parseds))+")"
        return ""
    @staticmethod
    def ifsF(block:str) -> str:
        args = BlockOptimizer.getArgs(block, 1)
        if block.startswith("else"):
            body = BlockOptimizer.parses(args[0])
            return f"else"+"{"+body+"}"
        args = BlockOptimizer.getArgs(block, 2)
        cond = BlockOptimizer.parse(args[0], dontAddSemicolon=True)
        body = BlockOptimizer.parses(args[1])
        funcName = block.split("(")[0].replace("elif", "else if")

        return f"{funcName}({cond})"+"{"+body+"}"
    @staticmethod
    def struct(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        structName = args[0]
        body = BlockOptimizer.parses(args[1])
        Compiled.structDefineds += f"typedef struct {structName} "+"{"+body+"} "+f"{structName};\n"
        return ""
    @staticmethod
    def index(block:str) -> str:
        args = BlockOptimizer.getArgs(block)
        arrayName = BlockOptimizer.parse(args[0], dontAddSemicolon=True)
        index = BlockOptimizer.parse(args[1], dontAddSemicolon=True) if len(args) >=2 else ""
        return f"{arrayName}[{index}]"
    @staticmethod
    def controlState(block:str) -> str:
        return block.split("(")[0]

class FunCompiler:
    @staticmethod
    def compile(code:str) -> str:
        code = BlockOptimizer.deleteComments(code)
        code = BlockOptimizer.optimizeImports(code)
        code, Compiled.literalDefineds = BlockOptimizer.optimizeLiterals(code)
        code = code.replace(" ", "")
        
        code, Compiled.funcDefineds = BlockOptimizer.optimizeFuncs(code)
        BlockOptimizer.parses(code)
        finalCode = ""
        if Compiled.includes:
            for inc in Compiled.includes:
                finalCode += f"#include {inc}\n"
        finalCode += Compiled.structDefineds + "\n"
        finalCode += Compiled.funcLinkeds + "\n"
        finalCode += Compiled.literalDefineds + "\n"
        finalCode += Compiled.funcDefineds + "\n"

        return finalCode
    @staticmethod
    def gccCompile(code:str, outputFileName:str="a.out") -> None:
        with open(".tmp.c", mode="w") as f:
            f.write(code)
        import subprocess
        subprocess.run(["g++", "-std=c++20", ".tmp.c", "-o", outputFileName])
        import os
        os.remove(".tmp.c")

def _init():
    Funcs.addFunc(Func(
        name="cinclude",
        minArgsLength=1,
        body=BuiltInFuncs.cinclude,
        dontAddSemicolon=True
    ))
    Funcs.addFunc(Func(
        name="cfunc",
        minArgsLength=1,
        body=BuiltInFuncs.cfunc,
        dontAddSemicolon=False
    ))
    Funcs.addFunc(Func(
        name="lifunc",
        minArgsLength=2,
        body=BuiltInFuncs.lifunc,
        dontAddSemicolon=True
    ))
    Funcs.addFunc(Func(
        name="devar",
        minArgsLength=1,
        body=BuiltInFuncs.devar,
        dontAddSemicolon=False
    ))
    Funcs.addFunc(Func(
        name="toeq",
        minArgsLength=2,
        body=BuiltInFuncs.toeq,
        dontAddSemicolon=False
    ))
    Funcs.addFunc(Func(
        name="while",
        minArgsLength=2,
        body=BuiltInFuncs.whileF,
        dontAddSemicolon=True
    ))
    Funcs.addFunc(Func(
        name="for",
        minArgsLength=4,
        body=BuiltInFuncs.forF,
        dontAddSemicolon=True
    ))
    Funcs.addFunc(Func(
        name="return",
        minArgsLength=0,
        body=BuiltInFuncs.returnF,
        dontAddSemicolon=False
    ))
    Funcs.addFunc(Func(
        name="cast",
        minArgsLength=2,
        body=BuiltInFuncs.cast,
        dontAddSemicolon=True
    ))
    Funcs.addFunc(Func(
        name="(bnot|band|bor|bxor|blshift|brshift|iseq|islt|isgt|add|sub|mul|div|mod|ptr|refptr|deptr)",
        minArgsLength=1,
        body=BuiltInFuncs.operator,
        dontAddSemicolon=True
    ))
    Funcs.addFunc(Func(
        name="(if|elif|else)",
        minArgsLength=1,
        body=BuiltInFuncs.ifsF,
        dontAddSemicolon=True
    ))
    Funcs.addFunc(Func(
        name="struct",
        minArgsLength=2,
        body=BuiltInFuncs.struct,
        dontAddSemicolon=False
    ))
    Funcs.addFunc(Func(
        name="index",
        minArgsLength=1,
        body=BuiltInFuncs.index,
        dontAddSemicolon=True
    ))
    Funcs.addFunc(Func(
        name="(continue|break)",
        minArgsLength=0,
        body=BuiltInFuncs.controlState,
        dontAddSemicolon=False
    ))
def main():
    _init()
    if len(sys.argv) < 2:
        print("Usage: python ka.py <input_file> <output_file> [--emit-c]")
        sys.exit(1)
    inputFile = sys.argv[1]
    outputFile = sys.argv[2] if len(sys.argv) >= 3 else "a.out"
    with open(inputFile, mode="r") as f:
        output = FunCompiler.compile(f.read())
    if len(sys.argv) >= 4 and sys.argv[3] == "--emit-c":
        with open(outputFile, mode="w") as f:
            f.write(output)
        return
    FunCompiler.gccCompile(output, outputFile)


if __name__ == "__main__":
    main()