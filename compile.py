import os
import sys

srcPath = ""
exePath = "~/.config/sharkc/sharkc"

fileFormat = "macho64"
output = "a.out"
i = 1
while i < len(sys.argv):
    arg = sys.argv[i]
    if arg == "-f" or arg == "--format":
        i += 1
        fileFormat = sys.argv[i]
    elif arg == "-o" or arg == "--output":
        i += 1
        output = sys.argv[i]
    elif arg == "-h" or arg == "--help":
        os.system(exePath + "-h")
        exit(0)
    elif arg == "-m" or arg == "--mir":
        os.system(exePath + " " + srcPath + " -m")
        exit(0)
    elif arg == "-i" or arg == "--ir":
        os.system(exePath + " " + srcPath + " -i")
        exit(0)
    elif arg == "-a" or arg == "--ast":
        os.system(exePath + " " + srcPath + " -a")
        exit(0)
    elif arg == "-t" or arg == "--tokens":
        os.system(exePath + " " + srcPath + " -t")
        exit(0)
    else:
        srcPath = sys.argv[i]
    i += 1

asmOutput = "~/.config/sharkc/{}.asm".format(output)
objOutput = "~/.config/sharkc/{}.o".format(output)

if os.system("{} {} -f {} -o {}".format(exePath, srcPath, fileFormat, asmOutput)) != 0:
    os.system("rm -f {} {}".format(asmOutput, objOutput))
    exit(101)
if os.system("nasm {} -f {} -o {}".format(asmOutput, fileFormat, objOutput)) != 0:
    os.system("rm -f {} {}".format(asmOutput, objOutput))
    exit(102)
if os.system("gcc {} -o {} > /dev/null 2>&1".format(objOutput, output)) != 0:
    print("linker exits with a non-zero exit code:")
    os.system("gcc {} -o {}".format(objOutput, output))
    os.system("rm -f {} {}".format(asmOutput, objOutput))
    exit(102)

os.system("rm -f {} {}".format(asmOutput, objOutput))
