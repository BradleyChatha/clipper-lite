from clipper.ast import clipper_parser
from clipper.transformer import ToLua
from subprocess import run

text = r"""
"""

print(clipper_parser.parse(text).pretty())

toLua = ToLua()
print(toLua.transform(clipper_parser.parse(text)))

with open("temp.lua", "w") as f:
    f.write(toLua.code)
run(["lua", "temp.lua"])
