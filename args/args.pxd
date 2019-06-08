from argparse import ArgumentParser, Namespace
# import argparse
from datetime import date
import inspect
import os.path
import json

cdef class EmperorArgumentParser:
	cpdef dict parse(self, args:[str])
# # 	cdef readonly ArgumentParserWithJson parser


# 	cpdef dict parse(self, args:[str])

# cdef class ArgumentParserWithJson:
# 	cdef list argumentsnbvcb
# 	cdef str licence
# 	cdef str version
# 	cdef str date
# 	cdef str seeAlso
# 	cdef str bugs
# 	cdef readonly list acceptibleArguments

# 	cpdef dict parse_args(self, args:[str])
# 	cdef void add_argument(self, dict arg)
# 	cpdef str toJson(self)
	