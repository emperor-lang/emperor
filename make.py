#!/usr/bin/python3

from distutils.core import setup
from Cython.Build import cythonize
from distutils.extension import Extension

sourcefiles = ['./parser/emperor.tab.c', './parser/emperor.yy.c', './emperor.pyx']
extensions = [Extension('emperor', sourcefiles)]
setup(
	ext_modules = cythonize(extensions, language_level=3)
)
