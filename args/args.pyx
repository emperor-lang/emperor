import argparse
from datetime import date
import os.path
import json
import sys

STDIN_FLAG = '-'
emperorVersion:str = '0.1.0'
emperorVersionString:str = f'emperor {emperorVersion}\nWritten by Edward Jones\n\nCopyright (C) 2019 Edward Jones\nThis is free software; see the source for copying conditions.  There is NO\nwarranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'

class EmperorArgumentParser:
	# def __cinit__(self):
	# 	print('Hello, world!')

	def parse(self, args:[str]) -> dict:
		print('Hello2')
		return {}

# cdef class EmperorArgumentParser:
# 	def __cinit__(self):
# 		self.parser = ArgumentParserWithJson(
# 			licence=[
# 				'Copyright (c) 2019, Edward Jones\n',
# 				'\n',
# 				'%%%%%%LICENSE_START(GPLv2+_DOC_FULL)\n',
# 				'This is free documentation; you can redistribute it and/or\n',
# 				'modify it under the terms of the GNU General Public License as\n',
# 				'published by the Free Software Foundation; either version 2 of\n',
# 				'the License, or (at your option) any later version.\n',
# 				'\n',
# 				'The GNU General Public License\'s references to "object code"\n',
# 				'and "executables" are to be interpreted as the output of any\n',
# 				'document formatting or typesetting system, including\n',
# 				'intermediate and printed output.\n',
# 				'\n',
# 				'This manual is distributed in the hope that it will be useful,\n',
# 				'but WITHOUT ANY WARRANTY; without even the implied warranty of\n',
# 				'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n',
# 				'GNU General Public License for more details.\n',
# 				'\n',
# 				'You should have received a copy of the GNU General Public\n',
# 				'License along with this manual; if not, see\n',
# 				'<http://www.gnu.org/licenses/>.\n',
# 				'%%%%%%LICENSE_END'
# 			],
# 			version = emperorVersion,
# 			description = f'''Compiler for the Emperor language v{emperorVersion}''',
# 			seeAlso = ['gcc(1)', 'dux(1)', 'bison(1)', 'flex(1)'],
# 			bugs = f'''There are no known bugs at this time! :D If you find any, however, please report them at <https://github.com/TheSignPainter98/emperor/issues>''',
# 			epilog = '''This is maintained by Edward Jones, and source code can be found at <https://github.com/TheSignPainter98/emperor>'''
# 		)	
# 		parser.add_argument('-v', '--verbose', action='store_true', description='Output verbosity')
# 		parser.add_argument('-V', '--version', action='store_true', description='Output version and exit')
# 		parser.add_argument('-O', '--optimisation', choices=[ 's', '0', '1', '2' ], default=0, dest='optimisation', description='Compiler optimisation level as speficied in \\fBgcc\\fP')
# 		parser.add_argument('-c', '--to-c', action='store_true', dest='compileCOnly', description='Translate to C (skips compilation step)')
# 		parser.add_argument('-o', '--output', type=str, dest='outputFile', metavar='OUTPUT_FILE', default=None, action='store', description='Specify output file')
# 		parser.add_argument('files', metavar='file', type=str, nargs='*', description=f'A file to compile, use {STDIN_FLAG} to read from stdin')

# 	cpdef dict parse(self, args:[str]):
# 		return {}
	
# # 	return {'x', 1} # parser.parse_args(args).__dict__


# cdef class ArgumentParserWithJson:
# 	def __cinit__(self, licence:[str]=[], version:str=None, seeAlso:[str]=[], date:str=date.today().strftime('%d %B %Y'), bugs:str=None):
# 		# prog=None,
# 		# usage=None,
# 		# description=None,
# 		# epilog=None,
# 		# parents=[],
# 		# formatter_class=HelpFormatter,
# 		# prefix_chars='-',
# 		# fromfile_prefix_chars=None,
# 		# argument_default=None,
# 		# conflict_handler='error',
# 		# add_help=True,
# 		# allow_abbrev=True):
# 		self.acceptibleArguments = [
# 			'name'
# 			'action'
# 			'nargs'
# 			'const'
# 			'default'
# 			'type'
# 			'choices'
# 			'required'
# 			'help'
# 			'metavar'
# 			'dest'
# 		]
# 		self.licence = licence
# 		self.version = version
# 		self.date = date
# 		self.seeAlso = seeAlso
# 		self.bugs = bugs
# 		self.add_argument('-*', '--arg-spec', dest='arg_spec', action='store_true', help='Output a json specification for the arguments')

# 	cdef void add_argument(self, dict arg):
# 		if not arg.keys().issubset(self.acceptibleArguments):
# 			print('Bad keys: %s' % str(arg.keys().difference(self.acceptibleArguments)))
# 		self.arguments.append(arg)

# 	cpdef dict parse_args(self, args:[str]):
# 		parser:argparse.ArgumentParser = argparse.ArgumentParser()
# 		for argument in self.arguments:
# 			# There must be a better way of doing this
# 			if len(argument['optionStrings']) == 1:
# 				parser.add_argument(
# 					argument['optionStrings'][0],
# 					name = argument['name'],
# 					action = argument['action'],
# 					nargs = argument['nargs'],
# 					const = argument['constant'],
# 					default = argument['default'],
# 					type = argument['type'],
# 					choices = argument['choices'],
# 					required = argument['required'],
# 					help = argument['help'],
# 					metavar = argument['metavar'],
# 					dest = argument['dest']
# 				)
# 			elif len(argument['optionStrings']) == 2:
# 				parser.add_argument(
# 					argument['optionStrings'][0],
# 					argument['optionStrings'][1],
# 					name = argument['name'],
# 					action = argument['action'],
# 					nargs = argument['nargs'],
# 					const = argument['constant'],
# 					default = argument['default'],
# 					type = argument['type'],
# 					choices = argument['choices'],
# 					required = argument['required'],
# 					help = argument['help'],
# 					metavar = argument['metavar'],
# 					dest = argument['dest']
# 				)
# 			else:
# 				print('Invalid number of option strings: %d' % len(argument['optionStrings']), file=sys.stderr)
# 		nameSpace:argparse.Namespace = parser.parse_args(args)
# 		if nameSpace.arg_spec:
# 			print(self.toJson())
# 		return nameSpace

# 	cpdef str toJson(self):
# 		allowedKeys:[str] = ['licence', 'version', 'date', 'seeAlso', 'bugs', 'prog', 'description', 'epilog']
# 		keys:[str] = [key for key in self.__dict__ if key in allowedKeys]
# 		options:dict = {}
# 		for key in keys:
# 			options[key] = self.__dict__[key]

# 		keyOptions:dict = {} 
# 		actions = self.__dict__['_actions']
# 		optionList:[] = []
# 		seenOptionStrings:[str] = []
# 		for action in actions:
# 			optionString:str = action.option_strings[0] if 'option_strings' in action.__dict__ and len(action.option_strings) >= 1 else ''
# 			if optionString == '' or optionString not in seenOptionStrings:
# 				optionList.append({k: action.__dict__[k] for k in [ k for k in action.__dict__ if k not in ['container', 'type'] ] })
# 			seenOptionStrings.append(optionString)
# 		options['options'] = optionList
# 		return json.dumps(options)
