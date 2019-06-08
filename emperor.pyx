# import args.args 
import sys

emperorVersion:str = '0.1.0'
emperorVersionString:str = f'emperor {emperorVersion}\nWritten by Edward Jones\n\nCopyright (C) 2019 Edward Jones\nThis is free software; see the source for copying conditions.  There is NO\nwarranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'

################################################################################
import argparse
from datetime import date
import inspect
import os.path
import json

STDIN_FLAG = '-'

class TroffArgumentParser(argparse.ArgumentParser):
	def __init__(self, licence:[str]=[], version:str=None, seeAlso:[str]=[], date:str=date.today().strftime('%d %B %Y'), bugs:str=None, *args, **kwargs):
		super(TroffArgumentParser, self).__init__(*args, **kwargs)
		self.licence = licence
		self.version = version
		self.date = date
		self.seeAlso = seeAlso
		self.bugs = bugs

	def toTroff(self) -> str:
		options:[argparse.Action] = self._actions
		options.sort(key=lambda option : self._checkMandatory(option))

		troffOutput:str = self._header(self.prog, self.date, self.version, self.licence)
		troffOutput += self._name(self.prog, self.description)
		troffOutput += self._synopsis(self.prog, options)
		troffOutput += self._description(self.description)
		troffOutput += self._options(options)
		troffOutput += self._seeAlso(self.seeAlso)
		troffOutput += self._bugs(self.bugs)
		troffOutput += self._authors(self.epilog)
		return troffOutput

	def _checkMandatory(self, action:argparse.Action) -> bool:
		return True if action.option_strings == [] else False

	def _header(self, program:str, date:str, version:str, licence:[str]) -> str:
		licenceString:str = '\n'.join(list(map(lambda line: r'.\" ' + line)))
		return f'{licenceString}\n.TH {program.upper()} 1 "{date}" "{program} {version}" "User Commands" "fdsa"\n'

	def _name(self, prog:str, description:str) -> str:
		return f'.SH "NAME"\n\\fB{prog}\\fP - {description}\n'

	def _synopsis(self, prog:str, options:[argparse.Action]) -> str:
		return f'.SH "SYNOPSIS"\n\\fB{prog}\\fP [OPTION]... [file... | -]\n'

	def _description(self, description:str) -> str:
		if description != '':
			return f'.SH "DESCRIPTION"\n{description}\n'
		else:
			return ''

	def _options(self, options:[argparse.Action]) -> str:
		description:str = f'.SH "OPTIONS"'
		firstOption:bool = True
		previousWasMandatory:bool = False
		mandatory:bool = False
		for option in options:
			mandatory = self._checkMandatory(option)
			if firstOption:
				if mandatory:
					description += f'\n.SS "Mandatory arguments"'
				else:
					description += f'\n.SS "Optional arguments"'
			elif mandatory and not previousWasMandatory:
				description += f'\n.SS "Mandatory arguments"'
				doingMandatory = True
			# 	switchToPrintingMandatoryOptions = False
			# if not self._checkMandatory(option):
			# 	switchToPrintingMandatoryOptions = True
			optionString:str = self._formatList(option.option_strings) if option.option_strings != [] else option.metavar
			optionString = r'\fB' + optionString + r'\fP '
			if option.choices is not None:
				choices:str = self._formatList(option.choices, separator=',')
				optionString += r'\fI{' + choices + r'}\fP'
			elif option.nargs is None:
				optionString += r'\fI' + (option.metavar.upper() if option.metavar is not None else option.dest.upper()) + r'\fP'
				pass
			elif option.nargs == '*':
				pass
			elif option.nargs == '+':
				pass
			elif int(option.nargs) >= 0:
				pass

			optionDescription:str = f'\n.TP\n{optionString}\n{option.help}'
			description += optionDescription
			firstOption = False
			previousWasMandatory = self._checkMandatory(option)
		return description + '\n'

	def _formatList(self, lst:[str], separator:str=', ') -> str:
		formattedList:str = ''
		if lst is not None:
			formattedList = ''
			firstItem:bool = True
			for item in lst:
				if not firstItem:
					formattedList += separator
				firstItem = False
				formattedList += item
		return formattedList

	def _seeAlso(self, seeAlso:[str]) -> str:
		if seeAlso == []:
			return ''
		else:
			seeAlsos:str = ', '.join(seeAlso)
			return f'.SH "SEE ALSO"\n{seeAlsos}\n'

	def _bugs(self, bugs:str) -> str:
		return f'.SH "BUGS"\n{bugs}\n' if bugs is not None else ''

	def _authors(self, epilog:str) -> str:
		return f'.SH "AUTHOR"\n{epilog}\n' if epilog is not None else ''

	def parse_args(self, args:[str]) -> argparse.Namespace:
		parsedArgs:argparse.Namespace = super(TroffArgumentParser, self).parse_args(args)
		parsedArgs = self._validateArgs(parsedArgs)
		return parsedArgs

	def _validateArgs(self, arguments:argparse.Namespace) -> argparse.Namespace:
		if arguments.version:
			print(emperorVersionString)
			sys.exit(0)

		if arguments.output_man:
			print(self.toTroff(), end='')
			sys.exit(0)

		if arguments.files == []:
			arguments.files = [STDIN_FLAG]
		else:
			arguments.files = list(map(_sanitiseFilePath, arguments.files))

		# arguments.files = list(map(sanitiseFilePath, arguments.Files))

		# arguments.files = unique(arguments.files)

		# if arguments.outputFile is None:
		# 	if len(arguments.files) == 1:
		# 		if arguments.files == STDIN_FLAG:
		# 			arguments.outputFile = 'emperorOut.o'
		# 		else:
		# 			arguments.outputFile = os.path.split(arguments.files[0])[0] + '.o'
		# outputParts:str*str = os.path.split(arguments.files[0])
		# outputFile = outputParts[1].split('.')[0] + '.o'
		# arguments.outputFile = outputParts + '/' + outputFile

		return arguments

def parseArguments(args:[str]) -> argparse.Namespace:
	parser: ArgumentParserWithJson = ArgumentParserWithJson(
		licence=[
			'Copyright (c) 2019, Edward Jones\n',
			'\n',
			'%%%%%%LICENSE_START(GPLv2+_DOC_FULL)\n',
			'This is free documentation; you can redistribute it and/or\n',
			'modify it under the terms of the GNU General Public License as\n',
			'published by the Free Software Foundation; either version 2 of\n',
			'the License, or (at your option) any later version.\n',
			'\n',
			'The GNU General Public License\'s references to "object code"\n',
			'and "executables" are to be interpreted as the output of any\n',
			'document formatting or typesetting system, including\n',
			'intermediate and printed output.\n',
			'\n',
			'This manual is distributed in the hope that it will be useful,\n',
			'but WITHOUT ANY WARRANTY; without even the implied warranty of\n',
			'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n',
			'GNU General Public License for more details.\n',
			'\n',
			'You should have received a copy of the GNU General Public\n',
			'License along with this manual; if not, see\n',
			'<http://www.gnu.org/licenses/>.\n',
			'%%%%%%LICENSE_END'
		],
		version = emperorVersion,
		description = f'''Compiler for the Emperor language v{emperorVersion}''',
		seeAlso = ['gcc(1)', 'dux(1)', 'bison(1)', 'flex(1)'],
		bugs = f'''There are no known bugs at this time! :D If you find any, however, please report them at <https://github.com/TheSignPainter98/emperor/issues>''',
		epilog = '''This is maintained by Edward Jones, and source code can be found at <https://github.com/TheSignPainter98/emperor>'''
	)	
	parser.add_argument('-v', '--verbose', action='store_true', help='Output verbosity')
	parser.add_argument('-V', '--version', action='store_true', help='Output version and exit')
	parser.add_argument('-O', '--optimisation', choices=[ 's', '0', '1', '2' ], default=0, dest='optimisation', help='Compiler optimisation level as speficied in \\fBgcc\\fP')
	parser.add_argument('-c', '--to-c', action='store_true', dest='compileCOnly', help='Translate to C (skips compilation step)')
	parser.add_argument('-o', '--output', type=str, dest='outputFile', metavar='OUTPUT_FILE', default=None, action='store', help='Specify output file')
	parser.add_argument('files', metavar='file', type=str, nargs='*', help=f'A file to compile, use {STDIN_FLAG} to read from stdin')
	
	return parser.parse_args(args)

class ArgumentParserWithJson(argparse.ArgumentParser):
	def __init__(self, licence:[str]=[], version:str=None, seeAlso:[str]=[], date:str=date.today().strftime('%d %B %Y'), bugs:str=None, *args, **kwargs):
		super(ArgumentParserWithJson, self).__init__(*args, **kwargs)
		self.licence = licence
		self.version = version
		self.date = date
		self.seeAlso = seeAlso
		self.bugs = bugs
		self.add_argument('-*', '--arg-spec', dest='arg_spec', action='store_true', help='Output a json specification for the arguments')

	def parse_args(self, args:[str]) -> argparse.Namespace:
		nameSpace:argparse.Namespace = super(ArgumentParserWithJson, self).parse_args(args)
		if nameSpace.arg_spec:
			print(self.toJson())
		return nameSpace

	def toJson(self) -> str:
		allowedKeys:[str] = ['licence', 'version', 'date', 'seeAlso', 'bugs', 'prog', 'description', 'epilog']
		keys:[str] = [key for key in self.__dict__ if key in allowedKeys]
		options:dict = {}
		for key in keys:
			options[key] = self.__dict__[key]

		keyOptions:dict = {} 
		actions = self.__dict__['_actions']
		actionList:[] = []
		seenOptionStrings:[str] = []
		for action in actions:
			# actionObj = actions[action]
			optionString:str = action.option_strings[0] if 'option_strings' in action.__dict__ and len(action.option_strings) >= 1 else ''
			if optionString == '' or optionString not in seenOptionStrings:
				actionList.append({k: action.__dict__[k] for k in [ k for k in action.__dict__ if k not in ['container', 'type'] ] })
				# actionList.append(action.__dict__)
			# if optionString not in seenOptionStrings:
			seenOptionStrings.append(optionString)
		options['actions'] = actionList
		return json.dumps(options)
		# return json.dumps(self.__dict__, indent=4, ensure_ascii=False)


def _unique(inputList:[str], key=lambda x : x) -> [str]:
	uniqueList:[str] = []
	uniqueListKeys:[] = []

	for item in inputList:
		itemKey = key(item)
		if itemKey not in uniqueListKeys:
			uniqueListKeys.append(itemKey)
			uniqueList.append(item)

	return uniqueList

def _sanitiseFilePath(filePath:str) -> str:
	if filePath != STDIN_FLAG:
		return os.path.abspath(filePath)
	else:
		return filePath

################################################################################

# cdef extern from "./parser/emperor.tab.h":
# 	int parseStd()
	
def main(args:[str]) -> int:
	arguments:argparse.NameSpace = parseArguments(args)
	return 0

if __name__ == '__main__':
	main(sys.argv[1:])
