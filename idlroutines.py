#!/usr/bin/env python

import getopt
import os
import re
import sys


header = '''function %s::init, _extra=e
  compile_opt strictarr

  if (~self->MGutTestCase::init(_extra=e)) then return, 0
'''

footer = '''
  return, 1
end
'''


def print_routines(functions, procedures):
  if procedures:
    print 'procedures:'
    for p in procedures:
      print '  %s' % p
  if functions:
    print 'functions:'
    for f in functions:
      print '  %s' % f

def print_testing_routine(routines, is_function):
  if routines:
    for i, r in enumerate(routines):
      if i == 0:
        prefix = '  self->addTestingRoutine, ['
      else:
        prefix = '                            '
      if i == len(routines) - 1:
        if is_function:
          suffix = '], /is_function'
        else:
          suffix = ']'
      else:
        suffix = ', $'
      print "%s'%s'%s" % (prefix, r, suffix)

def print_testing_routines(functions, procedures, procedure_last, testname):
  functions.reverse()
  procedures.reverse()
  print header % testname
  
  if procedure_last:
    print_testing_routine(procedures, False)
    print_testing_routine(functions, True)
  else:
    print_testing_routine(functions, True)
    print_testing_routine(procedures, False)

  print footer

def determine_testname(filename):
  basename = os.path.basename(filename)
  parts = os.path.splitext(basename)
  base = parts[0]
  if base.endswith('__define'):
    base = base[0:-8]
  base = base + '_ut'
  return base

def find_routines(filename):
  regex = re.compile('(function|pro)')
  functions = []
  procedures = []
  procedure_last = True
  with open(filename, 'r') as f:
    contents = f.readlines()
    for line in contents:
      match = regex.match(line)
      if match:
        args = line.split(',')
        tokens = args[0].split()
        if tokens[0] == 'function':
          functions.append(tokens[1])
          procedure_last = False
        elif tokens[0] == 'pro':
          procedures.append(tokens[1])
          procedure_last = True

  return functions, procedures, procedure_last

def usage():
  print 'usage: routines.py [OPTIONS] [filename]'
  print ''
  print '-h|--help        : print this help message'
  print '-t|--testing     : display mgunit code to add testing routines for this file'
  print '-f |--file <arg> : specify filename to parse'
  sys.exit(0)

def main(argv):
  # defaults
  testing = False
  filename = ''

  try:
    opts, args = getopt.getopt(argv, "hf:t", ['help', 'file=', 'testing'])
  except getopt.GetoptError:
    usage()
    sys.exit(2)

  if len(args) > 0:
    filename = args[0]

  for opt, arg in opts:
    if opt in ('-h', '--help'):
      usage()
    elif opt in ('-t', '--testing'):
      testing = True
    elif opt in ('-f', '--file'):
      filename = arg

  if len(filename) == 0:
    usage()

  filename = os.path.expanduser(filename)
  functions, procedures, procedure_last = find_routines(filename)
  if testing:
    testname = determine_testname(filename)
    print_testing_routines(functions, procedures, procedure_last, testname)
  else:
    print_routines(functions, procedures)


if __name__ == '__main__':
  main(sys.argv[1:])
