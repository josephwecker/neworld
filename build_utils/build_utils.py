#!/usr/bin/env python
# Setup and utilities for building the project in buildfile
#
# TODO:
#  * Build subsections
#  * Production vs. test environments
#    * -debug flag
#    * profiling and easy tracing
#  * Make sure all haxe and haxelib libraries are up-do-date / ready to go
#  * Directory creation when needed

import os, sys
from fabricate import *

# Pop this stuff off of argv
DEBUG = True
for command in sys.argv:
    if command == '-production':
        DEBUG = False
        sys.argv.remove('-production')

def flatten(l, ltypes=(list, tuple)):
    ltype = type(l)
    l = list(l)
    i = 0
    while i < len(l):
        while isinstance(l[i], ltypes):
            if not l[i]:
                l.pop(i)
                i -= 1
                break
            else:
                l[i:i + 1] = l[i]
        i += 1
    return ltype(l)

sources = ['./', 'src', 'lib']
setup(dirs=flatten([sources,'swfs/']), runner='smart_runner',
        hasher=mtime_hasher)

#------------------------------------------------------

def p(*args):
    '''Shorthand for joining path segments together into one path'''
    return os.path.normpath(os.path.join(*args))

BUILD_UTIL_DIR = os.path.abspath(os.path.dirname(__file__) + '/')
ROOT_DIR = p(BUILD_UTIL_DIR, '..')

HAXE_LIBS = []
HAXE_RESOURCES = []

def mkdir(*path):
    '''Shorthand for mkdir -p and then any number of path segments.'''
    run('mkdir', '-p', p(*path))

def rmdir(*path):
    run('rm', '-rf', p(*path))

def haxe(name, libs=[], resources=None):
    cmd = ['haxe',
            '-main', name,
            [['-cp',s] for s in sources],
            '-swf9', 'swfs/'+ name.lower() + '.swf',
            '-swf-version', 10,
            '-debug' if DEBUG else None,
            [['-lib',l] for l in libs]]
    run(cmd)


#------------------------------------------------------
# Make sure we're in the right working directory to begin with (possibly not
# needed and possibly even interferes w/ fabricate later, we'll see)
os.chdir(ROOT_DIR)


