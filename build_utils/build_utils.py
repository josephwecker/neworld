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
DEBUG = False
for command in sys.argv:
    if command == '-debug':
        DEBUG = True
        sys.argv.remove('-debug')

def p(*args):
    '''Shorthand for joining path segments together into one path'''
    return os.path.normpath(os.path.join(*args))

BUILD_UTIL_DIR = os.path.abspath(os.path.dirname(__file__) + '/')
ROOT_DIR = p(BUILD_UTIL_DIR, '..')

def mkdir(*path):
    '''Shorthand for mkdir -p and then any number of path segments.'''
    run('mkdir', '-p', p(*path))

def rmdir(*path):
    run('rm', '-rf', p(*path))

# Make sure we're in the right working directory to begin with (possibly not
# needed and possibly even interferes w/ fabricate later, we'll see)
os.chdir(ROOT_DIR)


