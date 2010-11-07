#!/usr/bin/env python
# Setup and utilities for building the project in buildfile
#
# TODO:
#  * Build subsections
#  * Production vs. test environments
#    * profiling and easy tracing
#  * Make sure all haxe and haxelib libraries are up-do-date / ready to go
#  * Directory creation when needed

import os, sys, glob, re
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
setup(dirs=sources, runner='smart_runner')

#------------------------------------------------------

def to_camelcase(s):
    return ''.join([w.title() for w in s.split('_')]);

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

def haxe(name, libs=[], resources=[], assets=[]):
    cmd = ['haxe',
            '-main', name,
            [['-cp',s] for s in sources],
            '-swf9', 'swfs/'+ name.lower() + '.swf',
            '-swf-version', 10,
            '--flash-strict',
            '-debug' if DEBUG else None,
            [['-lib',l] for l in libs],
            [['-swf-lib','.tmp/%s.swf' % a] for a in assets],
            [['-resource',r] for r in resources]]
    run(cmd)

ASSET_MAP = {'bitmap': 'flash.display.Bitmap',
             'clip':   'flash.display.MovieClip',
             'sound':  'flash.display.MovieClip'}

def build_assets(import_name='Assets', file_globs=[]):
    class_dest = 'src/' + '/'.join(import_name.split('.')) + '.hx'
    swfml = ['<?xml version="1.0" encoding="utf-8"?>',
             '<movie frames="1" width="100" height="100" version="10"><frame><library>']
    hx = ['package %s;' % '.'.join(import_name.split('.')[0:-1]), '']

    for g in file_globs:
        for f in glob.glob(g):
            atype = re.findall('('+'|'.join(ASSET_MAP.keys())+')', f)[0]
            name = '.'.join(f.split('/')[-1].split('.')[0:-1])
            swfml.append('<%s id="%s" import="%s"/>' % (atype, name, f))
            hx.append('class %s extends %s{public function new(){super();}}' % \
                    (to_camelcase(name), ASSET_MAP[atype]))

    swfml.append('</library></frame></movie>')

    run('mkdir', '-p', '.tmp')
    swfml_file = '.tmp/asset_lib.swfml'
    file(swfml_file, 'w').write('\n'.join(swfml))
    file(class_dest, 'w').write('\n'.join(hx))
    run('swfmill', 'simple', '.tmp/asset_lib.swfml', '.tmp/%s.swf' % import_name)



#------------------------------------------------------
# Make sure we're in the right working directory to begin with (possibly not
# needed and possibly even interferes w/ fabricate later, we'll see)
os.chdir(ROOT_DIR)


