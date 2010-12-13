
INTRODUCTION

	This archive contains some base code for writing 2D games for Flash with
	haXe using Tile Studio and SWFMill, intended to be used during Ludum Dare
	competitions (http://www.ludumdare.com/compo/).

	The code contains a Tile Studio definition file (haxe-swfmill.tsd) for
	exporting tile sets and maps, a haXe library (gamelib2d) with code to use
	this data to display tiles and maps and a little demo (Scroll.hx).


GETTING STARTED

	Below are steps you can use to use this package, assuming you're using a
	PC with Windows and don't have the software installed yet (you may want
	to use other paths):

	* Download haXe: http://haxe.org/download/ (choose Manual Install, the
	zip file is updated more frequently than the Windows installer), extract
	to c:\, rename c:\haxe-2.0-win (or whatever) to c:\haxe and in c:\haxe
	run haxesetup.exe.

	* Download swfmill: http://swfmill.org/pre/swfmill-0.2.12.4-win32.zip,
	extract it to c:\, rename the path c:\swfmill-0.2.12.4-win32 to 
	c:\swfmill. You can also get a newer version of the executable here:
	http://rs109.rapidshare.com/files/131871337/swfmill-0.2.12.6.rar, make
	sure you have c:\swfmill\swfmill.exe.

	* Download Tile Studio: http://tilestudio.sf.net/ts.zip and extract it in
	c:\ts, download the new http://tilestudio.sf.net/ts.exe and replace
	c:\ts\ts.exe with that.

	* Download and install FlashDevelop (just choose the newest version): 
	http://www.flashdevelop.org/community/viewforum.php?f=11 and run the
	installation program.

	* Start FlashDevelop and go to Tools, Program Settings, HaXeContext and
	setup the haXe path (c:\haxe) and a user classpath: c:\haxe\std.

	* Still in FlashDevelop, go to Project, Open Project and open the Scroll
	project (Scroll.hxprj) from this archive, go back to the menu Project
	and then Properties, Build. Check if the path and filename of
	swfmill.exe is correct.

	* Start Tile Studio and open Scroll.tsp (from this archive) in it. 
	Open the Code menu and make sure the option Output to Project Directory
	is checked. Press Shift+F10 and select haXe-swfmill at the top if it
	isn't already selected, then close that window. Finally, press F10 to
	generate all code and wait until it is finished.

	* Switch back to FlashDevelop and press F5 to compile and run the demo.
	In Project, Properties, Output, Test Movie, you can set up how to run
	the swf files.
	
	Once the Scroll demo works, you have successfully set up everything.
	

FLASH DOCUMENTATION

	http://livedocs.adobe.com/flex/201/langref/index.html


LICENSE

	(C) Copyright 2008-2010, Mike Wiering, Wiering Software

	This software is provided 'as-is', without any express or implied
	warranty. In no event will the authors be held liable for any damages
	arising from the use of this software.

	Permission is granted to anyone to use this software for any purpose,
	including commercial applications, and to alter it and redistribute it
	freely, subject to the following restrictions:

		1. The origin of this software must not be misrepresented; you must
		not claim that you wrote the original software. If you use this
		software in a product, an acknowledgment in the product documentation
		would be appreciated but is not required.

		2. Altered source versions must be plainly marked as such, and must
		not be misrepresented as being the original software.

		3. This notice may not be removed or altered from any source
		distribution.
