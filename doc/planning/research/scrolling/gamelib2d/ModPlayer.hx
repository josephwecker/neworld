package gamelib2d;

/*
 * Flash Module Player 
 * Copyright (C) 2008-2009 Kostas Michalopoulos
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Kostas Michalopoulos <badsector@runtimeterror.com>
 */
 
/*! @module "Flash MOD Player"
 * Flash MOD Player is a pair of classes to extend the sound capabilities of
 * Flash Player 9 by adding dynamic sound and MOD file support.
 */
 
import flash.events.Event;
import flash.events.IOErrorEvent;
import flash.events.SampleDataEvent;
import flash.media.Sound;
import flash.net.URLRequest;
import flash.net.URLLoader;
import flash.utils.ByteArray;
import flash.utils.Timer;

private class Sample
{
    public var name:String;
    public var wave:Array<Int>;
    public var length:Int;
    public var fine:Int;
    public var volume:Int;
    public var loopstart:Int;
    public var looplen:Int;
	
	public var flags:Int;  //*
	public var pan:Int;
	public var relative:Int;
	public var sixteen:Int;
    
    public function new(){}
}


//*
private class Instrument
{
	public var samplecount:Int;
	public var sample:Array<Sample>;
	public var samplepernote:Array<Int>;
	public var volumeenvelope:Array<Int>;
	public var panningenvelope:Array<Int>;
	public var volumesustainpoint:Int;
	public var volumeloopstartpoint:Int;
	public var volumeloopendpoint:Int;
	public var panningsustainpoint:Int;
	public var panningloopstartpoint:Int;
	public var panningloopendpoint:Int;
	public var volumetype:Int;
	public var pannnigtype:Int;
	public var vibratotype:Int;
	public var vibratosweep:Int;
	public var vibratodepth:Int;
	public var vibratorate:Int;
	public var volumefadeout:Int;
	
	public function new(){}
}


private class Note
{
    //public var sample:Sample;
	public var instrument:Instrument; //*
	
    public var period:Int;
    public var peridx:Int;
    public var command:Int;
    public var cmdarg:Int;
    
    public function new(){}
}

private  class Channel
{
    public var note:Array<Note>;
    
    public function new(){}
}

private  class Pattern
{
    public var channel:Array<Channel>;
    
    public function new(){}
}

private  class ChanState
{
    public var csmp:Sample;
    public var cslength:UInt;
    public var cslooplen:UInt;
    public var cperiod:Int;
    public var lastnoteperiod:Int;
    public var csp:UInt;
    public var cspinc:Int;
    public var cvolume:Int;
    public var rvolume:Int;
    public var arpeggio:Bool;
    public var arpeggionote:Int;
    public var arpeggiosemi1:Int;
    public var arpeggiosemi2:Int;
    public var arpeggiotick:Int;
    public var cutsample:Bool;
    public var cutsampleticks:Int;
    public var delaynote:Bool;
    public var delaynoteticks:Int;
    public var retriggersample:Bool;
    public var retriggersampleticks:Int;
    public var retriggersamplectr:Int;
    public var slidevolume:Bool;
    public var slidevolumeval:Int;
    public var slideperiod:Bool;
    public var slideperiodval:Int;
    public var slidetonote:Bool; // uses slideperiodval too
    public var slidetonotetarget:Int;
    public var vibrato:Bool;
    public var vibratowave:Int;
    public var vibratopos:Int;
    public var vibratospeed:Int;
    public var vibratodepth:Int;
    public var tremolo:Bool;
    public var tremolowave:Int;
    public var tremolopos:Int;
    public var tremolospeed:Int;
    public var tremolodepth:Int;
	public var pan:Int;  // 0: left - 0xFF: right
	public var channelpan:Int;
    
    public function new(){}
}

/**
 * This class provides a full MOD player. It synthesizes a full waveform from a
 * MOD file and uses the {@link DynSound} class to play the synthesized
 * waveform.
 */
class ModPlayer
{
    //NOTE: the fields below should be considered as "private"!! See the end of
    //      the class for the public API.
	
	public var dyn: Bool;
	private var sde: SampleDataEvent;
	public var ready: Bool;
	public var samplespercallback: Int;
	
    /**** Module data ****/
    public var pat:Array<Pattern>;
    public var smp:Array<Sample>;
	
	public var instr:Array<Instrument>;  //*
	
    public var order:Array<Int>;
    public var chancount:Int;
    public var songlength:Int;
    /**** Player data ****/
    public var wave:ByteArray; // generated waveform for the module
    public var wavelen:Int;
    public var periods:Array<Int>;
	public var periodsbase:Int;
    public var sinewave:Array<Int>;
    public var stopnow:Bool;
    public var repeating:Bool;
	public var stereo:Bool;
	
	public var xm: Bool;  //*
	public var xmflags:Int;
	public var uselinearfreq:Bool;
    public var linearperiods:Array<Int>;
	public var defaulttempo:Int;
	public var defaultbeatspermin:Int;
	public var globalvolume:Int; 
	public var restart:Int;
	public var userorder:Bool;
	
	public var fadevolume:Int;
	public var fadespeed:Int;
	public var activechannels:Array<Int>;
	public var transpose:Int;
	

    /**** Public functions ****/
    public function xtrace(msg:String)
    {
        if (showTraces) trace(msg);
    }
    
	public function clear ()
	{
		stopnow = true;
		//ready = false;
		//wave = null;
		//pat = null;
		//smp = null;
		//order = null;
		//periods = null;
		//sinewave = null;
		
		cnt = 0;
		rowtick = 0;
	}
	
    public function setRepeating(value:Bool)
    {
        repeating = value;
    }
    
    public function setStereo(value:Bool)
    {
        stereo = value;
    }
    
    public function setDynamic(value:Bool)
    {
        dyn = value;
    }
	
	public function setTranspose(value:Int)
	{
		transpose = value;
	}
    
	public function setPatternOrder(o:String, ?r:Int=0)
	{
		order = [];
		activechannels = [];
		var pos:Int = 0;
		var active:Int = 0xFFFFFFFF;
		for (s in o.split(",")) {
			// trim
			while (s != "" && s.charAt(0) == " ")
				s = s.substr(1, s.length - 1);
			var a:Array<String> = s.split(" ");
			order[pos++] = Std.parseInt(a[a.length - 1]);
			if (a.length > 1)
				for (s in a[0].split("|")) {
					if (s.length >= 2) {
						var ch = Std.parseInt(s.substr(1, s.length - 1)) - 1;
						if (s.charAt(0) == "+") active |= (1 << ch);
						if (s.charAt(0) == "-") active &= ~(1 << ch);
					}
				}
			activechannels.push(active);
		}
		
		if (ready)
		{
			corder = 0;
			crow = 0;
			//corder = order.length - 1;
			//crow = 63;
			cpat = order[corder];
			cpattern = pat[cpat];
		}
		
		repeating = (r >= 0);
		if (repeating) restart = r;
		
		userorder = !(order == []);
	}
	
    public function parseData(data:ByteArray)
    {
        /* MOD info */
		
        var samplecount:Int = 0;
        var patcount:Int;
		
		var instrcount:Int = 0;  //*
		var nextpos:Int = 0;
		
        var rowcount:Int = 64;
        var periods:Array<Int> = [3628,3424,3232,3048,2880,2712,2560,2416,2280,2152,2032,1920,
                                  1814,1712,1616,1524,1440,1356,1280,1208,1140,1076,1016,960,
                                  907,856,808,762,720,678,640,604,570,538,508,480,
                                  453,428,404,381,360,339,320,302,285,269,254,240,
                                  226,214,202,190,180,170,160,151,143,135,127,120,
                                  113,107,101,95,90,85,80,75,71,67,63,60,
                                  56,53,50,47,45,42,40,37,35,33,31,30,
                                  3600,3400,3208,3028,2860,2700,2544,2404,2268,2140,2020,1908,
                                  1800,1700,1604,1514,1430,1350,1272,1202,1134,1070,1010,954,
                                  900,850,802,757,715,675,636,601,567,535,505,477,
                                  450,425,401,379,357,337,318,300,284,268,253,238,
                                  225,212,200,189,179,169,159,150,142,134,126,119,
                                  112,106,100,94,89,84,79,75,71,67,63,59,
                                  56,53,50,47,44,42,39,37,35,33,31,29,
                                  3576,3376,3184,3008,2836,2680,2528,2388,2252,2128,2008,1896,
                                  1788,1688,1592,1504,1418,1340,1264,1194,1126,1064,1004,948,
                                  894,844,796,752,709,670,632,597,563,532,502,474,
                                  447,422,398,376,355,335,316,298,282,266,251,237,
                                  223,211,199,188,177,167,158,149,141,133,125,118,
                                  111,105,99,94,88,83,79,74,70,66,62,59,
                                  55,52,49,47,44,41,39,37,35,33,31,29,
                                  3548,3352,3164,2984,2816,2660,2512,2368,2236,2112,1992,1880,
                                  1774,1676,1582,1492,1408,1330,1256,1184,1118,1056,996,940,
                                  887,838,791,746,704,665,628,592,559,528,498,470,
                                  444,419,395,373,352,332,314,296,280,264,249,235,
                                  222,209,198,187,176,166,157,148,140,132,125,118,
                                  111,104,99,93,88,83,78,74,70,66,62,59,
                                  55,52,49,46,44,41,39,37,35,33,31,29,
                                  3524,3328,3140,2964,2796,2640,2492,2352,2220,2096,1976,1868,
                                  1762,1664,1570,1482,1398,1320,1246,1176,1110,1048,988,934,
                                  881,832,785,741,699,660,623,588,555,524,494,467,
                                  441,416,392,370,350,330,312,294,278,262,247,233,
                                  220,208,196,185,175,165,156,147,139,131,123,117,
                                  110,104,98,92,87,82,78,73,69,65,61,58,
                                  55,52,49,46,43,41,39,36,34,32,30,29,
                                  3500,3304,3116,2944,2776,2620,2476,2336,2204,2080,1964,1852,
                                  1750,1652,1558,1472,1388,1310,1238,1168,1102,1040,982,926,
                                  875,826,779,736,694,655,619,584,551,520,491,463,
                                  437,413,390,368,347,328,309,292,276,260,245,232,
                                  219,206,195,184,174,164,155,146,138,130,123,116,
                                  109,103,97,92,87,82,77,73,69,65,61,58,
                                  54,51,48,46,43,41,38,36,34,32,30,29,
                                  3472,3280,3096,2920,2756,2604,2456,2320,2188,2064,1948,1840,
                                  1736,1640,1548,1460,1378,1302,1228,1160,1094,1032,974,920,
                                  868,820,774,730,689,651,614,580,547,516,487,460,
                                  434,410,387,365,345,325,307,290,274,258,244,230,
                                  217,205,193,183,172,163,154,145,137,129,122,115,
                                  108,102,96,91,86,81,77,72,68,64,61,57,
                                  54,51,48,45,43,40,38,36,34,32,30,28,
                                  3448,3256,3072,2900,2736,2584,2440,2300,2172,2052,1936,1828,
                                  1724,1628,1536,1450,1368,1292,1220,1150,1086,1026,968,914,
                                  862,814,768,725,684,646,610,575,543,513,484,457,
                                  431,407,384,363,342,323,305,288,272,256,242,228,
                                  216,203,192,181,171,161,152,144,136,128,121,114,
                                  108,101,96,90,85,80,76,72,68,64,60,57,
                                  54,50,48,45,42,40,38,36,34,32,30,28,
                                  3424,3232,3048,2880,2712,2560,2416,2280,2152,2032,1920,1812,
                                  1712,1616,1524,1440,1356,1280,1208,1140,1076,1016,960,906,
                                  856,808,762,720,678,640,604,570,538,508,480,453,
                                  428,404,381,360,339,320,302,285,269,254,240,226,
                                  214,202,190,180,170,160,151,143,135,127,120,113,
                                  107,101,95,90,85,80,75,71,67,63,60,56,
                                  53,50,47,45,42,40,37,35,33,31,30,28,
                                  3400,3208,3028,2860,2696,2548,2404,2268,2140,2020,1908,1800,
                                  1700,1604,1514,1430,1348,1274,1202,1134,1070,1010,954,900,
                                  850,802,757,715,674,637,601,567,535,505,477,450,
                                  425,401,379,357,337,318,300,284,268,253,239,225,
                                  213,201,189,179,169,159,150,142,134,126,119,113,
                                  106,100,94,89,84,79,75,71,67,63,59,56,
                                  53,50,47,44,42,39,37,35,33,31,29,28,
                                  3376,3184,3008,2836,2680,2528,2388,2252,2128,2008,1896,1788,
                                  1688,1592,1504,1418,1340,1264,1194,1126,1064,1004,948,894,
                                  844,796,752,709,670,632,597,563,532,502,474,447,
                                  422,398,376,355,335,316,298,282,266,251,237,224,
                                  211,199,188,177,167,158,149,141,133,125,118,112,
                                  105,99,94,88,83,79,74,70,66,62,59,56,
                                  52,49,47,44,41,39,37,35,33,31,29,28,
                                  3352,3164,2984,2816,2660,2512,2368,2236,2112,1992,1880,1776,
                                  1676,1582,1492,1408,1330,1256,1184,1118,1056,996,940,888,
                                  838,791,746,704,665,628,592,559,528,498,470,444,
                                  419,395,373,352,332,314,296,280,264,249,235,222,
                                  209,198,187,176,166,157,148,140,132,125,118,111,
                                  104,99,93,88,83,78,74,70,66,62,59,55,
                                  52,49,46,44,41,39,37,35,33,31,29,27,
                                  3328,3140,2964,2796,2640,2492,2352,2220,2096,1980,1868,1764,
                                  1664,1570,1482,1398,1320,1246,1176,1110,1048,990,934,882,
                                  832,785,741,699,660,623,588,555,524,495,467,441,
                                  416,392,370,350,330,312,294,278,262,247,233,220,
                                  208,196,185,175,165,156,147,139,131,124,117,110,
                                  104,98,92,87,82,78,73,69,65,62,58,55,
                                  52,49,46,43,41,39,36,34,32,31,29,27,
                                  3304,3116,2944,2776,2620,2476,2336,2204,2080,1964,1852,1748,
                                  1652,1558,1472,1388,1310,1238,1168,1102,1040,982,926,874,
                                  826,779,736,694,655,619,584,551,520,491,463,437,
                                  413,390,368,347,328,309,292,276,260,245,232,219,
                                  206,195,184,174,164,155,146,138,130,123,116,109,
                                  103,97,92,87,82,77,73,69,65,61,58,54,
                                  51,48,46,43,41,38,36,34,32,30,29,27,
                                  3280,3096,2920,2756,2604,2456,2320,2188,2064,1948,1840,1736,
                                  1640,1548,1460,1378,1302,1228,1160,1094,1032,974,920,868,
                                  820,774,730,689,651,614,580,547,516,487,460,434,
                                  410,387,365,345,325,307,290,274,258,244,230,217,
                                  205,193,183,172,163,154,145,137,129,122,115,109,
                                  102,96,91,86,81,77,72,68,64,61,57,54,
                                  51,48,45,43,40,38,36,34,32,30,28,27,
                                  3256,3072,2900,2736,2584,2440,2300,2172,2052,1936,1828,1724,
                                  1628,1536,1450,1368,1292,1220,1150,1086,1026,968,914,862,
                                  814,768,725,684,646,610,575,543,513,484,457,431,
                                  407,384,363,342,323,305,288,272,256,242,228,216,
                                  204,192,181,171,161,152,144,136,128,121,114,108,
                                  102,96,90,85,80,76,72,68,64,60,57,54,
                                  51, 48, 45, 42, 40, 38, 36, 34, 32, 30, 28, 27];
        this.periods = periods;
                                  
        xtrace("Parsing mod data...");
        
		//*
		globalvolume = 0xFF;
		defaulttempo = 6;
		defaultbeatspermin = 125;
		
		uselinearfreq = false;
        periodsbase = 8 * 84;
		
		var s: String = "";
		for (i in 0...17)
			s += String.fromCharCode(data[i]);
		xm = (s == "Extended Module: ");
		
		if (xm) {
			
			// http://www.fileformat.info/format/xm/corion.htm
			
			data.endian = flash.utils.Endian.LITTLE_ENDIAN;
			data.position = 0x11;
			var name:String = "";
			for (j in 0...20) {
				var c:Int = data.readUnsignedByte();
				if (c > 31 && c < 127) name += String.fromCharCode(c); else break;
			}
			xtrace("Extended Module title: " + name);
			data.position = 0x3C;
			
			nextpos = data.position + data.readUnsignedInt();  // length of pattern block

			songlength = data.readUnsignedShort();
			xtrace("songlength: " + songlength);
			var i = data.readUnsignedShort();
			if (!userorder)
				restart = i;
			chancount = data.readUnsignedShort();
			xtrace("channels: " + chancount);
			patcount = data.readUnsignedShort();
			xtrace("patterns: " + patcount);
			instrcount = data.readUnsignedShort();
			xtrace("instruments: " + instrcount);
			instr = new Array<Instrument>();
			for (i in 0...instrcount) {
				instr[i] = new Instrument();
			}

			xmflags = data.readUnsignedShort();  // bit 0 - Linear frequency table / Amiga freq. table
			uselinearfreq = (xmflags & 1 != 0);
			//uselinearfreq = false;
			if (uselinearfreq)
			{
				setupLinearPeriods ();
				periodsbase = 128 * 84;
			}
			defaulttempo = data.readUnsignedShort();
			xtrace("default tempo: " + defaulttempo);
			defaultbeatspermin = data.readUnsignedShort();
			xtrace("default BPM: " + defaultbeatspermin);
			for (i in 0...256)
			{
				var j = data.readUnsignedByte();
				if (!userorder && i < songlength)
					order[i] = j;
			}
				
			data.position = nextpos;
			
			
		} else {
			
			chancount = 4;
			
			// load sample info
			smp = new Array<Sample>();
			if ((data[1080] == 77 && data[1081] == 46 && data[1082] == 75 && data[1083] == 46) || // M.K. signature
				(data[1080] == 70 && data[1081] == 76 && data[1082] == 84 && data[1083] == 52) || // FLT4 signature
				(data[1080] == 52 && data[1081] == 67 && data[1082] == 72 && data[1083] == 78)) { // 4CHN signature
				samplecount = 31;
			} else if (data[1080] == 52 && data[1081] == 67 && data[1082] == 72 && data[1083] == 78) {   // 2CHN signature
				samplecount = 31;
				chancount = 2;
			} else if (data[1080] == 54 && data[1081] == 67 && data[1082] == 72 && data[1083] == 78) {   // 6CHN signature
				samplecount = 31;
				chancount = 6;
			} else if ((data[1080] == 67 && data[1081] == 68 && data[1082] == 56 && data[1083] == 49) || // CD81 signature
					   (data[1080] == 79 && data[1081] == 67 && data[1082] == 84 && data[1083] == 65) || // OCTA signature
					   (data[1080] == 56 && data[1081] == 67 && data[1082] == 72 && data[1083] == 78)) { // 8CHN signature
				samplecount = 31;
				chancount = 8;
			} else if (data[1082] == 67 && data[1083] == 72) {   // xxCH signature
				samplecount = 31;
				chancount = (data[1080]-'0'.charCodeAt(0))*10 + (data[1081]-'0'.charCodeAt(0));
			} else {
				samplecount = 15;
			}
			xtrace("Module has " + samplecount + " samples, " + chancount + " channels");
			var name:String = "";
			for (j in 0...20) {
				var c:Int = data.readUnsignedByte();
				if (c > 31 && c < 127) name += String.fromCharCode(c); else break;
			}
			xtrace("Module title: " + name);
			
			data.position = 20;
			for (i in 0...samplecount) {
				name = "";
				for (j in 0...22) {
					var c:Int = data.readUnsignedByte();
					if (c > 31 && c < 127)
						name += String.fromCharCode(c);
				}
				if (name != "") xtrace("sample " + i + ": " + name);
				var len:Int = data.readUnsignedShort()*2;
				var fine:Int = data.readUnsignedByte()&0x0F;
				var vol:Int = data.readUnsignedByte();
				var loopstart:Int = data.readUnsignedShort()*2;
				var looplen:Int = data.readUnsignedShort()*2;
				
				if (len < 4) len = 0; // MilkyTracker bug?
				if (fine > 7) fine = fine - 16;
				
				smp[i] = new Sample();
				smp[i].name = name;
				smp[i].length = len;
				smp[i].fine = fine;
				smp[i].volume = vol;
				smp[i].loopstart = loopstart;
				smp[i].looplen = looplen;
				
				smp[i].relative = 0;
			}
			
			//*
			instr = new Array<Instrument>();
			for (i in 0...samplecount) {
				instr[i] = new Instrument();
				instr[i].samplecount = 1;
				instr[i].sample = new Array<Sample>();
				instr[i].sample[0] = smp[i];
			}
			
			// load order
			songlength = data.readUnsignedByte();
			if (songlength < 1) songlength = 1;
			data.readUnsignedByte(); // skip obsolete byte
			for (i in 0...128)
			{
				var j = data.readUnsignedByte();
				if (!userorder && i < songlength)
					order[i] = j;
			}
			if (samplecount != 15) data.readInt();
			
			xtrace("Song length is " + songlength + " patterns.");
			
			// load patterns
			patcount = 0;
			for (i in 0...songlength)
				if (patcount < order[i]) patcount = order[i];
			patcount += 1;
			xtrace("Pattern count: " + patcount);
		}
		
		
        pat = new Array<Pattern>();
        for (i in 0...patcount) {
			
			if (xm) {
				// load patterns
				var blocklen = data.readUnsignedInt();  // length of pattern header block
				data.readUnsignedByte();  // pack type
				rowcount = data.readUnsignedShort();
				xtrace("rowcount: " + rowcount);
				nextpos = data.readUnsignedShort() + data.position;  // size of pattern data
			}
			
			
            pat[i] = new Pattern();
            pat[i].channel = new Array<Channel>();
            for (c in 0...chancount) {
                pat[i].channel[c] = new Channel();
                pat[i].channel[c].note = new Array<Note>();
                for (r in 0...rowcount)
                    pat[i].channel[c].note[r] = new Note();
            }
            
			//var ss:String = "";
			
            for (r in 0...rowcount) {
                for (ch in 0...chancount) {
					
					if (xm) {  //*
						
						var note:Int = data.readUnsignedByte();
						var ins:Int = 0;
						var vol:Int = 0;
						var effect:Int = 0;
						var effectarg:Int = 0;
						var period:Int = 0;
						var peridx:Int = -1;
						
						var bits:Int = 0x1F;
						if (note >= 0x80) {
							bits &= note;
							if (bits & 1 != 0) note = data.readUnsignedByte();
						}
						if (bits & 2 != 0) ins = data.readUnsignedByte();
						if (bits & 4 != 0) vol = data.readUnsignedByte();
						if (bits & 8 != 0) effect = data.readUnsignedByte();
						if (bits & 0x10 != 0) effectarg = data.readUnsignedByte();
						
						//if (ss.length < 100)
						//	ss += note + "|" + ins + "|" + vol + "|" + effect + "   ";
						
						if (note == 97)
							peridx = note;
						if (note-12 > 0 && note-12 <= 84) {
							peridx = note-12 -1;
							if (uselinearfreq)
								period = linearperiods[peridx + periodsbase];
							else
								period = periods[peridx + periodsbase];
						}
						pat[i].channel[ch].note[r].instrument = ins == 0? null : instr[ins - 1];  //*
						pat[i].channel[ch].note[r].period = period;
						pat[i].channel[ch].note[r].peridx = peridx;
						pat[i].channel[ch].note[r].command = effect;
						pat[i].channel[ch].note[r].cmdarg = effectarg;
						
					} else {
						
						var a:Int = data.readUnsignedByte();
						var b:Int = data.readUnsignedByte();
						var c:Int = data.readUnsignedByte();
						var d:Int = data.readUnsignedByte();
						
						var sampidx:Int = (((a>>4)&0x0F)<<4)|(((c>>4)&0x0F));
						var period:Int = b|((a&0x0F)<<8);
						var effect:Int = d|((c&0x0F)<<8);
						var peridx:Int = -1;
						
						if (period > 0) {
							var diff = 50;
							
							for (p in 672...756) {
								if (Math.abs(period-periods[p]) < diff) {
									peridx = p;
									diff = Std.int(Math.abs(period - periods[p]));
								}
							}
						}
						
						//pat[i].channel[ch].note[r].sample = sampidx==0?null:smp[sampidx - 1];
						pat[i].channel[ch].note[r].instrument = sampidx==0?null:instr[sampidx - 1];  //*
						
						pat[i].channel[ch].note[r].period = period;
						pat[i].channel[ch].note[r].peridx = peridx;
						pat[i].channel[ch].note[r].command = (effect>>8)&0x0F;
						pat[i].channel[ch].note[r].cmdarg = effect & 0xFF;
					}
                }
            }
			//trace(ss);
			
			if (xm)   //*
				data.position = nextpos;
        }
        
		if (xm) {   //*
			
			// load instruments + samples
			for (i in 0...instrcount)
			{
				var blocksize:Int = data.readUnsignedInt();
				//xtrace("block size: " + blocksize);
				var nextpos:Int = data.position - 4 + blocksize;
				var name:String = "";
				for (j in 0...22) {
					var c:Int = data.readUnsignedByte();
					if (c > 0) name += String.fromCharCode(c);
				}
				xtrace("instrument name: " + name);
				data.readUnsignedByte();  // instrument type always 0
				instr[i].samplecount = data.readUnsignedShort();
				//xtrace("samples: " + instr[i].samplecount);
				var sampleheadersize:Int = 0;
				instr[i].samplepernote = new Array<Int>();
				if (instr[i].samplecount > 0) {
					sampleheadersize = data.readUnsignedInt();  // sample header size
					//xtrace("sample header size: " + sampleheadersize);
					for (j in 0...96)
						instr[i].samplepernote.push(data.readUnsignedByte());
					instr[i].volumeenvelope = new Array<Int>();
					for (j in 0...48)
						instr[i].volumeenvelope.push(data.readUnsignedByte());
					instr[i].panningenvelope = new Array<Int>();
					for (j in 0...48)
						instr[i].panningenvelope.push(data.readUnsignedByte());
					instr[i].volumeenvelope = instr[i].volumeenvelope.slice(0, data.readUnsignedByte());
					instr[i].panningenvelope = instr[i].panningenvelope.slice(0, data.readUnsignedByte());
						
					instr[i].volumesustainpoint = data.readUnsignedByte();
					instr[i].volumeloopstartpoint = data.readUnsignedByte();
					instr[i].volumeloopendpoint = data.readUnsignedByte();
					instr[i].panningsustainpoint = data.readUnsignedByte();
					instr[i].panningloopstartpoint = data.readUnsignedByte();
					instr[i].panningloopendpoint = data.readUnsignedByte();
					instr[i].volumetype = data.readUnsignedByte();
					instr[i].pannnigtype = data.readUnsignedByte();
					instr[i].vibratotype = data.readUnsignedByte();
					instr[i].vibratosweep = data.readUnsignedByte();
					instr[i].vibratodepth = data.readUnsignedByte();
					instr[i].vibratorate = data.readUnsignedByte();
					instr[i].volumefadeout = data.readUnsignedShort();
					data.readUnsignedShort();  // reserved
				}
				
				data.position = nextpos;
				instr[i].sample = new Array<Sample>();
				for (j in 0...instr[i].samplecount) {
					var nextpos:Int = data.position + sampleheadersize;
					instr[i].sample[j] = new Sample();
					instr[i].sample[j].length = data.readUnsignedInt();
					instr[i].sample[j].loopstart = data.readUnsignedInt();
					instr[i].sample[j].looplen = data.readUnsignedInt();
					instr[i].sample[j].volume = data.readUnsignedByte();
					
					if (uselinearfreq)
					{
						instr[i].sample[j].fine = signedByte(data.readUnsignedByte());
						if (instr[i].sample[j].fine > 127)
							instr[i].sample[j].fine -= 256;
					}
					else
					{
						instr[i].sample[j].fine = signedByte(data.readUnsignedByte()) >> 4;
						if (instr[i].sample[j].fine > 7)
							instr[i].sample[j].fine -= 16;
					}
					
					instr[i].sample[j].flags = data.readUnsignedByte();
					instr[i].sample[j].pan = signedByte(data.readUnsignedByte());
					instr[i].sample[j].relative = signedByte(data.readUnsignedByte());
					data.readUnsignedByte();
					var name:String = "";
					for (k in 0...22) {
						var c:Int = data.readUnsignedByte();
						if (c > 0) name += String.fromCharCode(c);
					}
					xtrace("sample: " + name);
					instr[i].sample[j].name = name;

					//xtrace ("flags: " + instr[i].sample[j].flags);
					
					data.position = nextpos;
				}
				for (j in 0...instr[i].samplecount) {
					instr[i].sample[j].wave = new Array<Int>();
					instr[i].sample[j].sixteen = (instr[i].sample[j].flags & 0x10 != 0)? 1 : 0;
					if (instr[i].sample[j].sixteen != 0) {
						instr[i].sample[j].length >>= 1;
						instr[i].sample[j].loopstart >>= 1;
						instr[i].sample[j].looplen >>= 1;
					}
					var delta:Int = 0;
					for (k in 0...instr[i].sample[j].length) 
						if (instr[i].sample[j].sixteen != 0) {
							delta = (delta + data.readUnsignedShort()) & 0xFFFF;
							if (delta > 32768) delta -= 65536;
							instr[i].sample[j].wave[k] = delta >> 9; // Std.int(64.0 * delta / 100);
						} else {
							delta += data.readByte();
							instr[i].sample[j].wave[k] = Std.int(64.0 * signedByte(((delta << 24) >> 24) & 0xFF) / 100);
							//instr[i].sample[j].wave[k] = delta;
						}
				}
			}

			/*
			// adjust finetuning after reading instruments
			for (i in 0...patcount) {
				for (r in 0...rowcount) {
					for (ch in 0...chancount) {
						var peridx: Int = pat[i].channel[ch].note[r].peridx;
						var period: Int;
						if (peridx != 97) {
							var ins: Instrument = pat[i].channel[ch].note[r].instrument;
							if (ins != null) {
								var n: Int = peridx + transpose;
								var s: Int = ins.samplepernote[n % 84];
								if (s < ins.samplecount) {
									var sample: Sample = ins.sample[s];
									n += sample.relative + sample.fine * 84 + periodsbase;
									if (uselinearfreq)
										period = linearperiods[n];
									else
										period = periods[n];
									//trace (period);
									pat[i].channel[ch].note[r].period = period;
								}
							}
						}
					}
				}
			}
			*/
			
		} else {   // not xm
		
			// load sample data
			for (i in 0...samplecount) {
				var b:Int = 0;
				smp[i].wave = new Array<Int>();
				for (j in 0...smp[i].length) {
					b = data.readByte();
					smp[i].wave[j] = Std.int(b*64.0/100);
				}
			}
			
		}
		
		if (!userorder)
		{
			activechannels = new Array<Int>();
			for (o in order)
				activechannels.push(0xFFFFFFFF);
		}
		
		if (uselinearfreq)
			this.periods = linearperiods;

        xtrace("Done parsing module data");
    }
	
	
	
	public function setupLinearPeriods()
	{
		linearperiods = new Array<Int>();
		
		var half: Float = 1.059463094;  // half ^ 12 = 2
		
		for (fine in -128...128)
		{
			var f: Float = (440 * 2 * 2 * 2);
			
			//f *= Math.sqrt(half);
			//f *= Math.sqrt(half);
			f *= Math.sqrt(half);  // tune with MilkyTracker
			
			// 127: half tone up: f * half
			//  64:  f * sqrt(half)
			//  32:  f * sqrt(sqrt(half))
			//  16:  f * sqrt(sqrt(sqrt(half)))
			//   8:  f * sqrt(sqrt(sqrt(sqrt(half))))
			//   4:  f * sqrt(sqrt(sqrt(sqrt(sqrt(half)))))
			//   2:  f * sqrt(sqrt(sqrt(sqrt(sqrt(sqrt(half))))))
			//   1:  f * sqrt(sqrt(sqrt(sqrt(sqrt(sqrt(sqrt(half)))))))
			
			if (fine < 0)
				for (i in 0...-fine)
					f *= Math.sqrt(Math.sqrt(Math.sqrt(Math.sqrt(Math.sqrt(Math.sqrt(Math.sqrt(half)))))));
			if (fine > 0)
				for (i in 0...fine)
					f /= Math.sqrt(Math.sqrt(Math.sqrt(Math.sqrt(Math.sqrt(Math.sqrt(Math.sqrt(half)))))));
			
			for (n in 0...84)
			{
				f /= half;
				linearperiods.push (Math.round(f));
			}
		}
	}
	

    public function signedByte(x:UInt):Int
	{
		if (x > 127) x -= 0x100;
		return x;
	}
	
    public var notsupflag:Array<Bool>;
    public function notsupport(code:Int,pat:Int,row:Int)
    {
        if (!notsupflag[code]) {
            notsupflag[code] = true;
            if (code > 0x0F)
                xtrace("Command not supported at pat " + pat + " row " + row + ": Extended " + (code>>4));
            else
                xtrace("Command not supported at pat " + pat + " row " + row + ": " + code);
        }
    }
    
    public static function getCurrentSWFUrl():Dynamic
    {
        return flash.Lib.current.stage.loaderInfo.url;
    }
    
    // state variables for genSegment
    var corder:Int;
    var cnt:Int;
    var cpat:Int;
    var crow:Int;
    var cpattern:Pattern;
    var states:Array<ChanState>;
    var rowspermin:Int;
    var ticksperrow:Int;
    var tickspermin:Int;
    var samplespertick:Int;
    var rowtick: Int;
    var ticksmpctr:Int;
    var checkrow:Bool;
    var checktick:Bool;
    var breakpatonrow:Bool;
    var breakpatnextrow:Int;
    var delaypattern:Int;
    
    public function calcSegment():Bool
    {
        var stamp:Float = haxe.Timer.stamp();
        var segsamples:Int = 0;
        var cs:ChanState;
        
        while (true) {
            var nextcheckrow:Bool = false;
            
            if (++ticksmpctr >= samplespertick) {
                ticksmpctr = 0;
                if (++rowtick >= ticksperrow) {
                    rowtick = 0;
                    if (delaypattern > 0) {
                        delaypattern--;
                    } else {
                        if (breakpatonrow || ++crow == 64) {
                            crow = breakpatonrow?breakpatnextrow:0;
                            if (crow >= 0x40) crow = 0x3F;
                            if (++corder >= order.length) 
								if (!dyn)
									return false; // end-of-song
								else
									if (repeating)
										corder = restart;
									else
									{
										ready = false;
										return false;
									}
                            cpat = order[corder];
                            cpattern = pat[cpat];
                            breakpatonrow = false;
                            delaypattern = 0;
                        }
                        checkrow = true;
                    }
                }
                checktick = true;
            }
            
            var mixed:Int = 0;
			var mixedLeft:Int = 0;
			var mixedRight:Int = 0;
            
            for (c in 0...chancount)
				if (activechannels[corder] & (1 << c) != 0)
			{
                cs = states[c];
                
                if (checkrow) {
                    var note:Note = cpattern.channel[c].note[crow];
					if (note != null) {
						cs.arpeggio = false;
						cs.delaynote = false;
						cs.retriggersample = false;
						cs.slidevolume = false;
						cs.slideperiod = false;
						cs.slidetonote = (note.command == 0x03 || note.command == 0x05);
						cs.vibrato = (note.command == 0x04 || note.command == 0x06);
						cs.tremolo = false;
						cs.pan = cs.channelpan;
						
						if (note.peridx == 97) {
							cs.csmp = null;
							note.instrument = null;
						} else {
							if (note.instrument != null) { 
								var s:Int = (xm)? note.instrument.samplepernote[(note.peridx /* + cs.csmp.relative + transpose */) % 84] : 0;
								
								if (s < note.instrument.samplecount) {
									cs.csmp = note.instrument.sample[s];  //*
									
									cs.cvolume = cs.rvolume = cs.csmp.volume;
									cs.cslength = cs.csmp.length * 16384;
									cs.cslooplen = cs.cslength;
								}
							}
						}
						if (note.period != 0 && cs.csmp != null && !cs.slidetonote) {
							if (note.peridx != -1) {
								var n:Int = note.peridx + cs.csmp.relative + transpose;
								//if (Math.abs (cs.csmp.fine) == 30) 
								//	trace (" " + n + " " + (256 * (n + cs.csmp.fine * 84 + periodsbase) / periods.length));
								cs.cperiod = periods[n + cs.csmp.fine * 84 + periodsbase];
								if (xm) cs.pan = cs.csmp.pan;
							}
							else 
							{
								//trace(note.period);
								cs.cperiod = note.period;
							}
							cs.lastnoteperiod = cs.cperiod;
							var amigabps:Float = 7159090.5/(cs.cperiod*2);
							cs.cspinc = Std.int((amigabps/44100.0)*16384);
							cs.csp = 0;
							cs.slideperiod = false;
							if (cs.vibratowave < 4) cs.vibratopos = 0;
							if (cs.tremolowave < 4) cs.tremolopos = 0;
						}
						if (note.command != 0 || note.cmdarg != 0) {
							var cmd:Int = note.command;
							var arg:Int = note.cmdarg;
							switch (cmd) {
							case 0x00:
								cs.arpeggio = note.peridx != -1 || note.period == 0;
								if (note.period != 0) cs.arpeggionote = note.peridx + cs.csmp.relative + transpose;
								cs.arpeggiosemi1 = (arg&0xF0)>>4;
								cs.arpeggiosemi2 = arg&0x0F;
								cs.arpeggiotick = 0;
							case 0x01: 
								if (arg != 0) {
									cs.slideperiod = true;
									cs.slideperiodval = -arg;
								}
							case 0x02: 
								if (arg != 0) {
									cs.slideperiod = true;
									cs.slideperiodval = arg;
								}
							case 0x03:
								if (arg != 0) cs.slideperiodval = arg;
								if (note.period != 0) {
									cs.slidetonotetarget = note.peridx==1?note.period:periods[note.peridx + cs.csmp.relative + transpose + cs.csmp.fine*84 + periodsbase];
									if (cs.cperiod > note.period && cs.slideperiodval > 0)
										cs.slideperiodval = -cs.slideperiodval;
									else if (cs.cperiod < note.period && cs.slideperiodval < 0)
										cs.slideperiodval = -cs.slideperiodval;
								} else if (arg != 0) {
									if (cs.cperiod > cs.slidetonotetarget && cs.slideperiodval > 0)
										cs.slideperiodval = -cs.slideperiodval;
									else if (cs.cperiod < cs.slidetonotetarget && cs.slideperiodval < 0)
										cs.slideperiodval = -cs.slideperiodval;
								}
								if (cs.slidetonotetarget == 0) cs.slidetonote = false;
							case 0x04:
								if (arg != 0) {
									cs.vibratospeed = (arg&0xF0)>>4;
									cs.vibratodepth = arg&0x0F;
									cs.vibratopos = 0;
								}
							case 0x07:
								cs.tremolo = true;
								if (arg != 0) {
									cs.tremolospeed = (arg&0xF0)>>4;
									cs.tremolodepth = arg&0x0F;
									cs.tremolopos = 0;
								}
							case 0x08:
								cs.pan = (arg & 0xFF);
							case 0x09:
								if (arg != 0) {
									arg <<= 8;
									if (arg >= cs.csmp.length) arg = cs.csmp.length;
									cs.csp = arg << 14;
								}
							case 0x05,0x06,0x0A:
								cs.slidevolume = true;
								if (arg != 0) {
									if ((arg&0xF0) != 0) {
										cs.slidevolumeval = arg>>4;
									} else {
										cs.slidevolumeval = -(arg&0x0F);
									}
								}
							case 0x0B: // allows only skips to orders below
								if (arg > corder) {
									corder = arg;
									if (corder >= order.length) {
										corder = order.length-1;
										crow = 63;
									} else {
										crow = 0;
									}
									cpat = order[corder];
									cpattern = pat[cpat];
									nextcheckrow = true;
								}
							case 0x0C:
								if (arg > 0x40) arg = 0x40;
								cs.cvolume = cs.rvolume = arg;
							case 0x0D:
								arg = ((arg&0xF0)>>4)*10 + (arg&0x0F);
								if (arg > 0x3F) arg = 0x3F;
								breakpatonrow = true;
								breakpatnextrow = arg;
							case 0x0E:
								switch (arg&0xF0) {
								case 0x00: // ignore
								case 0x10:
									cs.cperiod -= arg&0x0F;
									var amigabps:Float = 7159090.5/(cs.cperiod*2);
									cs.cspinc = Std.int((amigabps/44100.0)*16384);
								case 0x20:
									cs.cperiod += arg&0x0F;
									var amigabps:Float = 7159090.5/(cs.cperiod*2);
									cs.cspinc = Std.int((amigabps/44100.0)*16384);
								case 0x30: notsupport(arg&0xF0,cpat,crow);
								case 0x40:
									switch (arg&0x0F) {
									case 0,1,2,4,5,6: cs.vibratowave = arg&0x0F;
									case 3,7: 
										cs.vibratowave = Std.int(Math.random()*4);
										if (cs.vibratowave > 3) cs.vibratowave = 3;
										if (arg&0x0F == 7) cs.vibratowave += 4;
									}
								case 0x50: notsupport(arg&0xF0,cpat,crow);
								case 0x60: notsupport(arg&0xF0,cpat,crow);
								case 0x70:
									switch (arg&0x0F) {
									case 0,1,2,4,5,6: cs.tremolowave = arg&0x0F;
									case 3,7: 
										cs.tremolowave = Std.int(Math.random()*4);
										if (cs.tremolowave > 3) cs.tremolowave = 3;
										if (arg&0x0F == 7) cs.tremolowave += 4;
									}
								case 0x80:
									cs.pan = (arg & 0x0F) << 4;
								case 0x90:
									cs.retriggersample = (arg&0x0F) > 0;
									cs.retriggersamplectr = 0;
									cs.retriggersampleticks = arg&0x0F;
								case 0xA0: 
									cs.cvolume += arg&0x0F;
									if (cs.cvolume > 64) cs.cvolume=64;
									cs.rvolume = cs.cvolume;
								case 0xB0: 
									cs.cvolume -= arg&0x0F;
									if (cs.cvolume < 0) cs.cvolume=0;
									cs.rvolume = cs.cvolume;
								case 0xC0:
									cs.cutsample = (arg&0x0F) > 0;
									cs.cutsampleticks = arg&0x0F;
								case 0xD0: 
									cs.delaynote = true;
									cs.delaynoteticks = arg&0x0F;
								case 0xE0:
									delaypattern = arg&0x0F;
								case 0xF0: // universally unsupported
								}
							case 0x0F:
								if (arg <= 32) {
									if (arg == 0) arg = 1;
									ticksperrow = arg;
								} else {
									rowspermin = arg*4;
									tickspermin = rowspermin*6;
									samplespertick = Std.int((44100*60)/tickspermin);
								}
							
							//*
							case 0x10:  // "G": set global volume xx=0..40
								globalvolume = ((arg << 2) - 1) & 0xFF;
							case 0x11:  // "H": global volume slide, x0 up, 0x down
								var x = (arg >> 4) & 0xF;
								var y = arg & 0xF;
								globalvolume += x << 4;
								if (globalvolume > 0xFF) globalvolume = 0xFF;
								globalvolume -= y << 4;
								if (globalvolume < 0) globalvolume = 0;
							case 0x14:  // "K": key off after xx ticks, xx=0..song speed-1
								// not implemented yet
							case 0x15:  // "L": set volume envelope position xx=pos
								// not implemented yet
							case 0x19:  // "P": panning slide x0=right, 0x=left
								var x = (arg >> 4) & 0xF;
								var y = arg & 0xF;
								cs.pan += x << 4;
								if (cs.pan > 0xFF) cs.pan = 0xFF;
								cs.pan -= y << 4;
								if (cs.pan < 0) cs.pan = 0;
							case 0x1B:  // "R": re-trigger note with volume slide
								// not implemented yet
							case 0x1D:  // "T": tremor x=#on ticks, y=#off ticks
								// not implemented yet
							case 0x21:  // "X": extra fine portamento 1x=up, 2x=down
								// not implemented yet
							}
						}
					}
                }
                
                if (checktick) {
                    if (rowtick > 0) {
                        if (cs.cutsample && cs.cutsampleticks <= rowtick) {
                            cs.cvolume = cs.rvolume = 0;
                            cs.cutsample = false;
                        }
                        
                        if (cs.delaynote && cs.delaynoteticks <= rowtick) {
                            cs.csp = 0;
                            cs.delaynote = false;
                            cs.cvolume = cs.rvolume = cs.csmp.volume;
                        }
                        
                        if (cs.retriggersample && cs.csmp != null) {
                            if (++cs.retriggersamplectr == cs.retriggersampleticks) {
                                cs.csp = 0;
								cs.cslength = cs.csmp.length << 14;
									
                                cs.cslooplen = cs.cslength;
                                cs.cvolume = cs.rvolume = cs.csmp.volume;
                                cs.retriggersamplectr = 0;
                            }
                        }
                        
                        if (cs.slidevolume) {
                            cs.cvolume += cs.slidevolumeval;
                            if (cs.cvolume <= 0)
                                cs.cvolume = 0;
                            else if (cs.cvolume > 64)
                                cs.cvolume = 64;
                            cs.rvolume = cs.cvolume;
                        }
                        
                        if (cs.slideperiod) {
                            cs.cperiod += cs.slideperiodval;
                            if (cs.cperiod < 113)
                                cs.cperiod = 113;
                            else if (cs.cperiod > 856)
                                cs.cperiod = 856;
                            var amigabps:Float = 7159090.5/(cs.cperiod*2);
                            cs.cspinc = Std.int((amigabps/44100.0)*16384);
                        }
                        
                        if (cs.slidetonote) {
                            cs.cperiod += cs.slideperiodval;
                            if (cs.slideperiodval < 0 && cs.slidetonotetarget > cs.cperiod) {
                                cs.cperiod = cs.slidetonotetarget;
                            } else if (cs.slideperiodval > 0 && cs.slidetonotetarget < cs.cperiod) {
                                cs.cperiod = cs.slidetonotetarget;
                            }
                            var amigabps:Float = 7159090.5/(cs.cperiod*2);
                            cs.cspinc = Std.int((amigabps/44100.0)*16384);
                        }
                    }
                    
                    if (cs.arpeggio) {
                        var aperiod:Int;
                        if (cs.arpeggiotick == 0) {
                            aperiod = periods[cs.arpeggionote + cs.csmp.fine*84 + periodsbase];
                            var amigabps:Float = 7159090.5/(aperiod*2);
                            cs.cspinc = Std.int((amigabps/44100.0)*16384);
                            cs.arpeggiotick++;
                        } else if (cs.arpeggiotick == 1) {
                            aperiod = periods[cs.arpeggionote + cs.csmp.fine*84 + cs.arpeggiosemi1 + periodsbase];
                            var amigabps:Float = 7159090.5/(aperiod*2);
                            cs.cspinc = Std.int((amigabps/44100.0)*16384);
                            cs.arpeggiotick++;
                        } else if (cs.arpeggiotick == 2) {
                            aperiod = periods[cs.arpeggionote + cs.csmp.fine*84 + cs.arpeggiosemi2 + periodsbase];
                            var amigabps:Float = 7159090.5/(aperiod*2);
                            cs.cspinc = Std.int((amigabps/44100.0)*16384);
                            cs.arpeggiotick = 0;
                        }
                    }

                    if (cs.vibrato) {
                        // note: MilkyTracker and OpenCubicPlayer do not seem to
                        // use other waves (at least the sound "sounds" the same
                        // no matter the waveform chosen). So unless someone can
                        // provide me a module where setting the wave for vibrato
                        // does make a difference (so i can test it), i'll use
                        // sinewave only here.
                        var vibval:Int = cs.vibratodepth*sinewave[cs.vibratopos&0x3F] >> 7;
                        var amigabps:Float = 7159090.5/((cs.cperiod+vibval)*2);
                        cs.cspinc = Std.int((amigabps/44100.0)*16384);
                        if (rowtick > 0) cs.vibratopos += cs.vibratospeed;
                    }
                    
                    if (cs.tremolo) {
                        // note:same case with waves like above
                        var trmval:Int = cs.tremolodepth*sinewave[cs.tremolopos&0x3F] >> 6;
                        cs.rvolume = cs.cvolume + trmval;
                        if (cs.rvolume < 0) cs.rvolume = 0; else if (cs.rvolume > 64) cs.rvolume = 64;
                        if (rowtick > 0) cs.tremolopos += cs.tremolospeed;
                    }
                }
                
                if (cs.csmp == null || cs.rvolume == 0) continue;
				
                if (!cs.delaynote) {
					var smp = (((cs.csmp.wave[cs.csp >> 14] * cs.rvolume) * globalvolume >> 8) * fadevolume) >> 8;  //*
					
					mixed += smp;
					mixedLeft += (smp*(0xFF-cs.pan))>>8;
					mixedRight += (smp*cs.pan)>>8;
				}
                
                cs.csp += cs.cspinc;
                if (cs.csp >= cs.cslooplen) {
                    if (cs.csmp.looplen < 2) {
                        cs.rvolume = 0;
                        continue;
                    } else {
						cs.csp = cs.csmp.loopstart << 14;
						cs.cslooplen = cs.csp + (cs.csmp.looplen << 14);
                    }
                }
            }
            
			if (!dyn)
				if (cnt >= wavelen) {
					wavelen += 1000000;
					wave.length = wavelen;
				}
			if (stereo) {
				if (mixedLeft < -32768)
					mixedLeft = -32768;
				else if (mixedLeft > 32767)
					mixedLeft = 32767;
				if (mixedRight < -32768)
					mixedRight = -32768;
				else if (mixedRight > 32767)
					mixedRight = 32767;
				if (dyn)
				{
					sde.data.writeFloat (mixedLeft / 32768);
					sde.data.writeFloat (mixedRight / 32768);
				}
				else
				{
					wave[cnt++] = mixedLeft & 0xFF;
					wave[cnt++] = mixedLeft >> 8;
					wave[cnt++] = mixedRight & 0xFF;
					wave[cnt++] = mixedRight >> 8;
				}
			} else {
				if (mixed < -32768)
					mixed = -32768;
				else if (mixed > 32767)
					mixed = 32767;
				if (dyn)
				{
					sde.data.writeFloat (mixed / 32768);
					sde.data.writeFloat (mixed / 32768);
				}
				else
				{
					wave[cnt++] = mixed & 0xFF;
					wave[cnt++] = mixed >> 8;
				}
			}
            if (checkrow) checkrow = nextcheckrow;
            
			if (checktick) {
				checktick = false;
				if (fadespeed != 0)
				{
					fadevolume += fadespeed;
					//trace(fadevolume);
					if (fadevolume < 0) fadevolume = 0; else if (fadevolume > 0xFF) fadevolume = 0xFF;
				}
			}
            
            if (++segsamples >= segbreak || stopnow) return true;
			
        }
        return true;
    }

    public function genWaveform(onFinish:Void->Void)
    {
        // init state
        corder = 0x00;
        cpat = order[corder];
        cpattern = pat[cpat];
        states = new Array<ChanState>();
        
		// rowspermin = 125*4;
		rowspermin = defaultbeatspermin * 4;  //*
		// ticksperrow = 6;
		ticksperrow = defaulttempo;
        // tickspermin = rowspermin * ticksperrow;
        tickspermin = rowspermin * 6;
		
        samplespertick = Std.int((44100*60)/tickspermin);
        ticksmpctr = -1;
        checkrow = true;
        checktick = true;
        delaypattern = 0;

		ticksperrow = defaulttempo;
        
        // init waves
        sinewave = [0,24,49,74,97,120,141,161,180,197,212,224,235,244,250,253,
                    255,253,250,244,235,224,212,197,180,161,141,120,97,74,49,
                    24,0,-24,-49,-74,-97,-120,-141,-161,-180,-197,-212,-224,
                    -235,-244,-250,-253,-255,-253,-250,-244,-235,-224,-212,-197,
                    -180,-161,-141,-120,-97,-74,-49,-24];
        
        notsupflag = new Array<Bool>();
        for (i in 0...256) notsupflag[i] = false;
        
        for (c in 0...chancount) {
            states[c] = new ChanState();
            states[c].csmp = null;
            states[c].cperiod = 0;
            states[c].cvolume = 64;
            states[c].rvolume = 64;
            states[c].csp = 0;
            states[c].cspinc = 0;
            states[c].slideperiod = false;
            states[c].vibratowave = 0;
            states[c].tremolowave = 0;
			states[c].channelpan = ((c + 1) & 0x2 != 0)? 0xFF : 0;  // left, right, right, left
			states[c].pan = states[c].channelpan;
        }
        
        xtrace("Generating waveform from module data");
		if (!dyn)
		{
			wave = new ByteArray();
			wave.length = wavelen = 1000000;
		}
        cnt = 0;
        
        stopnow = false;
        this.onFinish = onFinish;
		if (!dyn)
			calcSegTask(null);
    }
    
    var onFinish:Void->Void;
    var tmr:Timer;
    private function calcSegTask(d:Dynamic)
    {
        if (!calcSegment()) {
            if (onProgress != null) onProgress(100);
            onFinish();
			ready = true;
        } else {
            if (onProgress != null)
                onProgress(10 + Std.int(89 * corder / songlength));
            if (stopnow) {
                stopnow = false;
                return;
            }
            tmr = new Timer(1, 1);
            tmr.addEventListener(flash.events.TimerEvent.TIMER_COMPLETE, calcSegTask);
            tmr.start();
        }
    }
    
    var ldr:flash.display.Loader;
    public function beginPlayback()
    {
        if (stopnow) return;
        xtrace("Beginning playback");
        ldr = DynSound.playSound(wave, repeating, true, stereo);
        wave.length = 0;
    }
    
    /* === Public API === */
    /**
     * Set to a {@code Int->Void} function to receive progress status. The
     * {@code Int} argument will receive values from 0 to 100 depending on the
     * download and sound synthesis status. This function will always be
     * called with 100 as an argument to indicate "end". If the download or
     * synthesis fails, this function will be called with -1 as the argument.
     */
    public var onProgress:Dynamic;
    /**
     * The synthesis happens in segments and this variable contains the number
     * of samples between segments. If you increase the value, the synthesis
     * will be faster, but more laggy. If you decrease the value, the synthesis
     * will be slower but more responsive. Its probably a good idea to use
     * a big value for small MODs and small value for bigger MODs. Note that
     * "bigger" and "small" here means the length of the MOD in playback time, 
     * not filesize.
     */
    public var segbreak:Int;
    /**
     * If set to true, debugging traces will be shown.
     */
    public var showTraces:Bool;
    
    /**
     * Construct a ModPlayer
     */
    public function new()
    {
		dyn = false;
		sde = null;
		ready = false;
        showTraces = false;
        segbreak = 500000;
        repeating = true;
		stereo = true;
		fadevolume = 0xFF;
		fadespeed = 0;
		userorder = false;
		order = new Array<Int>();
		transpose = 0;
		samplespercallback = 3200;
    }
    
    /**
     * Play the MOD file contained in the given {@code ByteArray}.
     *
     * @param data the ByteArray containing a full MOD file.
     */
    public function playBytes(data:ByteArray)
    {
        data.endian = flash.utils.Endian.BIG_ENDIAN;
        stopnow = false;
        try {
            parseData(data);
        } catch (e:flash.Error) {
            xtrace('pderror - ' + e.message);
            if (onProgress != null) onProgress(-1);
        }
        var stamp:Float = haxe.Timer.stamp();
        var self = this;
        try {
			genWaveform(function() {
				self.xtrace("Waveform synthesis time: " + (haxe.Timer.stamp()-stamp) + " seconds");
				self.wave.length = self.cnt;
				self.beginPlayback();
			});
        } catch (e:flash.Error) {
            xtrace(e.message);
            if (onProgress != null) onProgress(-1);
            return;
        }
    }
    
    /**
     * Download and play a MOD file from the given URL.
     *
     * @param url the url to download the MOD from
     */
    public function play(url:String)
    {
        var req:URLRequest = new URLRequest(url);
        var loader:URLLoader = new URLLoader();
        var self:ModPlayer = this;
        xtrace("Downloading " + url);
        loader.dataFormat = flash.net.URLLoaderDataFormat.BINARY;
        try {
            loader.load(req);
        } catch (e:flash.Error) {
            xtrace("Flash error - " + e.message);
            if (onProgress != null) onProgress(-1);
            return;
        }
        loader.addEventListener(IOErrorEvent.IO_ERROR, function(d:Dynamic) {
            self.xtrace("IO Error while loading " + url);
            if (self.onProgress != null) self.onProgress(-1);
        });
        loader.addEventListener(Event.COMPLETE, function(d:Dynamic) {
			if (self.dyn) {
				self.parseData(cast(loader.data, ByteArray));
				self.genWaveform (function() { self.ready = false; } );
				self.ready = true;
			} else {
				self.xtrace("Finished loading " + loader.bytesTotal + " bytes");
				self.playBytes(cast(loader.data,ByteArray));
				loader.data.length = 0;
			}
        });
        loader.addEventListener(flash.events.ProgressEvent.PROGRESS, function(d:Dynamic) {
            if (self.onProgress != null) self.onProgress(Std.int(9*d.bytesLoaded/d.bytesTotal));
        });
    }
    
	
	public function playRes (res: String)
	{
		var ba: ByteArray = haxe.Resource.getBytes (res).getData ();
		parseData (ba);
		var self = this;
		genWaveform (function() { self.ready = false; } );
		ready = true;
	}
	
	
    /**
     * Stop MOD playback. Note: in Flash Player 9 (and currently even in Flash
     * Player 10) this will stop ALL sounds.
     */
    public function stop()
    {
        stopnow = true;
        flash.media.SoundMixer.stopAll();
    }
	
	
	public function sampleDataEventHandler (event: SampleDataEvent)
	{
		if (!ready)
		{
			for (i in 0...samplespercallback) 
			{
				event.data.writeFloat(0);
				event.data.writeFloat(0);
			}
		}
		if (ready)
		{
			segbreak = samplespercallback;
			sde = event;
			calcSegment();
		}
	}
	
}

