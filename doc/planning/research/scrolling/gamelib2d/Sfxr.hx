package gamelib2d;

import flash.media.SoundChannel;
import flash.utils.ByteArray;
import flash.display.Loader;

// haXe version by Mike Wiering
// ported from sfxr: Copyright (c) 2007 Tomas Pettersson
// license: MIT

class SfxrCategory
{
	public static inline var PICKUP_COIN = 1;
	public static inline var LASER_SHOOT = 2;
	public static inline var EXPLOSION   = 3;
	public static inline var POWER_UP    = 4;
	public static inline var HIT_HURT    = 5;
	public static inline var JUMP        = 6;
	public static inline var BLIP_SELECT = 7;
}

class Sfxr
{
	var baWave: ByteArray;
	
	var randSeed1: Int;
	var randSeed2: Int;
	
	
	public function setRandSeed (?seed1: Int, ?seed2: Int)
	{
		if (seed1 == null)
			randSeed1 = Date.now ().getDate () + Std.int (Date.now ().getTime () * 0x7FF);
		else
			randSeed1 = seed1;
		if (seed2 == null)
			randSeed2 = Date.now ().getSeconds () * Date.now ().getMinutes () * Date.now ().getHours () + Std.int (Date.now ().getTime () * 0xFF);
		else 
			randSeed2 = seed2;
	}
	
	private function rnd (n: Int)
	{
		randSeed1 = (randSeed1 + 0x152 + randSeed2) << 1;
		randSeed2 = (randSeed2 ^ 0x259) + randSeed1;
		randSeed1 = ((randSeed1 << 1) + randSeed2) & 0xFFFF;
		randSeed2 = ((randSeed2 & 0xFF) << 8) + ((randSeed2 & 0xFF00) >> 8);
		return (randSeed1 % (n + 1));
	}
	
 	private function frnd (range: Float): Float
	{
		return rnd (10000) / 10000 * range;
	}

	private inline function boolToInt (b: Bool): Int { return b? 1 : 0; }
	
	
	public var wave_type: Int;
	
	public var p_base_freq: Float;
	public var p_freq_limit: Float;
	public var p_freq_ramp: Float;
	public var p_freq_dramp: Float;
	public var p_duty: Float;
	public var p_duty_ramp: Float;

	public var p_vib_strength: Float;
	public var p_vib_speed: Float;
	public var p_vib_delay: Float;

	public var p_env_attack: Float;
	public var p_env_sustain: Float;
	public var p_env_decay: Float;
	public var p_env_punch: Float;

	public var filter_on: Bool;
	
	public var p_lpf_resonance: Float;
	public var p_lpf_freq: Float;
	public var p_lpf_ramp: Float;
	public var p_hpf_freq: Float;
	public var p_hpf_ramp: Float;

	public var p_pha_offset: Float;
	public var p_pha_ramp: Float;

	public var p_repeat_speed: Float;

	public var p_arp_speed: Float;
	public var p_arp_mod: Float;
	
	public var master_vol: Float;
	public var sound_vol: Float;
	
	var playing_sample: Bool;
	
	var phase: Int;
	var fperiod: Float;  // double
	var fmaxperiod: Float;  // double
	var fslide: Float;  // double
	var fdslide: Float;  // double
	var period: Int;
	var square_duty: Float;
	var square_slide: Float;
	var env_stage: Int;
	var env_time: Int;
	var env_length: Array<Int>; //= new Array ();  / / 3
	var env_vol: Float;
	var fphase: Float;
	var fdphase: Float;
	var iphase: Int;
	var phaser_buffer: Array<Float>; //= new Array ();  / / 1024
	var ipp: Int;
	var noise_buffer: Array<Float>; //= new Array ();  // 32

	var fltp: Float;
	var fltdp: Float;
	var fltw: Float;
	var fltw_d: Float;
	var fltdmp: Float;
	var fltphp: Float;
	var flthp: Float;
	var flthp_d: Float;
	var vib_phase: Float;
	var vib_speed: Float;
	var vib_amp: Float;
	var rep_time: Int;
	var rep_limit: Int;
	var arp_time: Int;
	var arp_limit: Int;
	var arp_mod: Float;  // double

	//float* vselected=NULL;
	var vcurbutton: Int; // = -1;

	var wav_bits: Int; // = 16;
	var wav_freq: Int; // = 44100;

	var file_sampleswritten: Int;
	var filesample: Float; // = 0.0;
	var fileacc: Int; // = 0;
	
	var stereo_pan: Float;
	
	
	public function new (?vol: Float = 0.5, ?freq: Int = 44100, ?bits: Int = 16, ?pan: Float = 0.0, ?rnd1: Int = 0, ?rnd2: Int = 0)
	{
		baWave = new ByteArray ();
		baWave.endian = flash.utils.Endian.LITTLE_ENDIAN;
		
		master_vol = 0.05;
		sound_vol = vol;
		playing_sample = false;
		env_length = new Array ();
		phaser_buffer = new Array ();
		noise_buffer = new Array ();
		vcurbutton = -1;
		wav_bits = bits;
		wav_freq = freq;
		filesample = 0.0;
		fileacc = 0;
		
		stereo_pan = pan;  // -1.0 ... 1.0
		setRandSeed (rnd1, rnd2);
		
		resetParams ();
	}

	public function resetParams ()
	{
		wave_type = 0;

		p_base_freq = 0.3;
		p_freq_limit = 0.0;
		p_freq_ramp = 0.0;
		p_freq_dramp = 0.0;
		p_duty = 0.0;
		p_duty_ramp = 0.0;

		p_vib_strength = 0.0;
		p_vib_speed = 0.0;
		p_vib_delay = 0.0;

		p_env_attack = 0.0;
		p_env_sustain = 0.3;
		p_env_decay = 0.4;
		p_env_punch = 0.0;

		filter_on = false;
		p_lpf_resonance = 0.0;
		p_lpf_freq = 1.0;
		p_lpf_ramp = 0.0;
		p_hpf_freq = 0.0;
		p_hpf_ramp = 0.0;

		p_pha_offset = 0.0;
		p_pha_ramp = 0.0;

		p_repeat_speed = 0.0;

		p_arp_speed = 0.0;
		p_arp_mod = 0.0;
	}
	
	
	public function resetSample (restart: Bool)
	{
		if (!restart)
			phase = 0;
		fperiod = 100.0 / (p_base_freq * p_base_freq + 0.000001);
		period = Std.int (fperiod);
		fmaxperiod = 100.0 / (p_freq_limit * p_freq_limit + 0.000001);
		fslide = 1.0 - Math.pow (p_freq_ramp, 3.0) * 0.01;
		fdslide = -Math.pow (p_freq_dramp, 3.0) * 0.000001;
		square_duty = 0.5 - p_duty * 0.5;
		square_slide = -p_duty_ramp * 0.00005;
		if (p_arp_mod >= 0.0)
			arp_mod = 1.0 - Math.pow (p_arp_mod, 2.0) * 0.9;
		else
			arp_mod = 1.0 + Math.pow (p_arp_mod, 2.0) * 10.0;
		arp_time = 0;
		arp_limit = Std.int (Math.pow (1.0 - p_arp_speed, 2.0) * 20000 + 32);
		if (p_arp_speed == 1.0)
			arp_limit = 0;
		if (!restart)
		{
			// reset filter
			fltp = 0.0;
			fltdp = 0.0;
			fltw = Math.pow (p_lpf_freq, 3.0) * 0.1;
			fltw_d = 1.0 + p_lpf_ramp * 0.0001;
			fltdmp = 5.0 / (1.0 + Math.pow (p_lpf_resonance, 2.0) * 20.0) * (0.01 + fltw);
			if (fltdmp > 0.8) fltdmp = 0.8;
			
			fltphp = 0.0;
			flthp = Math.pow (p_hpf_freq, 2.0) * 0.1;
			flthp_d = 1.0 + p_hpf_ramp * 0.0003;
			// reset vibrato
			vib_phase = 0.0;
			vib_speed = Math.pow (p_vib_speed, 2.0) * 0.01;
			vib_amp = p_vib_strength * 0.5;
			// reset envelope
			env_vol = 0.0;
			env_stage = 0;
			env_time = 0;
			env_length[0] = Std.int (p_env_attack * p_env_attack * 100000.0);
			env_length[1] = Std.int (p_env_sustain * p_env_sustain * 100000.0);
			env_length[2] = Std.int (p_env_decay * p_env_decay * 100000.0);

			fphase = Math.pow (p_pha_offset, 2.0) * 1020.0;
			if (p_pha_offset < 0.0) fphase = -fphase;
			fdphase = Math.pow (p_pha_ramp, 2.0) * 1.0;
			if (p_pha_ramp < 0.0) fdphase = -fdphase;
			iphase = Std.int (Math.abs (fphase));
			ipp = 0;
			for (i in 0...1024)
				phaser_buffer[i] = 0.0;

			for (i in 0...32)
				noise_buffer[i] = frnd (2.0) - 1.0;

			rep_time = 0;
			rep_limit = Std.int (Math.pow (1.0 - p_repeat_speed, 2.0) * 20000 + 32);
			if (p_repeat_speed == 0.0)
				rep_limit = 0;
		}
	}
	
	
	function playSample ()
	{
		resetSample (false);
		playing_sample = true;
	}
	

	//void SynthSample(int length, float* buffer, FILE* file)
	function synthSample (length: Int, ba: ByteArray)
	{
		for (i in 0...length)
		{
			if (!playing_sample)
				break;

			rep_time++;
			if (rep_limit != 0 && rep_time >= rep_limit)
			{
				rep_time = 0;
				resetSample (true);
			}

			// frequency envelopes/arpeggios
			arp_time++;
			if (arp_limit != 0 && arp_time >= arp_limit)
			{
				arp_limit = 0;
				fperiod *= arp_mod;
			}
			fslide += fdslide;
			fperiod *= fslide;
			if (fperiod > fmaxperiod)
			{
				fperiod = fmaxperiod;
				if (p_freq_limit > 0.0)
					playing_sample = false;
			}
			var rfperiod: Float = fperiod;
			if (vib_amp > 0.0)
			{
				vib_phase += vib_speed;
				rfperiod = fperiod * (1.0 + Math.sin (vib_phase) * vib_amp);
			}
			period = Std.int (rfperiod);
			if (period < 8) period = 8;
			square_duty += square_slide;
			if (square_duty < 0.0) square_duty = 0.0;
			if (square_duty > 0.5) square_duty = 0.5;		
			// volume envelope
			env_time++;
			if (env_time > env_length[env_stage])
			{
				env_time = 0;
				env_stage++;
				if (env_stage == 3)
					playing_sample = false;
			}
			if (env_stage == 0)
				env_vol = env_time / env_length[0];
			if (env_stage == 1)
				env_vol = 1.0 + Math.pow (1.0 - env_time / env_length[1], 1.0) * 2.0 * p_env_punch;
			if (env_stage == 2)
				env_vol = 1.0 - env_time / env_length[2];

			// phaser step
			fphase += fdphase;
			iphase = Std.int (Math.abs (fphase));
			if (iphase > 1023) iphase = 1023;

			if (flthp_d != 0.0)
			{
				flthp *= flthp_d;
				if (flthp < 0.00001) flthp=0.00001;
				if (flthp > 0.1) flthp=0.1;
			}

			var ssample: Float = 0.0;
			for (si in 0...8) // 8x supersampling
			{
				var sample: Float = 0.0;
				phase++;
				if (phase >= period)
				{
	//				phase = 0;
					phase %= period;
					if (wave_type == 3)
						for (i in 0...32)
							noise_buffer[i] = frnd (2.0) - 1.0;
				}
				// base waveform
				var fp: Float = phase / period;
				switch (wave_type)
				{
					case 0: // square
						if (fp < square_duty)
							sample = 0.5;
						else
							sample = -0.5;
					case 1: // sawtooth
						sample = 1.0 - fp * 2;
					case 2: // sine
						sample = Math.sin (fp * 2 * Math.PI);
					case 3: // noise
						sample = noise_buffer[Std.int (phase * 32 / period)];
				}
				// lp filter
				var pp: Float = fltp;
				fltw *= fltw_d;
				if (fltw < 0.0) fltw = 0.0;
				if (fltw > 0.1) fltw = 0.1;
				if (p_lpf_freq != 1.0)
				{
					fltdp += (sample - fltp) * fltw;
					fltdp -= fltdp * fltdmp;
				}
				else
				{
					fltp = sample;
					fltdp = 0.0;
				}
				fltp += fltdp;
				// hp filter
				fltphp += fltp - pp;
				fltphp -= fltphp * flthp;
				sample = fltphp;
				// phaser
				phaser_buffer[ipp & 1023] = sample;
				sample += phaser_buffer[(ipp - iphase + 1024) & 1023];
				ipp = (ipp + 1) & 1023;
				// final accumulation and envelope application
				ssample += sample * env_vol;
			}
			ssample = ssample / 8 * master_vol;

			ssample *= 2.0 * sound_vol;

			/*
			if (buffer != NULL)
			{
				if (ssample>1.0) ssample=1.0;
				if (ssample<-1.0) ssample=-1.0;
				*buffer++=ssample;
			}
			*/
			
			if (ba != null)
			{
				// quantize depending on format
				// accumulate/count to accomodate variable sample rate?
				ssample *= 4.0; // arbitrary gain to get reasonable output volume...
				if (ssample > 1.0) ssample = 1.0;
				if (ssample < -1.0) ssample = -1.0;
				filesample += ssample;
				fileacc++;
				if (wav_freq==44100 || fileacc == 2)
				{
					filesample /= fileacc;
					fileacc = 0;
					if (wav_bits == 16)
					{
					/*
						short isample = (short)(filesample * 32000);
						fwrite(&isample, 1, 2, file);
					*/
						if (stereo_pan == 0.0)
							ba.writeShort (Std.int (filesample * 32000));
						else
						{
							var pan: Float = (stereo_pan + 1.0) / 2;
							var left: Float = filesample * (1 - pan);
							var right: Float = filesample * (pan);
							ba.writeShort (Std.int (left * 32000));
							ba.writeShort (Std.int (right * 32000));
						}
					}
					else
					{
					/*
						unsigned char isample=(unsigned char)(filesample*127+128);
						fwrite(&isample, 1, 1, file);
					*/
						if (stereo_pan == 0.0)
							ba.writeByte (Std.int (filesample * 127 + 128));
						else
						{
							var pan: Float = (stereo_pan + 1.0) / 2;
							var left: Float = filesample * (1 - pan);
							var right: Float = filesample * (pan);
							ba.writeByte (Std.int (left * 127 + 128));
							ba.writeByte (Std.int (right * 127 + 128));
						}
					}
					filesample = 0.0;
				}
				file_sampleswritten++;
			}
		}
	}

	
	// use -resource sound.sfs@sound_sfs
	// load (Resource.getBytes("sound_sfs").getData());
	
	public function load (ba: ByteArray): Bool
	{
		ba.endian = flash.utils.Endian.LITTLE_ENDIAN;
		var version: Int = ba.readInt ();
		if (version != 100 && version != 101 && version != 102)
			return false;
		wave_type = ba.readInt ();
		sound_vol = 0.5;
		if (version == 102)
			sound_vol = ba.readFloat ();
		p_base_freq = ba.readFloat ();
		p_freq_limit = ba.readFloat ();
		p_freq_ramp = ba.readFloat ();
		if (version >= 101)
			p_freq_dramp = ba.readFloat ();
		p_duty = ba.readFloat ();
		p_duty_ramp = ba.readFloat ();

		p_vib_strength = ba.readFloat ();
		p_vib_speed = ba.readFloat ();
		p_vib_delay = ba.readFloat ();

		p_env_attack = ba.readFloat ();
		p_env_sustain = ba.readFloat ();
		p_env_decay = ba.readFloat ();
		p_env_punch = ba.readFloat ();

		filter_on = ba.readByte () != 0;
		p_lpf_resonance = ba.readFloat ();
		p_lpf_freq = ba.readFloat ();
		p_lpf_ramp = ba.readFloat ();
		p_hpf_freq = ba.readFloat ();
		p_hpf_ramp = ba.readFloat ();
		
		p_pha_offset = ba.readFloat ();
		p_pha_ramp = ba.readFloat ();

		p_repeat_speed = ba.readFloat ();

		if (version >= 101)
		{
			p_arp_speed = ba.readFloat ();
			p_arp_mod = ba.readFloat ();
		}
		return true;
	}
	
	
	public function create (category: Int)
	{
		switch (category)
		{
			case SfxrCategory.PICKUP_COIN:
			{
				resetParams ();
				p_base_freq = 0.4 + frnd (0.5);
				p_env_attack = 0.0;
				p_env_sustain = frnd (0.1);
				p_env_decay = 0.1 + frnd (0.4);
				p_env_punch = 0.3 + frnd (0.3);
				if (rnd (1) != 0)
				{
					p_arp_speed = 0.5 + frnd (0.2);
					p_arp_mod = 0.2 + frnd (0.4);
				}
			}
			case SfxrCategory.LASER_SHOOT:
			{
				resetParams ();
				wave_type=rnd (2);
				if (wave_type == 2 && rnd (1) != 0)
					wave_type = rnd (1);
				p_base_freq = 0.5 + frnd (0.5);
				p_freq_limit = p_base_freq - 0.2 - frnd (0.6);
				if (p_freq_limit < 0.2) p_freq_limit = 0.2;
				p_freq_ramp = -0.15 - frnd (0.2);
				if (rnd (2)==0)
				{
					p_base_freq = 0.3 + frnd (0.6);
					p_freq_limit = frnd (0.1);
					p_freq_ramp = -0.35 - frnd (0.3);
				}
				if (rnd (1) != 0)
				{
					p_duty = frnd (0.5);
					p_duty_ramp = frnd (0.2);
				}
				else
				{
					p_duty = 0.4 + frnd (0.5);
					p_duty_ramp = -frnd (0.7);
				}
				p_env_attack = 0.0;
				p_env_sustain = 0.1 + frnd (0.2);
				p_env_decay = frnd (0.4);
				if (rnd (1) != 0)
					p_env_punch = frnd (0.3);
				if (rnd (2) == 0)
				{
					p_pha_offset = frnd (0.2);
					p_pha_ramp = -frnd (0.2);
				}
				if (rnd (1) != 0)
					p_hpf_freq = frnd (0.3);
			}
			case SfxrCategory.EXPLOSION:
			{
				resetParams ();
				wave_type = 3;
				if (rnd (1) != 0)
				{
					p_base_freq = 0.1 + frnd (0.4);
					p_freq_ramp = -0.1 + frnd (0.4);
				}
				else
				{
					p_base_freq = 0.2 + frnd (0.7);
					p_freq_ramp = -0.2 - frnd (0.2);
				}
				p_base_freq *= p_base_freq;
				if (rnd (4) == 0)
					p_freq_ramp = 0.0;
				if (rnd (2) == 0)
					p_repeat_speed = 0.3 + frnd (0.5);
				p_env_attack = 0.0;
				p_env_sustain = 0.1 + frnd (0.3);
				p_env_decay = frnd (0.5);
				if (rnd (1) == 0)
				{
					p_pha_offset = -0.3 + frnd (0.9);
					p_pha_ramp = -frnd (0.3);
				}
				p_env_punch = 0.2 + frnd (0.6);
				if (rnd (1) != 0)
				{
					p_vib_strength = frnd (0.7);
					p_vib_speed = frnd (0.6);
				}
				if (rnd (2) == 0)
				{
					p_arp_speed = 0.6 + frnd (0.3);
					p_arp_mod = 0.8 - frnd (1.6);
				}
			}
			case SfxrCategory.POWER_UP:
			{
				resetParams ();
				if (rnd (1) != 0)
					wave_type = 1;
				else
					p_duty = frnd (0.6);
				if (rnd (1) != 0)
				{
					p_base_freq = 0.2 + frnd (0.3);
					p_freq_ramp = 0.1 + frnd (0.4);
					p_repeat_speed = 0.4 + frnd (0.4);
				}
				else
				{
					p_base_freq = 0.2 + frnd (0.3);
					p_freq_ramp = 0.05 + frnd (0.2);
					if (rnd (1) != 0)
					{
						p_vib_strength = frnd (0.7);
						p_vib_speed = frnd (0.6);
					}
				}
				p_env_attack = 0.0;
				p_env_sustain = frnd (0.4);
				p_env_decay = 0.1 + frnd (0.4);
			}
			case SfxrCategory.HIT_HURT:
			{
				resetParams ();
				wave_type = rnd (2);
				if (wave_type == 2)
					wave_type = 3;
				if (wave_type == 0)
					p_duty = frnd (0.6);
				p_base_freq = 0.2 + frnd (0.6);
				p_freq_ramp = -0.3 - frnd (0.4);
				p_env_attack = 0.0;
				p_env_sustain = frnd (0.1);
				p_env_decay = 0.1 + frnd (0.2);
				if (rnd (1) != 0)
					p_hpf_freq = frnd (0.3);
			}
			case SfxrCategory.JUMP:
			{
				resetParams ();
				wave_type = 0;
				p_duty=frnd (0.6);
				p_base_freq = 0.3 + frnd (0.3);
				p_freq_ramp = 0.1 + frnd (0.2);
				p_env_attack = 0.0;
				p_env_sustain = 0.1 + frnd (0.3);
				p_env_decay = 0.1 + frnd (0.2);
				if (rnd (1) != 0)
					p_hpf_freq = frnd (0.3);
				if (rnd (1) != 0)
					p_lpf_freq = 1.0 - frnd (0.6);
			}
			case SfxrCategory.BLIP_SELECT:
			{
				resetParams ();
				wave_type = rnd (1);
				if (wave_type == 0)
					p_duty=frnd (0.6);
				p_base_freq = 0.2 + frnd (0.4);
				p_env_attack = 0.0;
				p_env_sustain = 0.1 + frnd (0.1);
				p_env_decay = frnd (0.2);
				p_hpf_freq = 0.1;
			}
			
		}
	}
	
	
	// copy string from http://superflashbros.net/as3sfxr/
	public function setSettingsString (s: String)
	{
		var fv: Array<Float> = new Array ();
		var sv: Array <String> = s.split (",");
		for (i in 0...sv.length)
			fv[i] = Std.parseFloat ("0" + sv[i]);
				
		resetParams ();
		
		wave_type = Std.int (fv[0]); // waveType
		p_env_attack = fv[1]; // attackTime
		p_env_sustain = fv[2]; // sustainTime
		p_env_punch = fv[3]; // sustainPunch
		p_env_decay = fv[4]; // decayTime
		p_base_freq = fv[5]; // startFrequency
		p_freq_limit = fv[6]; // minFrequency
		p_freq_ramp = fv[7]; // slide
		p_freq_dramp = fv[8]; // deltaSlide
		vib_amp = fv[9]; // vibratoDepth
		vib_speed = fv[10]; // vibratoSpeed
		p_arp_mod = fv[11]; // changeAmount
		p_arp_speed = fv[12]; // changeSpeed
		p_duty = fv[13]; // squareDuty
		p_duty_ramp = fv[14]; // dutySweep
		p_repeat_speed = fv[15]; // repeatSpeed
		p_pha_offset = fv[16]; // phaserOffset
		p_pha_ramp = fv[17]; // phaserSweep
		p_lpf_freq = fv[18]; // lpFilterCutoff
		p_lpf_ramp = fv[10]; // lpFilterCutoffSweep
		p_lpf_resonance = fv[20]; // lpFilterResonance
		p_hpf_freq = fv[21]; // hpFilterCutoff
		p_hpf_ramp = fv[22]; // hpFilterCutoffSweep
		sound_vol = fv[23]; // masterVolume
	}
	
	
	public function transpose (halfNotes: Int, ?base: Float)
	{
		// base: use 0.125 for 440 Hz A
		if (base == null)
			base = p_base_freq * p_base_freq;
		p_base_freq = Math.sqrt (base * Math.pow (2, halfNotes / 12));
	}
	
	
	public function generate ()
	{
		baWave.length = 0;
		file_sampleswritten = 0;
		filesample = 0.0;
		fileacc = 0;
		playSample ();
		while (playing_sample)
			synthSample (256, baWave);
	}
	

	public function randomize ()
	{
		p_base_freq = Math.pow (frnd (2.0) - 1.0, 2.0);
		if (rnd (1) != 0)
			p_base_freq = Math.pow (frnd (2.0) - 1.0, 3.0) + 0.5;
		p_freq_limit = 0.0;
		p_freq_ramp = Math.pow (frnd (2.0) - 1.0, 5.0);
		if (p_base_freq > 0.7 && p_freq_ramp > 0.2)
			p_freq_ramp = -p_freq_ramp;
		if (p_base_freq < 0.2 && p_freq_ramp < -0.05)
			p_freq_ramp = -p_freq_ramp;
		p_freq_dramp = Math.pow (frnd (2.0) - 1.0, 3.0);
		p_duty = frnd (2.0) - 1.0;
		p_duty_ramp = Math.pow (frnd (2.0) - 1.0, 3.0);
		p_vib_strength = Math.pow (frnd (2.0) - 1.0, 3.0);
		p_vib_speed = frnd (2.0) - 1.0;
		p_vib_delay = frnd (2.0) - 1.0;
		p_env_attack = Math.pow (frnd (2.0) - 1.0, 3.0);
		p_env_sustain = Math.pow (frnd (2.0) - 1.0, 2.0);
		p_env_decay = frnd (2.0) - 1.0;
		p_env_punch = Math.pow (frnd (0.8), 2.0);
		if (p_env_attack + p_env_sustain + p_env_decay < 0.2)
		{
			p_env_sustain += 0.2 + frnd (0.3);
			p_env_decay += 0.2 + frnd (0.3);
		}
		p_lpf_resonance = frnd (2.0) - 1.0;
		p_lpf_freq = 1.0 - Math.pow (frnd (1.0), 3.0);
		p_lpf_ramp = Math.pow (frnd (2.0) - 1.0, 3.0);
		if (p_lpf_freq < 0.1 && p_lpf_ramp < -0.05)
			p_lpf_ramp = -p_lpf_ramp;
		p_hpf_freq = Math.pow (frnd (1.0), 5.0);
		p_hpf_ramp = Math.pow (frnd (2.0) - 1.0, 5.0);
		p_pha_offset = Math.pow (frnd (2.0) - 1.0, 3.0);
		p_pha_ramp = Math.pow (frnd (2.0) - 1.0, 3.0);
		p_repeat_speed = frnd (2.0) - 1.0;
		p_arp_speed = frnd (2.0) - 1.0;
		p_arp_mod = frnd (2.0) - 1.0;
	}
	
	public function mutate ()
	{
		if (rnd (1) != 0) p_base_freq += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_freq_ramp += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_freq_dramp += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_duty += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_duty_ramp += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_vib_strength += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_vib_speed += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_vib_delay += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_env_attack += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_env_sustain += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_env_decay += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_env_punch += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_lpf_resonance += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_lpf_freq += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_lpf_ramp += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_hpf_freq += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_hpf_ramp += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_pha_offset += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_pha_ramp += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_repeat_speed += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_arp_speed += frnd (0.1) - 0.05;
		if (rnd (1) != 0) p_arp_mod += frnd (0.1) - 0.05;
	}

	
	public function play ()
	{
		playWave (baWave, 2 + boolToInt (wav_freq == 44100), boolToInt (wav_bits == 16), boolToInt (stereo_pan != 0.0));
	}
	
	public function clear ()
	{
		baWave.length = 0;
	}
	
	
	// based on BadSector's DynSound.hx:
    public static function playWave (wave: ByteArray, ?rate: Int = 3, ?is16bits: Int = 0, ?stereo: Int = 0)
    {
        var swf: ByteArray = new ByteArray ();
        swf.endian = flash.utils.Endian.LITTLE_ENDIAN;
        
        swf.writeByte (0x46);  // 'SWF' signature
        swf.writeByte (0x57);
        swf.writeByte (0x53);
        swf.writeByte (0x07);  // version
        swf.writeUnsignedInt (0);  // filesize (will be set later)
        swf.writeByte (0x78);  // area size
        swf.writeByte (0x00);
        swf.writeByte (0x05);
        swf.writeByte (0x5F);
        swf.writeByte (0x00);
        swf.writeByte (0x00);
        swf.writeByte (0x0F);
        swf.writeByte (0xA0);
        swf.writeByte (0x00);
        swf.writeByte (0x00);  // framerate (12fps)
        swf.writeByte (0x0C);
        
		swf.writeShort (1);  // one frame
        // DefineSound tag
        writeTagInfo (swf, 14, 2 + 1 + 4 + wave.length);
        swf.writeShort (1);  // sound (character) ID

        swf.writeByte ((3 << 4) + (rate << 2) + (is16bits << 1) + stereo);
		// sound format bits:
		//  7654   32    1      0    
		// format rate 16bit stereo 
		
        swf.writeUnsignedInt (wave.length >> (is16bits + stereo)); // sample count
        swf.writeBytes (wave);  // data
		
        // StartSound tag
        writeTagInfo (swf, 15, 2 + 1);
        swf.writeShort (1);  // character id of the sound
        swf.writeByte(0);  // SOUNDINFO flags (all 0)
		
        // End tag
        writeTagInfo(swf, 0, 0);
        
        // Set size
        swf.position = 4;
        swf.writeUnsignedInt (swf.length);
        swf.position = 0;
        
        // load it
        var ldr: Loader;
        ldr = new Loader ();
        ldr.loadBytes (swf);
    }
	
    private static function writeTagInfo (swf: ByteArray, code: Int, len: Int)
    {
        if (len >= 63)
        {
            swf.writeShort ((code << 6) | 0x3F);
            swf.writeInt (len);
        }
        else swf.writeShort ((code << 6) | len);
    }

}


