package gamelib2d;

/*
 * Flash 9 Dynamic Sound Playback (DynSound)
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

import flash.display.Loader;
import flash.utils.ByteArray;

/*! @module "Flash MOD Player" */

/**
 * Contains static functions for playing back synthesized sound data. This
 * works by creating a Flash9 Sound object in a synthesized SWF file which is
 * loaded using the loadBytes function from Flash9's Loader object.
 */
class DynSound
{
    private static function writeTagInfo(swf:ByteArray,code:Int,len:Int)
    {
        if (len >= 63) {
            swf.writeShort((code << 6)|0x3F);
            swf.writeInt(len);
        } else swf.writeShort((code << 6)|len);
    }

    /**
     * Play a waveform sound.
     *
     * @param wave a ByteArray containing the sound waveform.
     * @param repeat repeat the sound playback
     * @param sixteen use 16bit samples (two bytes per sample) instead of 8bit
     * @return the Loader object constructed by calling this function
     */
    public static function playSound(wave:ByteArray,repeat:Bool,sixteen:Bool,stereo:Bool):Loader
    {
        var swf:ByteArray = new ByteArray();
        var ldr:Loader;
        
        // generate the file
        swf.endian = flash.utils.Endian.LITTLE_ENDIAN;
        
        // SWF header
        swf.writeByte(0x46);    // 'FWS' signature
        swf.writeByte(0x57);
        swf.writeByte(0x53);
        swf.writeByte(0x07);    // version
        swf.writeUnsignedInt(0);// filesize (will be set later)
        swf.writeByte(0x78);    // area size
        swf.writeByte(0x00);
        swf.writeByte(0x05);
        swf.writeByte(0x5F);
        swf.writeByte(0x00);
        swf.writeByte(0x00);
        swf.writeByte(0x0F);
        swf.writeByte(0xA0);
        swf.writeByte(0x00);
        swf.writeByte(0x00);   // framerate (12fps)
        swf.writeByte(0x0C);
        swf.writeShort(1);      // one frame
        
        // DefineSound tag
        writeTagInfo(swf, 14, 2 + 1 + 4 + wave.length); 
        swf.writeShort(1);      // sound (character) ID
		
		swf.writeByte(0x3C + ((sixteen)? 2 : 0) + ((stereo)? 1 : 0));
			// sound format (uncompressed) = 4 bits (3)
			// 44100 rate = 2 bits (3)
			// 16bit samples = 1 bit
			// stereo sound = 1 bit
			
        swf.writeUnsignedInt(wave.length); // sample count (one byte=one sample)
        swf.writeBytes(wave);   // samples
        
        // StartSound tag
        writeTagInfo(swf, 15, 2 + 1);
        swf.writeShort(1);      // character id of the sound
        if (repeat) {
            swf.writeByte(4);    // thanks to oNyx for this :-)
            swf.writeShort(65535);
        } else swf.writeByte(0);       // SOUNDINFO flags (all 0)
        
        // End tag
        writeTagInfo(swf, 0, 0);
        
        // Set size
        swf.position = 4;
        swf.writeUnsignedInt(swf.length);
        swf.position = 0;
        
        // "load" it
        ldr = new Loader();
        ldr.loadBytes(swf);
        ldr.addEventListener(flash.events.Event.COMPLETE, function(d:Dynamic){
            swf.length = 0;
        });
        
        return ldr;
    }
    
    /**
     * Basically the same as {@link playSound} but skips the WAV file header
     * bytes. It doesn't parse the WAV data, so the sixteen argument must be
     * valid. It also assumes that the wave data are at the end of the file.
     *
     * @param wave a ByteArray pointing to WAV file data.
     * @param repeat if true the wave will be repeated.
     * @param sixteen if true the wave is 16bit data instead of 8bit.
     * @param volume to play the wave (255=full, 0=silent).
     * @return the Loader object used to play the waveform or null if failed.
     */
    public static function playWAV(wave:ByteArray,repeat:Bool,sixteen:Bool,volume:Int):Loader
    {
        if (volume <= 0) return null;
        if (volume > 255) volume = 255;
        wave.endian = flash.utils.Endian.LITTLE_ENDIAN;
        wave.position += 12; // skip 'RIFF' <size> 'WAVE'
        while (wave.bytesAvailable > 0) {
            var c1:Int = wave.readByte();
            var c2:Int = wave.readByte();
            var c3:Int = wave.readByte();
            var c4:Int = wave.readByte();
            if (c1 == 0x64 && c2 == 0x61 && c3 == 0x74 && c4 == 0x61) { // 'data'
                if (volume == 255) {
                    wave.position += 4; // skip size;
                    return playSound(wave, repeat, sixteen, false);
                } else {
                    var size:Int = wave.readInt();
                    var pos:Int = 0;
                    var ww:ByteArray = new ByteArray();
                    ww.endian = flash.utils.Endian.LITTLE_ENDIAN;
                    if (sixteen) {
                        while (pos < size) {
                            ww.writeShort((wave.readShort()*volume) >> 8);
                            pos += 2;
                        }
                    } else {
                        while (pos < size) {
                            ww.writeByte((wave.readByte()*volume) >> 8);
                            pos++;
                        }
                    }
                    ww.position = 0;
                    return playSound(ww, repeat, sixteen, false);
                }
            } else {
                var s1:Int = wave.readByte();
                var s2:Int = wave.readByte();
                var s3:Int = wave.readByte();
                var s4:Int = wave.readByte();
                var size:UInt = (s4 << 24)|(s3 << 16)|(s2 << 8)|s1;
                wave.position += size;
            }
        }
        return null;
    }
}

