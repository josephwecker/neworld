/**
 *  Haxe memory scrolling version of BitmapDataCollectionSampler
 * 
 * @author Glenn Ko
 */

package com.flashartofwar;

import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.Error;
import flash.geom.Rectangle;
import flash.Memory;
import flash.system.ApplicationDomain;
import flash.utils.ByteArray;
import flash.utils.Endian;
import flash.Vector;


class BitmapScrollerHaxe extends Bitmap
{
	
	var _totalLength:Int;
	var _breath:Int;
	function getTotalLength():Int {
		return _totalLength;
	}
	function getTotalLengthInBytes():Int {
		return _totalLength << 2;
	}
	function getBreath():Int {
		return _breath;
	}
	var _internalBmpData:BitmapData;
	var _internalBuffer:ByteArray;
	var _internalSampleArea:Rectangle;
	var _invalidSize:Bool;
	public var scrollX:Int;
	

	
	/** Set this to a custom memory address if needed */
	public var mem_offset:Int;
	
	public function new() 
	{
		super();
		scrollX = 0;
		_totalLength = 0;
		_breath = 0;
		mem_offset = 0;
		_internalSampleArea = new Rectangle(0, 0, 0, 0);
	}
	
	public function getBitmapData():BitmapData {
		return _internalBmpData;
	}
	
	/**
	 * PostConstruct method to initialise data collection and bitmapdata into memory
	 * @param	collection
	 * @param	sampleArea
	 */
	public function init(collection:Vector<BitmapData>, sampleArea:Rectangle) {
		var bmd:BitmapData;
		if (collection.length < 1) throw new Error("Empty bitmapdata collection!");
		
		var calcTotalLength:Int = 0;
		for (i in 0...collection.length) {
			bmd = collection[i];
			calcTotalLength += bmd.width;
      
			if (bmd.height != sampleArea.height) {
				throw new Error("Sorry, no rescaling routine at the moment:");
				// need to rescale to fit bounds
			}
			
		}
		_totalLength = calcTotalLength;
		var w:Int;
		var h:Int;
		
		w = Std.int(sampleArea.height);
		h = Std.int(sampleArea.width);
		
		setInternalBmpData( new BitmapData(w , h, false, 0x000000) );
		_internalSampleArea = new Rectangle(0, 0, w, h);
		_breath = w;
		
		_internalBuffer = new ByteArray();
		_internalBuffer.length = (_totalLength * _breath) << 2;  

		
		_internalBuffer.endian = Endian.LITTLE_ENDIAN;
		_internalBuffer.position = 0;
	
		Memory.select(_internalBuffer);
		
		var lastX:Int = 0;
		
		for (i in 0...collection.length) {
			bmd = collection[i];
			copyPixelsOf(bmd, lastX);
			lastX += bmd.width; 
			
		}
	}
	
	
         public function getWidth():Float
        {
            return _internalSampleArea.width;
        }

         public function setWidth(value:Float):Void
        {
            if (value == _internalSampleArea.height) return;
            else
            {
                _internalSampleArea.height = value;
               invalidate();
            }
        }

        public function getHeight():Float
        {
            return _internalSampleArea.height;
        }

        public function setHeight(value:Float):Void
        {
			// This shouldn't execute because width breath is immutable for this.
			// Changing such a dimension would be costly because the entire memory
			// might need to be re-cached again.
			return;
            if (value == _internalSampleArea.width)
            {
                return;
            }
            else
            {
                _internalSampleArea.width = value;
                invalidate();
            }
        }
		
				
		inline function invalidate():Void {
			_invalidSize = true;
		}
		
	
	inline function copyPixelsOf(srcBitmapData:BitmapData, positionPx:Int):Void {
		// Worse case scenerio , read in 2 dimensions to translate pixels
		var h:Int = srcBitmapData.height;
		var w:Int = srcBitmapData.width;
		for (v in 0...h) {
			for (u in 0...w) {
				setPixelMemAt(v , positionPx+u, srcBitmapData.getPixel(u, v) );
			}
		}
	}
	
	inline function setPixelMemAt(xVal:Int, yVal:Int, color:UInt):Void {
		Memory.setI32( (( yVal * _breath + xVal )) << 2, color);
	}
	
	inline function getPositionAddressOf(positionPixels:Int):Int {  // to add custom direction
		return mem_offset + ( Std.int(positionPixels * _internalBmpData.width ) << 2);
	}
	
	inline function setInternalBmpData(bmpData:BitmapData):Void {
		_internalBmpData = bmpData;
		bitmapData = _internalBmpData;
	}
	
	inline function validateSize():Void {
			 _internalBmpData.dispose();
			setInternalBmpData(new BitmapData(Std.int(_internalSampleArea.width), Std.int(_internalSampleArea.height), false, 0x000000));
			_invalidSize = false;
	}
	
	public function render():Void {
		 if (_invalidSize) {
			validateSize();
		}
		_internalBmpData.lock();
		ApplicationDomain.currentDomain.domainMemory.position =getPositionAddressOf(scrollX);
		_internalBmpData.setPixels(_internalSampleArea, ApplicationDomain.currentDomain.domainMemory);
		_internalBmpData.unlock();
	}
	
	
	
}