About the BitmapScrollerHaxe.hx version
-----------------------------------------------
com.flashartofwar.BitmapScrollerHaxe
Note: This hx file is just a sample fork modification an old version of BitmapDataCollectionSampler and is in no way affiliated to Jesse Freemen/Flash Art Of War Blog. The location of the Haxe file was placed at the same package location for convenience. 

Getting Started
------------------
The haxelib.hxml will compile a matching SWC for you, if you have Haxe installed, which will contain the compiled code from the hx file.

Compile BitmapScrollerAppHaxe.as in Flash Player 10 with the compiled haxelib.swc library. The build.xml and build.properties has been adjusted to be able to compile the Haxe version. The CONFIG::mobile flag seems to be handled by the ANT build, so there's no need to replace it with a variable.

About this Fork
--------------
This fork was basically a Haxe-i-fied version of an older (to be depreciated?) BitmapDataCollectionSampler class. As before, this also involves scrolling a bunch of bitmap-datas stored in Alchemy memory and is a similar implementation to that.

The result is a consistent framerate throughout, regardless of how far in/out you're scrolling within the list, the search time is always at "O(1)" constant time. This is achieved by simply moving the ByteArray memory position pointer to match the scrolling value, and calling bitmapData.setPixels(rect, byteArray) to render the bytes. I get 61 fps consistently for smaller screen sizes. For larger screen sizes, I can get 30-55 fps.

In the standard AS3 copyPixels() version, codes had to do a linear "O(n)" search to find the target location from the starting location. So, the further you go down the list, the more unpredicatable the search time would get. Iterating from the last known position index won't work either if in the worse case scenerio, the distance travelled per tick is of extremely large skips, so it'd be better of starting from the first index. For large lists, this could mean iterating through many indexes to find the current index. However, the number of indexes and recursive calls to draw each index is minimised in the standard example, so there's not much of a performance hit.

The Haxe implementation has no limit of the number of n-entries as far as performance is concerned. However, memory usage/storage can be extremely high and time-consuming at the beginning, including storing all pixel data found in the images. However, once everything is cached, you'll get a consistent framerate throughout.

The ideal case for using the Haxe/Alchemy implementation is when your server or developer has already pre-processed the images and fitted them into the actual viewing size during scrolling. Also, for images to be scrolled horizontally, the images had to be read in a translated (rotate 90/270 degrees) fashion into memory, and the bitmap & bitmapData itself had to be rotated 90/270 degrees and the bitmap re-positioned/flipped. If the images loaded in were already rotated, these translations could be avoided on the Flash-side, improving startup time. Also, it's better to NOT cache all images into memory at once, but progressively load/cache images in at intervals.


General observations between the 2 versions
-------------------------------------------
For both versions, i cranked the target framerate to 120fps to see how high one can get in fps. For drastically reduced screen sizes, both versions go beyond the ideal framerate of 61fps.

For the standard AS3 CopyPixels version, after scrolling through all bitmapdata instances (ie. scrolling through the entire length of the timeline), the scrolling can actually turn out faster than the Haxe/Alchemy's setPixels() version (meeting the ideal framerate of 61fps compared to the Haxe/Alchemy version which lags behind).

It seems the Flash player has a way of caching previous memory accesses to bitmapData such that once such a bitmapData has been visited before, subsequent copyPixels() calls to that bitmapData would take less far time to execute. In fact, copyPixels() is generally a faster routine compared to setPixels(), and there are times the standard AS3 version can peak at higher FPS compared to the Haxe/Alchemy version. 

The only reason why the Haxe/Alchemy version can appear "faster" is because it avoids the O(n) search, making framerate consistent across the board. If such a search was negated in the regular AS3 version (among other inlined optimisations), the AS3 version could very well be much faster than the Haxe/Alchemy implementation overall. In fact, the Haxe/Alchemy version still has an inherant O(n) operation through setPixels() which makes it unfriendly for blitting to large screens. 

For the Haxe version, setPixels() does take a toll due to the very action of having to write every pixel. In that sense, the Haxe alchemy memory implementation isn't necessarily better, and in fact the AS3 copyPixels() version performs and scales better on larger screens. 