package gamelib2d;

class Keys
{
  private static var keyStatus: Array<Bool>;
  private static var maxKey: Int = 255;
  
  public static function init ()
  {
    keyStatus = new Array ();
	var i: Int;
    for (i in 0...maxKey)
      keyStatus[i] = false;
  }
  
  public static function setKeyStatus (key: UInt, status: Bool)
  {
    if (key <= maxKey) 
	  keyStatus[key] = status;
  }
  
  public static function keyIsDown (key: UInt): Bool
  {
    if (key <= maxKey)
      return keyStatus[key]
	else
	  return false;
  }

}
