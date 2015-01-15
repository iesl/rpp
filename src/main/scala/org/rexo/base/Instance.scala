package org.rexo.base

/**
 * Created by klimzaporojets on 10/2/14.
 */
class Instance(var data:Any, var target:Any, var name:Any, var source:Any, var pipe:Pipe) {

  if(pipe != null)
  {
    pipe.pipe(this)
  }
  def getData():Any =
  {
    return data;
  }

  def setSource(thisSource:Any):Unit =
  {
    source = thisSource;
  }
  def setData(d:Any) {
    data = d
  }
  def setTarget(t:Any):Unit =
  {
    target = t
  }

}
