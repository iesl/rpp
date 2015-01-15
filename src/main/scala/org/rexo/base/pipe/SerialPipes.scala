package org.rexo.base.pipe

import scala.collection.mutable
import org.rexo.base.{Instance, Pipe}
//import edu.umass.cs.mallet.base.types.Instance
//import edu.umass.cs.mallet.base.pipe.Pipe

/**
 * Created by klimzaporojets on 10/2/14.
 */
class SerialPipes(var pipes:mutable.MutableList[Pipe]) extends Pipe{

    def pipe(carrier: Instance): Instance =
    {
      return pipe(carrier, 0);
    }

  def pipe(carrier: Instance, startingIndex: Int): Instance = {
    var carrierVar = carrier;
    {
      var i: Int = startingIndex
      while (i < pipes.size) {
        {
          val p: Pipe = pipes.get(i).get.asInstanceOf[Pipe]
          if (p == null) {
            System.err.println("Pipe is null")
          }
          else {
            try {
              carrierVar = p.pipe(carrier)
            }
            catch {
              case e: Exception => {
                System.err.println("Exception on pipe " + i + ". " + e)
                e.printStackTrace(System.err)
                 throw new RuntimeException(e)
              }
            }
          }
        }
        ({
          i += 1; i - 1
        })
      }
    }
    return carrier
  }
}
