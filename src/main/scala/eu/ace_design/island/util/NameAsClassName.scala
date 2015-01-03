package eu.ace_design.island.util

/**
 * This file is part of the island project
 * @author mosser (03/01/2015, 12:52)
 **/
trait NameAsClassName {

  // the name of the target class, used to generate useful message. Object ending $ (scala convention) are removed.
  val name = {
    val n = this.getClass.getSimpleName
    if ( n endsWith "$") n.substring(0,n.size-1) else n
  }

}
