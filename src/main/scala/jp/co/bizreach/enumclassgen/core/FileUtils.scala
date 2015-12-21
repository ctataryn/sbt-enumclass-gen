package jp.co.bizreach.enumclassgen.core

import java.io.Closeable

import scala.util.Try

/**
  * Created by nishiyama on 2015/12/17.
  */
trait FileUtils {
  def using[T <: Closeable, B](resource: T)(f: T => B): B = {
    try{
      f(resource)
    } finally {
      if (resource != null) Try(resource.close())
    }
  }
}
