/*
 * Copyright 2017 Spotify AB.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.spotify.featran.numpy

import java.io.{File, FileOutputStream, OutputStream}

import com.google.common.io.{LittleEndianDataOutputStream => LEDOutputStream}

trait NumpyType[@specialized (Int, Long, Float, Double) T] {
  val descr: String
  val sizeOf: Int
  def write(out: LEDOutputStream, value: T)
}

object NumpyType {
  implicit val intNumpyType = new NumpyType[Int] {
    override val descr: String = "<i4"
    override val sizeOf: Int = 4
    override def write(out: LEDOutputStream, value: Int): Unit = out.writeInt(value)
  }

  implicit val longNumpyType = new NumpyType[Long] {
    override val descr: String = "<i8"
    override val sizeOf: Int = 8
    override def write(out: LEDOutputStream, value: Long): Unit = out.writeLong(value)
  }

  implicit val floatNumpyType = new NumpyType[Float] {
    override val descr: String = "<f4"
    override val sizeOf: Int = 4
    override def write(out: LEDOutputStream, value: Float): Unit = out.writeFloat(value)
  }

  implicit val doubleNumpyType = new NumpyType[Double] {
    override val descr: String = "<f8"
    override val sizeOf: Int = 8
    override def write(out: LEDOutputStream, value: Double): Unit = out.writeDouble(value)
  }
}

object NumpyWriter {

  private def header(dimensions: Seq[Int], nt: NumpyType[_]): String = {
    val d = s"(${dimensions.mkString(",")},)"
    val h = s"{'descr': '${nt.descr}', 'fortran_order': False, 'shape': $d, }"
    val l = h.length + 10 + 1 // 0x93NUMPY + 4 + \n
    val n = if (l % 16 == 0) 0 else (l / 16 + 1) * 16 - l
    h + " " * n + "\n"
  }

  private def writeHeader(out: LEDOutputStream, dimensions: Seq[Int], nt: NumpyType[_]): Unit = {
    // magic
    out.writeByte(0x93)
    out.write("NUMPY".getBytes)

    // major, minor
    out.writeByte(1)
    out.writeByte(0)

    // header
    val headerString = header(dimensions, nt)
    out.writeShort(headerString.length)
    out.write(headerString.getBytes)
  }

  private def writeData[T](out: LEDOutputStream, data: Array[T], nt: NumpyType[T]): Unit = {
    var i = 0
    while (i < data.length) {
      nt.write(out, data(i))
      i += 1
    }
  }

  def write[@specialized (Int, Long, Float, Double) T: NumpyType]
  (output: OutputStream, data: Array[T], dimensions: Seq[Int] = Seq.empty): Unit = {
    val nt = implicitly[NumpyType[T]]
    val out = new LEDOutputStream(output)

    val dims = if (dimensions.isEmpty) {
      Seq(data.length)
    } else {
      require(data.length == dimensions.product)
      dimensions
    }
    writeHeader(out, dims, nt)
    writeData(out, data, nt)
    out.close()
  }

  def write[@specialized (Int, Long, Float, Double) T: NumpyType]
  (output: OutputStream, data: Iterator[Array[T]], numRows: Int, numCols: Int): Unit = {
    val nt = implicitly[NumpyType[T]]
    val out = new LEDOutputStream(output)

    val dims = Seq(numRows, numCols)
    writeHeader(out, dims, nt)
    while (data.hasNext) {
      writeData(out, data.next(), nt)
    }
    out.close()
  }

}

object Numpy {

  def write[T: NumpyType](name: String, data: Array[T], dim: Seq[Int] = Seq.empty): Unit =
    NumpyWriter.write(new FileOutputStream(new File(name)), data, dim)

  def main(args: Array[String]): Unit = {
    val data = (1 to 10).toArray
    write("scala-int.npy", data)
    write("scala-long.npy", data.map(_.toLong))
    write("scala-float.npy", data.map(_.toFloat))
    write("scala-double.npy", data.map(_.toDouble))
    write("scala-mat1.npy", (1 to 100).toArray, Seq(20, 5))
    NumpyWriter.write(
      new FileOutputStream(new File("scala-mat2.npy")),
      Iterator.fill(20)(Array(1, 2, 3, 4, 5)), 20, 5)
  }
}
