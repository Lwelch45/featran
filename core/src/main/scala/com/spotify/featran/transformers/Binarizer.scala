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

package com.spotify.featran.transformers

object Binarizer {
  // Missing value = 0.0
  def apply[A: Numeric](name: String, threshold: Double = 0.0): Transformer[A, Unit, Unit] =
    new Binarizer(name, threshold)
}

private class Binarizer[@specialized (Int, Long, Float, Double) A: Numeric]
(name: String, val threshold: Double) extends MapOne[A](name) {
  private val num = implicitly[Numeric[A]]
  override def map(a: A): Double = if (num.toDouble(a) > threshold) 1.0 else 0.0
}
