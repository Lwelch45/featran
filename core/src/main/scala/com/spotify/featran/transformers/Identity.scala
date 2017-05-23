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

object Identity {
  // Missing value = 0.0
  def apply[A: Numeric](name: String): Transformer[A, Unit, Unit] = new Identity(name)
}

private class Identity[@specialized (Int, Long, Float, Double) A: Numeric](name: String)
  extends MapOne[A](name) {
  private val num = implicitly[Numeric[A]]
  override def map(a: A): Double = num.toDouble(a)
}
