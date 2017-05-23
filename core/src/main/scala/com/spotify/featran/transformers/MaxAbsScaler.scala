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

import com.spotify.featran.FeatureBuilder
import com.twitter.algebird.{Aggregator, Max}

object MaxAbsScaler {
  // Missing value = 0.0
  def apply[A: Numeric](name: String): Transformer[A, Max[Double], Double] = new MaxAbsScaler(name)
}

private class MaxAbsScaler[@specialized (Int, Long, Float, Double) A: Numeric](name: String)
  extends OneDimensional[A, Max[Double], Double](name) {
  private val num = implicitly[Numeric[A]]
  override val aggregator: Aggregator[A, Max[Double], Double] =
    Aggregators.from[A](x => Max(math.abs(num.toDouble(x)))).to(_.get)
  override def buildFeatures(a: Option[A], c: Double, fb: FeatureBuilder[_]): Unit = a match {
    case Some(x) => fb.add(num.toDouble(x) / c)
    case None => fb.skip()
  }
}
