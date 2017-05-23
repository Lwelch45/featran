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

import com.spotify.featran._
import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators

object TransformerSpec extends Properties("transformer") {

  implicit def seq[T](implicit arb: Arbitrary[T]): Arbitrary[Seq[T]] =
    Arbitrary(Gen.listOfN(100, arb.arbitrary))

  // Double.NaN != Double.NaN
  // Also map to float to workaround precision error
  private def safeCompare(xs: Seq[Array[Double]], ys: Seq[Seq[Double]]) = {
    def d2e(x: Double): Either[Int, Float] = if (x.isNaN) Left(0) else Right(x.toFloat)
    xs.map(_.toSeq.map(d2e)) == ys.map(_.map(d2e))
  }

  private def test[T](input: Seq[T])
                      (t: Transformer[T, _, _],
                      names: Seq[String],
                      expected: Seq[Seq[Double]],
                      missing: Seq[Double]): Prop = {
    // all values present
    val f1 = FeatureSpec.of[T].required(identity)(t).extract(input)
    // add one missing value
    val f2 = FeatureSpec.of[Option[T]].optional(identity)(t).extract(input.map(Some(_)) :+ None)

    Prop.all(
      "required names" |: f1.featureNames == Seq(names),
      "optional names" |: f2.featureNames == Seq(names),
      "required values" |: safeCompare(f1.featureValues[Array[Double]], expected),
      "optional values" |: safeCompare(f2.featureValues[Array[Double]], expected :+ missing))
  }

  property("binarizer") = Prop.forAll { xs: Seq[Double] =>
    val expected = xs.map(x => Seq(if (x > 0.0) 1.0 else 0.0))
    test(xs)(Binarizer("id"), Seq("id"), expected, Seq(0.0))
  }

  property("binarizer params") = Prop.forAll { (xs: Seq[Double], threshold: Double) =>
    val expected = xs.map(x => Seq(if (x > threshold) 1.0 else 0.0))
    test(xs)(Binarizer("id", threshold), Seq("id"), expected, Seq(0.0))
  }

  property("identity") = Prop.forAll { xs: Seq[Double] =>
    test(xs)(Identity("id"), Seq("id"), xs.map(Seq(_)), Seq(0.0))
  }

  property("max abs") = Prop.forAll { xs: Seq[Double] =>
    val max = xs.map(math.abs).max
    val expected = xs.map(x => Seq(x / max))
    test(xs)(MaxAbsScaler("max_abs"), Seq("max_abs"), expected, Seq(0.0))
  }

  property("min max") = Prop.forAll { xs: Seq[Double] =>
    val (min, max) = (xs.min, xs.max)
    val delta = max - min
    val expected = xs.map(x => Seq((x - min) / delta))
    test(xs)(MinMaxScaler("min_max"), Seq("min_max"), expected, Seq(0.0))
  }

  property("min max params") = Prop.forAll { (xs: Seq[Double], x: Double, y: Double) =>
    val (minP, maxP) = if (x == y) {
      (math.min(x / 2, x), math.max(x / 2, x))
    } else {
      (math.min(x, y), math.max(x, y))
    }
    val (min, max) = (xs.min, xs.max)
    val delta = max - min
    val expected = xs.map(x => Seq((x - min) / delta * (maxP - minP) + minP))
    test(xs)(MinMaxScaler("min_max", minP, maxP), Seq("min_max"), expected, Seq(minP))
  }

  private val nHotGen = Gen.listOfN(100, Gen.listOfN(10, Gen.alphaStr))
  property("n hot") = Prop.forAll(nHotGen) { xs =>
    val cats = xs.flatten.distinct.sorted
    val names = cats.map("n_hot_" + _)
    val expected = xs.map(s => cats.map(c => if (s.contains(c)) 1.0 else 0.0))
    val missing = cats.map(_ => 0.0)
    test(xs)(NHotEncoder("n_hot"), names, expected, missing)
  }

  private val oneHotGen = Gen.listOfN(100, Gen.alphaStr)
  property("one hot") = Prop.forAll(oneHotGen) { xs =>
    val cats = xs.distinct.sorted
    val names = cats.map("one_hot_" + _)
    val expected = xs.map(s => cats.map(c => if (s == c) 1.0 else 0.0))
    val missing = cats.map(_ => 0.0)
    test(xs)(OneHotEncoder("one_hot"), names, expected, missing)
  }

  private val polyGen = Gen.choose(2, 4)
    .flatMap(n => Gen.listOfN(100, Gen.listOfN(n, Arbitrary.arbDouble.arbitrary).map(_.toArray)))
  property("poly") = Prop.forAll(polyGen, Gen.choose(2, 4)) { (xs, degree) =>
    val dim = PolynomialExpansion.expand(xs.head, degree).length
    val names = (0 until dim).map("poly_" + _)
    val expected = xs.map(v => PolynomialExpansion.expand(v, degree).toSeq)
    val missing = (0 until dim).map(_ => 0.0)
    test(xs)(PolynomialExpansion("poly", degree), names, expected, missing)
  }

  def meanAndStddev(xs: Seq[Double]): (Double, Double) = {
    // breeze.stats.stddev is sample stddev
    val mean = breeze.stats.mean(xs)
    (mean, math.sqrt(xs.map(x => math.pow(x - mean, 2)).sum / xs.size))
  }

  property("standard") = Prop.forAll { xs: Seq[Double] =>
    val (mean, stddev) = meanAndStddev(xs)
    val expected = xs.map(x => Seq((x - mean) / stddev + mean))
    test(xs)(StandardScaler("std"), Seq("std"), expected, Seq(mean))
  }

  property("standard true true") = Prop.forAll { xs: Seq[Double] =>
    val (mean, stddev) = meanAndStddev(xs)
    val expected = xs.map(x => Seq((x - mean) / stddev))
    test(xs)(StandardScaler("std", true, true), Seq("std"), expected, Seq(0.0))
  }

  property("standard true false") = Prop.forAll { xs: Seq[Double] =>
    val (mean, stddev) = meanAndStddev(xs)
    val expected = xs.map(x => Seq((x - mean) / stddev + mean))
    test(xs)(StandardScaler("std", true, false), Seq("std"), expected, Seq(mean))
  }

  property("standard false true") = Prop.forAll { xs: Seq[Double] =>
    val (mean, _) = meanAndStddev(xs)
    val expected = xs.map(x => Seq(x - mean))
    test(xs)(StandardScaler("std", false, true), Seq("std"), expected, Seq(0.0))
  }

  property("standard false false") = Prop.forAll { xs: Seq[Double] =>
    val (mean, _) = meanAndStddev(xs)
    val expected = xs.map(Seq(_))
    test(xs)(StandardScaler("std", false, false), Seq("std"), expected, Seq(mean))
  }

}
