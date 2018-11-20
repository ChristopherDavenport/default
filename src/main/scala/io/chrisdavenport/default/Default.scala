package io.chrisdavenport.default

import cats._
import cats.implicits._

trait Default[A]{
  def default: A
}
object Default{
  def apply[A](implicit ev: Default[A]): Default[A] = ev
  def of[A](a: A): Default[A] = new Default[A]{
    def default: A = a
  }
  def empty[A: Monoid]: Default[A] = of(Monoid[A].empty)
  
  private val zero = 0
  implicit val intDefault: Default[Int] = of(zero.toInt)
  implicit val javaIntDefault: Default[java.lang.Integer] = of(zero)
  implicit val byteDefault: Default[Byte] = of(zero.toByte)
  implicit val javaByteDefault: Default[java.lang.Byte] = of(zero.toByte)
  implicit val floatDefault: Default[Float] = of(zero.toFloat)
  implicit val javaFloatDefault : Default[java.lang.Float] = of(zero)
  implicit val shortDefault: Default[Short] = of(zero.toShort)
  implicit val javaShortDefault: Default[java.lang.Short] = of(zero.toShort)
  implicit val longDefault : Default[Long] = of(zero.toLong)
  implicit val javaLongDefault : Default[java.lang.Long] = of(zero.toLong)
  implicit val bigIntDefault: Default[BigInt] = of(BigInt(zero))
  implicit val javaBigIntegerDefault: Default[java.math.BigInteger] = of(BigInt(zero).bigInteger)
  implicit val doubleDefault: Default[Double] = of(zero.toDouble)
  implicit val javaDoubleDefault : Default[java.lang.Double] = of(zero.toDouble)

  implicit def optionDefault[A]: Default[Option[A]] = of(Option.empty[A])

  implicit val unitDefault: Default[Unit] = empty
  implicit val stringDefault: Default[String] = empty
  implicit def listDefault[A]: Default[List[A]] = empty

  // Tuple Defaults -- TODO: Macro for arity
  implicit def tuple2Default[A: Default, B: Default]: Default[(A, B)]= 
    of((default[A], default[B]))
  implicit def tuple3Default[A: Default, B: Default, C: Default]: Default[(A, B, C)] = 
    of((default[A], default[B], default[C]))
  implicit def tuple4Default[A: Default, B: Default, C: Default, D: Default]: Default[(A, B, C, D)] = 
    of((default[A], default[B], default[C], default[D]))
  implicit def tuple5Default[A: Default, B: Default, C: Default, D: Default, E: Default
    ]: Default[(A, B, C, D, E)] = 
    of((default[A], default[B], default[C], default[D], default[E]))
  implicit def tuple6Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default
    ]: Default[(A, B, C, D, E, F)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F]))
  implicit def tuple7Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default
    ]: Default[(A, B, C, D, E, F, G)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G]))
  implicit def tuple8Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default
  ]: Default[(A, B, C, D, E, F, G, H)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H]
    ))
  implicit def tuple9Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default
  ]: Default[(A, B, C, D, E, F, G, H, I)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I]
    ))
  implicit def tuple10Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J]
    ))
  implicit def tuple11Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K]
    ))
  implicit def tuple12Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L]
    ))
  implicit def tuple13Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M]
    ))
  implicit def tuple14Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default, N: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M], default[N]
    ))
  implicit def tuple15Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default, N: Default, O: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M], default[N], default[O]
    ))
  implicit def tuple16Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default, N: Default, O: Default,
    P: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M], default[N], default[O],
    default[P]
    ))
  implicit def tuple17Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default, N: Default, O: Default,
    P: Default, Q: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M], default[N], default[O],
    default[P], default[Q]
    ))
  implicit def tuple18Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default, N: Default, O: Default,
    P: Default, Q: Default, R: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M], default[N], default[O],
    default[P], default[Q], default[R]
    ))
  implicit def tuple19Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default, N: Default, O: Default,
    P: Default, Q: Default, R: Default, S: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M], default[N], default[O],
    default[P], default[Q], default[R], default[S]
    ))
  implicit def tuple20Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default, N: Default, O: Default,
    P: Default, Q: Default, R: Default, S: Default, T: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M], default[N], default[O],
    default[P], default[Q], default[R], default[S], default[T]
    ))
  implicit def tuple21Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default, N: Default, O: Default,
    P: Default, Q: Default, R: Default, S: Default, T: Default, U: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M], default[N], default[O],
    default[P], default[Q], default[R], default[S], default[T], default[U]
    ))
  implicit def tuple22Default[A: Default, B: Default, C: Default, D: Default, E: Default, F: Default, G: Default,
    H: Default, I: Default, J: Default, K: Default, L: Default, M: Default, N: Default, O: Default,
    P: Default, Q: Default, R: Default, S: Default, T: Default, U: Default, V: Default
  ]: Default[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = 
    of((default[A], default[B], default[C], default[D], default[E], default[F], default[G], 
    default[H], default[I], default[J], default[K], default[L], default[M], default[N], default[O],
    default[P], default[Q], default[R], default[S], default[T], default[U], default[V]
    ))


}