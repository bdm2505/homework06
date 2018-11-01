package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[-A] {
  def equiv(o1: A, o2: A): Boolean
}

object Eq {

  implicit val any: Eq[Any] = _ == _

  implicit def seq[T](implicit eq: Eq[T]): Eq[Seq[T]] = (s1, s2) => {
    if (s1.length == s2.length)
      !(s1 zip s2 exists { case (l, r) => !eq.equiv(l, r) })
    else
      false
  }

  implicit def map[K, V](implicit eqKey: Eq[K], eqValue: Eq[V]): Eq[Map[K, V]] = (m1, m2) => {
    if (m1.size == m2.size)
      !(m1 zip m2 exists { case ((k1, v1), (k2, v2)) => !eqKey.equiv(k1, k2) || !eqValue.equiv(v1, v2) })
    else
      false
  }

  implicit def option[V](implicit eq: Eq[V]): Eq[Option[V]] = {
    case (Some(v1: V), Some(v2: V)) => eq.equiv(v1, v2)
    case (None, None) => true
    case _ => false
  }

}

object EqOps {
  implicit class Comparator[T](val l: T) {

    def =~=[U >: T](r: U)(implicit eq: Eq[U]): Boolean =
      eq.equiv(l, r)
  }

  def accuracy[T](acc: T)(implicit num: Numeric[T]): Eq[T] = (e1, e2) => {
    import num._
    abs(e1 - e2) <= acc
  }
}