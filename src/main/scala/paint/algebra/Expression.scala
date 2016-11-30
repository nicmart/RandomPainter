package paint.algebra

import scala.language.higherKinds

/**
  * Created by nic on 30/11/2016.
  */

trait Expression[F[_]] {
    def apply[T](algebra: F[T]): T
}

trait ExpressionTransformation[F[_], G[_]] {
    def apply(expr: Expression[F]): Expression[G]
}

case class AlgebraExpressionTransformation[F[_], G[_]](algebra: F[Expression[G]])
    extends ExpressionTransformation[F, G]
{
    def apply(expr: Expression[F]): Expression[G] = {
        expr.apply(algebra)
    }
}