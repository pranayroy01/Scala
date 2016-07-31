def product(f:Int => Int)(a:Int, b:Int): Int =
  if(a>b) 1
  else f(a) +product(f)(a+1,b)


def fact(n: Int) = product(x=> x * x)(1,n)



object rationals {
  val x = new Rational(1, 2)
  val y = new Rational(2, 3)

  print("d"+x.add(y))

}

class Rational(x:Int, y:Int) {
  require(y>0,"error")
  def number = x

  def denom = y

  def this(x: Int) =this(x ,1)

  private def gcd(a:Int, b:Int): Int ={
    if (b==0) a  else gcd(b,a%b)
  }


  def add(that: Rational) =
    new Rational(number * that.denom + that.number + denom,
      denom * that.denom)



}


rationals