package collections

import sun.security.ec.point.ProjectivePoint.Mutable

import scala.collection.immutable.Iterable.from
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer

object task_collections extends App {

  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," , "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," , "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * **/
  def capitalizeIgnoringASCII(text: List[String]): List[String] = {
    def mapFunc(word: String): String = {
      if (isASCIIString(word)) word.toUpperCase
      else word.toLowerCase
    }

    text match {
      case Nil => Nil
      case _ :: Nil => text
      case head :: tail => head :: tail.map(mapFunc)
    }
  }

  def capitalizeIgnoringASCII_2(text: List[String]): List[String] = {
    text.zipWithIndex.collect {
      case (st, 0) => st
      case (st, _) => if (isASCIIString(st)) st.toUpperCase() else st.toLowerCase()
    }
  }

  val testCase1 = List(
    List(),
    List("Ones"),
    List("first", "second"),
    List("Lorem", "ipsum", "dolor", "sit", "amet"),
    List("Оказывается", ",", "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")
  )
  testCase1.foreach(w=>println(capitalizeIgnoringASCII(w)))
  testCase1.foreach(w=>println(capitalizeIgnoringASCII_2(w)))


  /**
   *
   * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
   * В тексте встречаются числа от 0 до 9
   *
   * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * **/
  def numbersToNumericString(text: String): String = {
    val cardinalMap = Map(0 -> "zero", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine")
    text.flatMap(
      c => if (c.isDigit) cardinalMap.getOrElse(c.asDigit, "?") else c.toString
    )
  }

  println( numbersToNumericString("There are 2 caps and 3 books"))

  /**
   *
   * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии что записи уникальные и не имеют повторений
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * **/

  case class Auto(mark: String, model: String)

  /**
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
   * Реализуйте метод который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
   **/
  def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    immutable.Set.from(dealerOne ++ dealerTwo).toList
    // like:
    //mutable.Set.from(dealerOne).addAll(dealerTwo)
    // ListBuffer.from(dealerOne).addAll(dealerTwo).distinct
  }

  val dealer1 = List(Auto("ZAZ", "like camel"), Auto("volga", "21"), Auto("BMW", "X5"), Auto("BMW", "X1"))
  val dealer2 = List(Auto("Moskvich", "407"), Auto("volga", "21"), Auto("BMW", "X5"))

  println(intersectionAuto(dealer1,dealer2))

  /**
   * Хотим узнать какие машины обслуживается в первом дилеромском центре, но не обслуживаются во втором
   * Реализуйте метод который примет две коллекции (два источника)
   * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
   **/
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    val one = Set.from(dealerOne)
    val two = Set.from(dealerTwo)
    one.diff(two)
  }

  println(filterAllLeftDealerAutoWithoutRight(dealer1,dealer2))
}
