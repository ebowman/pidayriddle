import scala.util.Try

object Main extends App {

  def colorRules(colors: Seq[String]) = Try(colors(colors.indexOf("White") - 1) == "Green").getOrElse(false)

  def natRules(colors: Seq[String], nats: Seq[String]) =
    nats.head == "Norwegian" && colors(nats.indexOf("Brit")) == "Red" &&
      (Try(colors(nats.indexOf("Norwegian") - 1) == "Blue").getOrElse(false) ||
        Try(colors(nats.indexOf("Norwegian") + 1) == "Blue").getOrElse(false))

  def drinkRules(colors: Seq[String], nats: Seq[String], drinks: Seq[String]) =
    drinks(nats.indexOf("Dane")) == "Tea" &&
      drinks(colors.indexOf("Green")) == "Coffee" &&
      drinks(2) == "Milk"

  def petRules(nats: Seq[String], pets: Seq[String]) = pets(nats.indexOf("Swede")) == "Dogs"

  def smokeRules(colors: Seq[String], nats: Seq[String], drinks: Seq[String], pets: Seq[String], smokes: Seq[String]) =
    pets(smokes.indexOf("Pall Mall")) == "Birds" &&
      smokes(colors.indexOf("Yellow")) == "Dunhill" &&
      (Try(pets(smokes.indexOf("Blend") - 1) == "Cats").getOrElse(false) ||
        Try(pets(smokes.indexOf("Blend") + 1) == "Cats").getOrElse(false)) &&
      (Try(smokes(pets.indexOf("Horses") - 1) == "Dunhill").getOrElse(false) ||
        Try(pets(pets.indexOf("Horses") + 1) == "Dunhill").getOrElse(false)) &&
      drinks(smokes.indexOf("BlueMaster")) == "Beer" &&
      smokes(nats.indexOf("German")) == "Prince" &&
      (Try(drinks(smokes.indexOf("Blend") - 1) == "Water").getOrElse(false) ||
        Try(drinks(smokes.indexOf("Blend") + 1) == "Water").getOrElse(false))

  val start = System.currentTimeMillis()
  val models = for {
    colors <- Seq("Red", "Blue", "White", "Green", "Yellow").permutations if colorRules(colors)
    nats <- Seq("Brit", "Swede", "Dane", "Norwegian", "German").permutations if natRules(colors, nats)
    drinks <- Seq("Tea", "Coffee", "Milk", "Beer", "Water").permutations if drinkRules(colors, nats, drinks)
    pets <- Seq("Dogs", "Birds", "Cats", "Horses", "Fish").permutations if petRules(nats, pets)
    smokes <- Seq("BlueMaster", "Blend", "Pall Mall", "Dunhill", "Prince").permutations if smokeRules(colors, nats, drinks, pets, smokes)
  } yield (colors, nats, drinks, pets, smokes)

  val solution = models.next
  val toPrint = solution._2(solution._4.indexOf("Fish"))
  val stop = System.currentTimeMillis()
  println(solution)
  println(toPrint)
  println(s"${stop - start} ms")
}
