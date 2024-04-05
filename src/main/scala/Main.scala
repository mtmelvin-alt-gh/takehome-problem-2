object Main extends App {
  val allPromotions: Seq[Promotion] = Seq(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2"))
  )

  private val finder = PromotionComboFinder()
  private val allCombinableResult = finder.allCombinablePromotions(allPromotions)
  println(allCombinableResult.toString())
  println()


  private val p1Result = finder.combinablePromotions("P1", allPromotions)
  println(p1Result.toString())
  println()

  private val p3Result = finder.combinablePromotions("P3", allPromotions)
  println(p3Result.toString())
  println()
}
