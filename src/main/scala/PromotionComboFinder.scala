import scala.collection.mutable.ListBuffer

case class PromotionComboFinder() {
  /**
   * Find all valid combinations of promotions from a given list of promotions
   * @param allPromotions The list of all promotions
   * @return
   */
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    // We're using a Map for this because it's faster and simpler than constantly filtering iterables
    val conflicts: Map[String, Seq[String]] = allPromotions.map(p => (p.code, p.notCombinableWith)).toMap

    val allPromotionCodes = allPromotions.map(p => p.code).toList
    val allowableCombinations = findAllowableCombinations(conflicts, allPromotionCodes)

    // Remove all combos that are a subset of other combos
    // This is needed because findAllowableCombinations returns both greedy and lazy combinations and the
    // examples only show returning greedy ones, and it seemed like a simpler implementation to post-process
    // the results than try to do it during the find operation.

    // All this filtering would be better done using sets but I didn't want to take any risks about whether
    // the output needs to be in the same order as the examples.

    val subsetCombos = ListBuffer[ListBuffer[String]]()

    for (combo1 <- allowableCombinations) {
      for (combo2 <- allowableCombinations if combo1 != combo2 && combo1.forall(combo2.contains)) {
        subsetCombos += combo1
      }
    }

    allowableCombinations.filterInPlace(combo => !subsetCombos.contains(combo))

    val promotionCombos = allowableCombinations.sortWith(_.head < _.head)

    Seq.from(promotionCombos.map(combo => PromotionCombo(combo.sorted.toSeq)))
  }

  /**
   * Finds all promotion combinations for the given promotion code
   * @param promotionCode The promotion code to find combinations for
   * @param allPromotions All promotions from which to select combinations
   * @return
   */
  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    // We're going to reuse the logic in allCombinablePromotions but prefilter the list for anything conflicting
    // to reduce the data set size since allCombinablePromotions has relatively high cyclomatic complexity.

    // Clearly the current implementation is unlikely to scale well to large data sets

    val noConflicts: ListBuffer[Promotion] = new ListBuffer[Promotion]
    for (promotion <- allPromotions if !promotion.notCombinableWith.contains(promotionCode)) {
      noConflicts += promotion
    }

    Seq.from(allCombinablePromotions(Seq.from(noConflicts)))
  }

  /**
   * Used for recursive depth-first search to find allowable combinations of promotions
   *
   * @param conflicts          Map keyed by promotion code containing a Seq[String] of conflicting promotions
   * @param promotionCodes     Promotion codes to search for allowable combinations
   * @param currentCombination The current combination being built via recursive calls to this method
   * @return A ListBuffer of ListBuffers containing all allowable combinations for the parameters passed
   */
  private def findAllowableCombinations(conflicts: Map[String, Seq[String]],
                                        promotionCodes: Seq[String],
                                        currentCombination: Option[ListBuffer[String]] = None): ListBuffer[ListBuffer[String]] = {

    val currentCombo = currentCombination match {
      case Some(value) => value
      case None => new ListBuffer[String]
    }

    val results: ListBuffer[ListBuffer[String]] = new ListBuffer[ListBuffer[String]]()

    if (promotionCodes.isEmpty) {
      // Nothing left to check for combos so add the current combo to the result
      results += currentCombo
    }
    else {
      // Iterate promotionCodes and build / add allowable combinations to results

      promotionCodes.zipWithIndex.foreach {
        case (code, index) =>
          val copyItems: ListBuffer[String] = new ListBuffer[String]()
          copyItems ++= promotionCodes
          copyItems.remove(index)

          if (hasNoConflicts(conflicts, code, currentCombo)) {
            val nextCombo: ListBuffer[String] = new ListBuffer[String]()
            nextCombo ++= currentCombo
            nextCombo += code
            results ++= findAllowableCombinations(conflicts, copyItems.toSeq, Option(nextCombo))
          }

          results ++= findAllowableCombinations(conflicts, copyItems.toSeq, Option(currentCombo))
      }

      results.distinctBy(_.toSet)
    }
  }

  /**
   * Checks whether a given promotion code is combinable with the combination currently being built
   *
   * @param conflicts          Map keyed by promotion code containing a Seq[String] of conflicting promotions
   * @param code               The promotion code to check for conflicts
   * @param currentCombination The current combination being built
   * @return true if code does not conflict with any codes in currentCombination, otherwise false
   */
  private def hasNoConflicts(conflicts: Map[String, Seq[String]], code: String, currentCombination: IterableOnce[String]): Boolean = {
    val notCombinable = conflicts(code)

    currentCombination.iterator.forall(currentCode => {
      !notCombinable.contains(currentCode) && !conflicts(currentCode).contains(code)
    })
  }
}
