module Arkham.Act.Cards.Awakening (awakening) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Scenario (getIsReturnTo)
import Arkham.Matcher
import Arkham.Name

newtype Awakening = Awakening ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

awakening :: ActCard Awakening
awakening = act (1, A) Awakening Cards.awakening (groupClueCost (PerPlayer 3))

instance RunMessage Awakening where
  runMessage msg a@(Awakening attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      location <- selectRandomJust "must be at least one location" $ SetAsideCardMatch LocationCard
      otherLocationCount <- selectCount $ LocationWithUnrevealedTitle $ nameTitle $ toName location
      let label = nameToLabel (toName location) <> tshow (otherLocationCount + 1)

      locationId <- placeLocation location
      push $ SetLocationLabel locationId label

      createEnemyAt_ Cards.theManInThePallidMask locationId

      isReturnTo <- getIsReturnTo
      -- Advance to one of the 3 copies of act 2a, at random
      nextAct <-
        sample
          $ Cards.theStrangerACityAflame
          :| ( [Cards.theStrangerThePathIsMine, Cards.theStrangerTheShoresOfHali]
                 <> ( guard isReturnTo
                        *> [Cards.theStrangerAlaranMists, Cards.theStrangerUnderTheCity, Cards.theStrangerHereIsMyReply]
                    )
             )
      advanceToAct attrs nextAct A
      pure a
    _ -> Awakening <$> liftRunMessage msg attrs
