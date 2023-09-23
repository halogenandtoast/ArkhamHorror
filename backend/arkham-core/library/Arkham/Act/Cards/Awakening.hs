module Arkham.Act.Cards.Awakening (
  Awakening (..),
  awakening,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message
import Arkham.Name

newtype Awakening = Awakening ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

awakening :: ActCard Awakening
awakening =
  act
    (1, A)
    Awakening
    Cards.awakening
    (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage Awakening where
  runMessage msg a@(Awakening attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      location <- selectRandomJust "must be at least one location" $ SetAsideCardMatch LocationCard
      otherLocationCount <- selectCount $ LocationWithUnrevealedTitle $ nameTitle $ toName location
      let label = nameToLabel (toName location) <> tshow (otherLocationCount + 1)

      (locationId, locationPlacement) <- placeLocation location

      -- spawn the set-aside The Man in the Pallid Mask enemy at that location
      theManInThePallidMask <- getSetAsideCard Cards.theManInThePallidMask

      -- Advance to one of the 3 copies of act 2a, at random
      nextAct <-
        sample
          $ Cards.theStrangerACityAflame
          :| [Cards.theStrangerThePathIsMine, Cards.theStrangerTheShoresOfHali]

      createTheManInThePallidMask <-
        createEnemyAt_ theManInThePallidMask locationId Nothing

      pushAll
        [ locationPlacement
        , SetLocationLabel locationId label
        , createTheManInThePallidMask
        , AdvanceToAct (actDeckId attrs) nextAct A (toSource attrs)
        ]
      pure a
    _ -> Awakening <$> runMessage msg attrs
