module Arkham.Homebrew.AgesUnwound.Locations.RearCorridors (rearCorridors) where

import Arkham.Cost
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RearCorridors = RearCorridors LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | Unrevealed: "As an additional cost to enter Rear Corridors, investigators at
Side Building must spend 3[per_investigator] clues as a group."

By title, not by def: Scenario VI has its own Side Building (@:ages-unwound:170@).
-}
rearCorridors :: LocationCard RearCorridors
rearCorridors =
  symbolLabel $ location RearCorridors Cards.rearCorridors 4 (PerPlayer 1)
    & setCostToEnterUnrevealed (GroupClueCost (PerPlayer 3) "Side Building")

-- | "Enemies in the Rear Corridors gain massive and lose aloof."
instance HasModifiersFor RearCorridors where
  getModifiersFor (RearCorridors a) =
    modifySelectWhen
      a
      a.revealed
      (enemyAt a.id)
      [AddKeyword Keyword.Massive, RemoveKeyword Keyword.Aloof]

instance RunMessage RearCorridors where
  runMessage msg (RearCorridors attrs) = runQueueT $ RearCorridors <$> liftRunMessage msg attrs
