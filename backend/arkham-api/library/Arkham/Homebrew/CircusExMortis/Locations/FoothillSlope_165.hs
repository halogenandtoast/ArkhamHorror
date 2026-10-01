module Arkham.Homebrew.CircusExMortis.Locations.FoothillSlope_165 (foothillSlope_165) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FoothillSlope_165 = FoothillSlope_165 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foothillSlope_165 :: LocationCard FoothillSlope_165
foothillSlope_165 = location FoothillSlope_165 Cards.foothillSlope_165 4 (PerPlayer 1)

instance HasAbilities FoothillSlope_165 where
  getAbilities (FoothillSlope_165 a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> canDiscoverCluesAt (be a))
      $ freeReaction
      $ IfEnemyDefeated #after You ByAny (enemyWasAt a)

instance RunMessage FoothillSlope_165 where
  runMessage msg l@(FoothillSlope_165 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure l
    _ -> FoothillSlope_165 <$> liftRunMessage msg attrs
