module Arkham.Homebrew.AgesUnwound.Locations.OrnateFountain (ornateFountain) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OrnateFountain = OrnateFountain LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ornateFountain :: LocationCard OrnateFountain
ornateFountain = symbolLabel $ location OrnateFountain Cards.ornateFountain 3 (PerPlayer 1)

-- | "[action]: Heal 1 damage and 1 horror. (Group limit once per round.)"
instance HasAbilities OrnateFountain where
  getAbilities (OrnateFountain a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted
        a
        1
        (Here <> any_ [HealableInvestigator (toSource a) kind You | kind <- [#damage, #horror]])
        actionAbility

instance RunMessage OrnateFountain where
  runMessage msg l@(OrnateFountain attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamage iid (attrs.ability 1) 1
      healHorror iid (attrs.ability 1) 1
      pure l
    _ -> OrnateFountain <$> liftRunMessage msg attrs
