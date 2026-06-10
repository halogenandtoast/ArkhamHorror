module Arkham.Location.Cards.DesertOasis (desertOasis) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DesertOasis = DesertOasis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desertOasis :: LocationCard DesertOasis
desertOasis = symbolLabel $ location DesertOasis Cards.desertOasis 5 (PerPlayer 1)

instance HasAbilities DesertOasis where
  getAbilities (DesertOasis a) =
    extendRevealed
      a
      [ playerLimit PerGame
          $ restricted a 1 (Here <> oneOf [can.heal.damage (a.ability 1) You, can.heal.horror (a.ability 1) You]) actionAbility
      , groupLimit PerGame
          $ restricted a 2 (Here <> youExist (InvestigatorWithHorror $ EqualTo $ Static 0))
          doubleActionAbility
      ]

instance RunMessage DesertOasis where
  runMessage msg l@(DesertOasis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamage iid (attrs.ability 1) 1
      healHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 3
      removeStrengthOfTheAbyss 1
      pure l
    _ -> DesertOasis <$> liftRunMessage msg attrs
