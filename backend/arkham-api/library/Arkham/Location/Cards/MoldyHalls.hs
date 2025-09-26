module Arkham.Location.Cards.MoldyHalls (moldyHalls) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MoldyHalls = MoldyHalls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moldyHalls :: LocationCard MoldyHalls
moldyHalls = location MoldyHalls Cards.moldyHalls 4 (PerPlayer 1)

instance HasAbilities MoldyHalls where
  getAbilities (MoldyHalls a) =
    extendRevealed1 a
      $ withI18n
      $ countVar 3
      $ hauntedI "loseResources" a 1
      `withCriteria` InvestigatorExists (You <> InvestigatorWithAnyResources)

instance RunMessage MoldyHalls where
  runMessage msg l@(MoldyHalls attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseResources iid (attrs.ability 1) 3
      pure l
    _ -> MoldyHalls <$> liftRunMessage msg attrs
