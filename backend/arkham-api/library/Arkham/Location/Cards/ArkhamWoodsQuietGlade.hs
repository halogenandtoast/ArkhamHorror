module Arkham.Location.Cards.ArkhamWoodsQuietGlade (ArkhamWoodsQuietGlade (..), arkhamWoodsQuietGlade) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Healing
import Arkham.Location.Cards qualified as Cards (arkhamWoodsQuietGlade)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsQuietGlade :: LocationCard ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade = location ArkhamWoodsQuietGlade Cards.arkhamWoodsQuietGlade 1 (Static 0)

instance HasAbilities ArkhamWoodsQuietGlade where
  getAbilities (ArkhamWoodsQuietGlade attrs) =
    extendRevealed1 attrs
      $ playerLimit PerTurn
      $ withCriteria
        (mkAbility attrs 1 actionAbility)
        (Here <> any_ [HealableInvestigator (toSource attrs) kind You | kind <- [#horror, #damage]])

instance RunMessage ArkhamWoodsQuietGlade where
  runMessage msg l@(ArkhamWoodsQuietGlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseHealDamageOrHorror (attrs.ability 1) iid
      pure l
    _ -> ArkhamWoodsQuietGlade <$> liftRunMessage msg attrs
