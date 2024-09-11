module Arkham.Location.Cards.ArkhamWoodsQuietGlade (ArkhamWoodsQuietGlade (..), arkhamWoodsQuietGlade) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards (arkhamWoodsQuietGlade)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ArkhamWoodsQuietGlade = ArkhamWoodsQuietGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsQuietGlade :: LocationCard ArkhamWoodsQuietGlade
arkhamWoodsQuietGlade = location ArkhamWoodsQuietGlade Cards.arkhamWoodsQuietGlade 1 (Static 0)

instance HasAbilities ArkhamWoodsQuietGlade where
  getAbilities (ArkhamWoodsQuietGlade attrs) =
    extendRevealed
      attrs
      [ playerLimit PerTurn
          $ withCriteria
            (mkAbility attrs 1 actionAbility)
            (Here <> any_ [HealableInvestigator (toSource attrs) kind You | kind <- [#horror, #damage]])
      ]

instance RunMessage ArkhamWoodsQuietGlade where
  runMessage msg l@(ArkhamWoodsQuietGlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseOrRunOneM iid do
        whenM (canHaveDamageHealed source iid) do
          damageLabeled iid do
            healDamage iid source 1
        whenM (canHaveHorrorHealed source iid) do
          horrorLabeled iid do
            healHorror iid source 1
      pure l
    _ -> ArkhamWoodsQuietGlade <$> liftRunMessage msg attrs
