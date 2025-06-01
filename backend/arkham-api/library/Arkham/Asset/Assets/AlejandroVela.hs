module Arkham.Asset.Assets.AlejandroVela (alejandroVela) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Trait

newtype AlejandroVela = AlejandroVela AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandroVela :: AssetCard AlejandroVela
alejandroVela = ally AlejandroVela Cards.alejandroVela (2, 2)

instance HasModifiersFor AlejandroVela where
  getModifiersFor (AlejandroVela a) = for_ a.controller \iid ->
    maybeModified_ a iid do
      liftGuardM $ isInvestigationOf $ LocationWithTrait Ancient
      pure [AnySkillValue 1]

instance HasAbilities AlejandroVela where
  getAbilities (AlejandroVela a) =
    [ controlled a 1 (can.draw.cards You <> OnLocation (withTrait Ancient))
        $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage AlejandroVela where
  runMessage msg a@(AlejandroVela attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 2
      pure a
    _ -> AlejandroVela <$> liftRunMessage msg attrs
