module Arkham.Asset.Assets.AlejandroVela (alejandroVela, AlejandroVela (..)) where

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
  getModifiersFor (InvestigatorTarget iid) (AlejandroVela a) = maybeModified a do
    guard $ a `controlledBy` iid
    liftGuardM $ isInvestigationOf $ LocationWithTrait Ancient
    pure [AnySkillValue 1]
  getModifiersFor _ _ = pure []

instance HasAbilities AlejandroVela where
  getAbilities (AlejandroVela a) =
    [ controlledAbility a 1 (can.draw.cards You <> OnLocation (LocationWithTrait Ancient))
        $ actionAbilityWithCost (exhaust a)
    ]

instance RunMessage AlejandroVela where
  runMessage msg a@(AlejandroVela attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 2
      pure a
    _ -> AlejandroVela <$> liftRunMessage msg attrs
