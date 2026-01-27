module Arkham.Asset.Assets.CovertOpsInTheShadows (covertOpsInTheShadows) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyEvaded)
import Arkham.Capability
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype CovertOpsInTheShadows = CovertOpsInTheShadows AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

covertOpsInTheShadows :: AssetCard CovertOpsInTheShadows
covertOpsInTheShadows = asset CovertOpsInTheShadows Cards.covertOpsInTheShadows

instance HasAbilities CovertOpsInTheShadows where
  getAbilities (CovertOpsInTheShadows a) =
    [ controlled
        a
        1
        (youExist $ oneOf [can.draw.cards, InvestigatorCanMoveTo (toSource a) AccessibleLocation])
        $ triggered (EnemyEvaded #after You AnyEnemy) (exhaust a)
    ]

instance RunMessage CovertOpsInTheShadows where
  runMessage msg a@(CovertOpsInTheShadows attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawOk <- can.draw.cards iid
      locations <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseOneM iid do
        when drawOk $ labeled "Draw 1 card" $ drawCards iid attrs 1
        targets locations $ moveTo (attrs.ability 1) iid
      pure a
    _ -> CovertOpsInTheShadows <$> liftRunMessage msg attrs
