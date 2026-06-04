module Arkham.Asset.Assets.BertieMusgraveATrueAesthete (bertieMusgraveATrueAesthete) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype BertieMusgraveATrueAesthete = BertieMusgraveATrueAesthete AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bertieMusgraveATrueAesthete :: AssetCard BertieMusgraveATrueAesthete
bertieMusgraveATrueAesthete =
  allyWith BertieMusgraveATrueAesthete Cards.bertieMusgraveATrueAesthete (2, 1) noSlots

instance HasModifiersFor BertieMusgraveATrueAesthete where
  getModifiersFor (BertieMusgraveATrueAesthete a) = controllerGets a [HealthModifier 1, SanityModifier 1]

instance HasAbilities BertieMusgraveATrueAesthete where
  getAbilities (BertieMusgraveATrueAesthete a) =
    [ restricted a 1 OnSameLocation
        $ FastAbility'
          (OrCost [DamageCost (a.ability 1) YouTarget 1, HorrorCost (a.ability 1) YouTarget 1] <> exhaust a)
          #parley
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage BertieMusgraveATrueAesthete where
  runMessage msg a@(BertieMusgraveATrueAesthete attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      codex iid (attrs.ability 1) Omega
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      removeFromGame attrs
      push $ SetAsideCards [toCard attrs]
      pure a
    _ -> BertieMusgraveATrueAesthete <$> liftRunMessage msg attrs
