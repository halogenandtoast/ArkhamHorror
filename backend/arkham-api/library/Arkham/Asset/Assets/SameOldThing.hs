module Arkham.Asset.Assets.SameOldThing (sameOldThing) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype SameOldThing = SameOldThing AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sameOldThing :: AssetCard SameOldThing
sameOldThing = assetWith SameOldThing Cards.sameOldThing discardWhenNoUses

instance HasModifiersFor SameOldThing where
  getModifiersFor (SameOldThing a) = for_ a.controller \iid ->
    controllerGets
      a
      [CanSpendUsesAsResourceOnCardFromInvestigator a.id Supply (colocatedWith iid) #event]
