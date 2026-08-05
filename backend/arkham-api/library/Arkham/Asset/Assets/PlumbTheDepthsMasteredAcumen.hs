module Arkham.Asset.Assets.PlumbTheDepthsMasteredAcumen (plumbTheDepthsCompleted) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher

newtype PlumbTheDepthsMasteredAcumen = PlumbTheDepthsMasteredAcumen AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plumbTheDepthsCompleted :: AssetCard PlumbTheDepthsMasteredAcumen
plumbTheDepthsCompleted = asset PlumbTheDepthsMasteredAcumen Cards.plumbTheDepthsCompleted

instance HasModifiersFor PlumbTheDepthsMasteredAcumen where
  getModifiersFor (PlumbTheDepthsMasteredAcumen a) =
    for_ a.controller \iid -> modified_ a iid [SkillModifier #intellect 1]

instance HasAbilities PlumbTheDepthsMasteredAcumen where
  getAbilities (PlumbTheDepthsMasteredAcumen a) =
    [ controlled a 1 NoRestriction
        $ triggered (DiscoveringLastClue #after You YourLocation) (exhaust a)
    ]

instance RunMessage PlumbTheDepthsMasteredAcumen where
  runMessage msg a@(PlumbTheDepthsMasteredAcumen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> PlumbTheDepthsMasteredAcumen <$> liftRunMessage msg attrs
