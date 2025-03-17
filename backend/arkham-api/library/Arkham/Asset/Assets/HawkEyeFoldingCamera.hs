module Arkham.Asset.Assets.HawkEyeFoldingCamera (hawkEyeFoldingCamera) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (discoveredLocation)
import Arkham.Matcher

newtype Metadata = Metadata {locations :: [LocationId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HawkEyeFoldingCamera = HawkEyeFoldingCamera (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hawkEyeFoldingCamera :: AssetCard HawkEyeFoldingCamera
hawkEyeFoldingCamera = asset (HawkEyeFoldingCamera . (`with` Metadata [])) Cards.hawkEyeFoldingCamera

instance HasModifiersFor HawkEyeFoldingCamera where
  getModifiersFor (HawkEyeFoldingCamera (a `With` _)) = for_ a.controller \iid -> do
    modified_ a iid
      $ [SkillModifier #willpower 1 | a.use Evidence >= 1]
      <> [SkillModifier #intellect 1 | a.use Evidence >= 2]
      <> [SanityModifier 1 | a.use Evidence >= 3]

instance HasAbilities HawkEyeFoldingCamera where
  getAbilities (HawkEyeFoldingCamera (a `With` meta)) =
    [restricted a 1 ControlsThis $ freeReaction (DiscoveringLastClue #after Anyone locationMatcher)]
   where
    locationMatcher =
      if null (locations meta)
        then YourLocation
        else YourLocation <> not_ (mapOneOf LocationWithId $ locations meta)

instance RunMessage HawkEyeFoldingCamera where
  runMessage msg (HawkEyeFoldingCamera (attrs `With` meta)) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (discoveredLocation -> lid) _ -> do
      placeTokens (attrs.ability 1) attrs Evidence 1
      pure $ HawkEyeFoldingCamera (attrs `with` Metadata (lid : locations meta))
    _ -> HawkEyeFoldingCamera . (`with` meta) <$> liftRunMessage msg attrs
