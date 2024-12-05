module Arkham.Asset.Assets.HawkEyeFoldingCamera (hawkEyeFoldingCamera, HawkEyeFoldingCamera (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Metadata = Metadata {locations :: [LocationId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HawkEyeFoldingCamera = HawkEyeFoldingCamera (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hawkEyeFoldingCamera :: AssetCard HawkEyeFoldingCamera
hawkEyeFoldingCamera =
  asset (HawkEyeFoldingCamera . (`with` Metadata [])) Cards.hawkEyeFoldingCamera

instance HasModifiersFor HawkEyeFoldingCamera where
  getModifiersFor (HawkEyeFoldingCamera (a `With` _)) = case a.controller of
    Nothing -> pure mempty
    Just iid ->
      modified_ a iid
        $ [SkillModifier #willpower 1 | a.use Evidence >= 1]
        <> [SkillModifier #intellect 1 | a.use Evidence >= 2]
        <> [SanityModifier 1 | a.use Evidence >= 3]

instance HasAbilities HawkEyeFoldingCamera where
  getAbilities (HawkEyeFoldingCamera (a `With` meta)) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction (DiscoveringLastClue #after You locationMatcher)
    ]
   where
    locationMatcher =
      if null (locations meta)
        then YourLocation
        else YourLocation <> NotLocation (LocationMatchAny $ map LocationWithId $ locations meta)

toLocation :: [Window] -> LocationId
toLocation [] = error "invalid call"
toLocation ((windowType -> Window.DiscoveringLastClue _ lid) : _) = lid
toLocation (_ : xs) = toLocation xs

instance RunMessage HawkEyeFoldingCamera where
  runMessage msg (HawkEyeFoldingCamera (attrs `With` meta)) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (toLocation -> lid) _ -> do
      push $ PlaceTokens (toAbilitySource attrs 1) (toTarget attrs) Evidence 1
      pure $ HawkEyeFoldingCamera (attrs `with` Metadata (lid : locations meta))
    _ -> HawkEyeFoldingCamera . (`with` meta) <$> runMessage msg attrs
