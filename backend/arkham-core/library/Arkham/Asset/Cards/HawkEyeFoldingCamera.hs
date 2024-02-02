module Arkham.Asset.Cards.HawkEyeFoldingCamera (
  hawkEyeFoldingCamera,
  HawkEyeFoldingCamera (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Metadata = Metadata {locations :: [LocationId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype HawkEyeFoldingCamera = HawkEyeFoldingCamera (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

hawkEyeFoldingCamera :: AssetCard HawkEyeFoldingCamera
hawkEyeFoldingCamera =
  asset (HawkEyeFoldingCamera . (`with` Metadata [])) Cards.hawkEyeFoldingCamera

instance HasModifiersFor HawkEyeFoldingCamera where
  getModifiersFor (InvestigatorTarget iid) (HawkEyeFoldingCamera (a `With` _))
    | controlledBy a iid = do
        pure
          $ toModifiers a
          $ [SkillModifier SkillWillpower 1 | assetResources a >= 1]
          <> [SkillModifier SkillIntellect 1 | assetResources a >= 2]
          <> [SanityModifier 1 | assetResources a >= 3]
  getModifiersFor _ _ = pure []

instance HasAbilities HawkEyeFoldingCamera where
  getAbilities (HawkEyeFoldingCamera (a `With` meta)) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (DiscoveringLastClue Timing.After You locationMatcher)
          Free
    ]
   where
    locationMatcher =
      if null (locations meta)
        then Anywhere
        else NotLocation (LocationMatchAny $ map LocationWithId $ locations meta)

toLocation :: [Window] -> LocationId
toLocation [] = error "invalid call"
toLocation ((windowType -> Window.DiscoveringLastClue _ lid) : _) = lid
toLocation (_ : xs) = toLocation xs

instance RunMessage HawkEyeFoldingCamera where
  runMessage msg (HawkEyeFoldingCamera (attrs `With` meta)) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (toLocation -> lid) _ -> do
      push $ PlaceResources (toAbilitySource attrs 1) (toTarget attrs) 1
      pure $ HawkEyeFoldingCamera (attrs `with` Metadata (lid : locations meta))
    _ -> HawkEyeFoldingCamera . (`with` meta) <$> runMessage msg attrs
