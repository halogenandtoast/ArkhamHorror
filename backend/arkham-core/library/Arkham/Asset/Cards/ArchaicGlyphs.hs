module Arkham.Asset.Cards.ArchaicGlyphs (
  archaicGlyphs,
  ArchaicGlyphs (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Matcher
import Arkham.Placement
import Arkham.SkillType

newtype Metadata = Metadata {discarding :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype ArchaicGlyphs = ArchaicGlyphs (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

archaicGlyphs :: AssetCard ArchaicGlyphs
archaicGlyphs =
  asset (ArchaicGlyphs . (`with` Metadata False)) Cards.archaicGlyphs

instance HasAbilities ArchaicGlyphs where
  getAbilities (ArchaicGlyphs (attrs `With` meta)) =
    [ restrictedAbility attrs 1 ControlsThis
        $ actionAbilityWithCost
        $ SkillIconCost 1 (singleton $ SkillIcon #intellect)
    , controlledAbility attrs 2 ability2Criteria $ ForcedAbility AnyWindow
    ]
   where
    ability2Criteria =
      if discarding meta
        then Never
        else
          exists
            (AssetWithId (toId attrs) <> AssetWithUseCount Secret (atLeast 3))

instance RunMessage ArchaicGlyphs where
  runMessage msg a@(ArchaicGlyphs (attrs `With` meta)) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AddUses (toId attrs) Secret 1
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      case assetPlacement attrs of
        InPlayArea controllerId -> do
          pushAll
            [ toDiscardBy iid attrs attrs
            , TakeResources controllerId 5 (toAbilitySource attrs 2) False
            , Record YouHaveTranslatedTheGlyphs
            ]
        _ -> error "must be controlled"
      pure . ArchaicGlyphs $ attrs `with` Metadata True
    _ -> ArchaicGlyphs . (`with` meta) <$> runMessage msg attrs
