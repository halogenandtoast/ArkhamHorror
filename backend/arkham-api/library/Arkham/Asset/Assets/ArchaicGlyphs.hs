module Arkham.Asset.Assets.ArchaicGlyphs (archaicGlyphs) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.CampaignLogKey
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.SkillType

newtype Metadata = Metadata {discarding :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ArchaicGlyphs = ArchaicGlyphs (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archaicGlyphs :: AssetCard ArchaicGlyphs
archaicGlyphs = asset (ArchaicGlyphs . (`with` Metadata False)) Cards.archaicGlyphs

instance HasAbilities ArchaicGlyphs where
  getAbilities (ArchaicGlyphs (attrs `With` meta)) =
    [ restricted attrs 1 ControlsThis
        $ actionAbilityWithCost
        $ SkillIconCost 1 (singleton $ SkillIcon #intellect)
    , controlled attrs 2 ability2Criteria $ forced AnyWindow
    ]
   where
    ability2Criteria =
      if discarding meta
        then Never
        else exists (be attrs <> AssetWithUseCount Secret (atLeast 3))

instance RunMessage ArchaicGlyphs where
  runMessage msg a@(ArchaicGlyphs (attrs `With` meta)) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Secret 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      case attrs.placement of
        InPlayArea controllerId -> do
          toDiscardBy iid attrs attrs
          gainResources controllerId (attrs.ability 2) 5
          record YouHaveTranslatedTheGlyphs
        _ -> error "must be controlled"
      pure . ArchaicGlyphs $ attrs `with` Metadata True
    _ -> ArchaicGlyphs . (`with` meta) <$> liftRunMessage msg attrs
