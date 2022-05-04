module Arkham.Asset.Cards.ArchaicGlyphs
  ( archaicGlyphs
  , ArchaicGlyphs(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Cost
import Arkham.Criteria
import Arkham.SkillType

newtype ArchaicGlyphs = ArchaicGlyphs AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archaicGlyphs :: AssetCard ArchaicGlyphs
archaicGlyphs = asset ArchaicGlyphs Cards.archaicGlyphs

instance HasAbilities ArchaicGlyphs where
  getAbilities (ArchaicGlyphs attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ActionAbility Nothing
        $ SkillIconCost 1
        $ singleton SkillIntellect
    ]

instance AssetRunner env => RunMessage env ArchaicGlyphs where
  runMessage msg a@(ArchaicGlyphs attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AddUses (toTarget attrs) Secret 1)
    AddUses target Secret _ | isTarget attrs target -> do
      let controllerId = fromJustNote "must be controller" $ assetController attrs
      attrs' <- runMessage msg attrs
      ArchaicGlyphs attrs' <$ when
        (useCount (assetUses attrs') >= 3)
        (pushAll
          [ Discard (toTarget attrs)
          , TakeResources controllerId 5 False
          , Record YouHaveTranslatedTheGlyphs
          ]
        )
    _ -> ArchaicGlyphs <$> runMessage msg attrs
