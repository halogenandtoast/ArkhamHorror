module Arkham.Asset.Cards.PeterSylvestre2
  ( PeterSylvestre2(..)
  , peterSylvestre2
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype PeterSylvestre2 = PeterSylvestre2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre2 :: AssetCard PeterSylvestre2
peterSylvestre2 = ally PeterSylvestre2 Cards.peterSylvestre2 (1, 3)

instance HasModifiersFor PeterSylvestre2 where
  getModifiersFor (InvestigatorTarget iid) (PeterSylvestre2 a)
    | controlledBy a iid = pure $ toModifiers
      a
      [SkillModifier SkillAgility 1, SkillModifier SkillWillpower 1]
  getModifiersFor _ _ = pure []

instance HasAbilities PeterSylvestre2 where
  getAbilities (PeterSylvestre2 x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> AssetExists (AssetWithId (toId x) <> AssetWithHorror))
        (ReactionAbility (TurnEnds Timing.After You) Free)
    ]

instance RunMessage PeterSylvestre2 where
  runMessage msg (PeterSylvestre2 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      pure $ PeterSylvestre2 $ attrs & horrorL -~ 1
    _ -> PeterSylvestre2 <$> runMessage msg attrs
