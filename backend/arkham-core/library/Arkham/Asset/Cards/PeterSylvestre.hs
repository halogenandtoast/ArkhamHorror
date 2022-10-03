module Arkham.Asset.Cards.PeterSylvestre
  ( PeterSylvestre(..)
  , peterSylvestre
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

newtype PeterSylvestre = PeterSylvestre AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre :: AssetCard PeterSylvestre
peterSylvestre = ally PeterSylvestre Cards.peterSylvestre (1, 2)

instance HasModifiersFor PeterSylvestre where
  getModifiersFor (InvestigatorTarget iid) (PeterSylvestre a) =
    pure [ toModifier a (SkillModifier SkillAgility 1) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance HasAbilities PeterSylvestre where
  getAbilities (PeterSylvestre x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> AssetExists (AssetWithId (toId x) <> AssetWithHorror))
        (ReactionAbility (TurnEnds Timing.After You) Free)
    ]

instance RunMessage PeterSylvestre where
  runMessage msg (PeterSylvestre attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      pure $ PeterSylvestre $ attrs & horrorL -~ 1
    _ -> PeterSylvestre <$> runMessage msg attrs
