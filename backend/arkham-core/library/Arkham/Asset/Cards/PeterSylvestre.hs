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
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype PeterSylvestre = PeterSylvestre AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre :: AssetCard PeterSylvestre
peterSylvestre = ally PeterSylvestre Cards.peterSylvestre (1, 2)

instance HasModifiersFor env PeterSylvestre where
  getModifiersFor _ (InvestigatorTarget iid) (PeterSylvestre a) =
    pure [ toModifier a (SkillModifier SkillAgility 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities PeterSylvestre where
  getAbilities (PeterSylvestre x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> AssetExists (AssetWithId (toId x) <> AssetWithHorror))
        (ReactionAbility (TurnEnds Timing.After You) Free)
    ]

instance (AssetRunner env) => RunMessage env PeterSylvestre where
  runMessage msg (PeterSylvestre attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      pure $ PeterSylvestre $ attrs & sanityDamageL -~ 1
    _ -> PeterSylvestre <$> runMessage msg attrs
