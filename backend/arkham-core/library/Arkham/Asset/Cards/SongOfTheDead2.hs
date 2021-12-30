module Arkham.Asset.Cards.SongOfTheDead2
  ( songOfTheDead2
  , SongOfTheDead2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype SongOfTheDead2 = SongOfTheDead2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheDead2 :: AssetCard SongOfTheDead2
songOfTheDead2 = asset SongOfTheDead2 Cards.songOfTheDead2

instance HasAbilities SongOfTheDead2 where
  getAbilities (SongOfTheDead2 x) =
    [ restrictedAbility x 1 OwnsThis $ ActionAbility (Just Action.Fight) $ Costs
        [ActionCost 1, UseCost (toId x) Charge 1]
    ]

instance AssetRunner env => RunMessage env SongOfTheDead2 where
  runMessage msg a@(SongOfTheDead2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 1)
      , CreateEffect "02112" Nothing source (InvestigatorTarget iid)
      , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
      ]
    _ -> SongOfTheDead2 <$> runMessage msg attrs
