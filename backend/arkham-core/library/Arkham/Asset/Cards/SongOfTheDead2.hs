module Arkham.Asset.Cards.SongOfTheDead2 (
  songOfTheDead2,
  SongOfTheDead2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype SongOfTheDead2 = SongOfTheDead2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

songOfTheDead2 :: AssetCard SongOfTheDead2
songOfTheDead2 = asset SongOfTheDead2 Cards.songOfTheDead2

instance HasAbilities SongOfTheDead2 where
  getAbilities (SongOfTheDead2 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ActionAbility ([Action.Fight])
        $ Costs
          [ActionCost 1, UseCost (AssetWithId $ toId x) Charge 1]
    ]

instance RunMessage SongOfTheDead2 where
  runMessage msg a@(SongOfTheDead2 attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ pushAll
              [ skillTestModifier
                  source
                  (InvestigatorTarget iid)
                  (SkillModifier SkillWillpower 1)
              , CreateEffect "02112" Nothing source (InvestigatorTarget iid)
              , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
              ]
    _ -> SongOfTheDead2 <$> runMessage msg attrs
