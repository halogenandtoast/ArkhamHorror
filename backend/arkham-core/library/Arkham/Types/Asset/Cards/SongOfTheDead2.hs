module Arkham.Types.Asset.Cards.SongOfTheDead2
  ( songOfTheDead2
  , SongOfTheDead2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype SongOfTheDead2 = SongOfTheDead2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheDead2 :: AssetCard SongOfTheDead2
songOfTheDead2 = arcane SongOfTheDead2 Cards.songOfTheDead2

instance HasActions SongOfTheDead2 where
  getActions (SongOfTheDead2 x) =
    [ restrictedAbility x 1 OwnsThis $ ActionAbility (Just Action.Fight) $ Costs
        [ActionCost 1, UseCost (toId x) Charge 1]
    ]

instance HasModifiersFor env SongOfTheDead2

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SongOfTheDead2 where
  runMessage msg a@(SongOfTheDead2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 1)
      , CreateEffect "02112" Nothing source (InvestigatorTarget iid)
      , ChooseFightEnemy iid source SkillWillpower mempty False
      ]
    _ -> SongOfTheDead2 <$> runMessage msg attrs
