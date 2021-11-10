module Arkham.Types.Asset.Cards.BaseballBat
  ( BaseballBat(..)
  , baseballBat
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype BaseballBat = BaseballBat AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

baseballBat :: AssetCard BaseballBat
baseballBat = asset BaseballBat Cards.baseballBat

instance HasModifiersFor env BaseballBat where
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Fight)) (InvestigatorTarget iid) (BaseballBat a)
    | ownedBy a iid && isSource a source
    = pure $ toModifiers a [DamageDealt 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BaseballBat where
  getAbilities (BaseballBat a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight) (ActionCost 1)
    ]

instance AssetRunner env => RunMessage env BaseballBat where
  runMessage msg a@(BaseballBat attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 2)
      , CreateEffect "01074" Nothing source (InvestigatorTarget iid)
      , ChooseFightEnemy iid source Nothing SkillCombat mempty False
      ]
    _ -> BaseballBat <$> runMessage msg attrs
