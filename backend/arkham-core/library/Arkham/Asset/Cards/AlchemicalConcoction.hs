module Arkham.Asset.Cards.AlchemicalConcoction
  ( alchemicalConcoction
  , AlchemicalConcoction(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Id
import Arkham.Modifier
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype AlchemicalConcoction = AlchemicalConcoction AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalConcoction :: AssetCard AlchemicalConcoction
alchemicalConcoction = asset AlchemicalConcoction Cards.alchemicalConcoction

instance HasAbilities AlchemicalConcoction where
  getAbilities (AlchemicalConcoction a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
    ]

instance (HasId CardCode env EnemyId, HasSkillTest env) => HasModifiersFor env AlchemicalConcoction where
  getModifiersFor (SkillTestSource _ _ source (Just Action.Fight)) _ (AlchemicalConcoction a)
    | isSource a source
    = do
      skillTestTarget <- fromJustNote "not a skilltest" <$> getSkillTestTarget
      case skillTestTarget of
        EnemyTarget eid -> do
          cardCode <- getId eid
          pure $ toModifiers a [ DamageDealt 6 | cardCode == CardCode "02059" ]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env AlchemicalConcoction where
  runMessage msg a@(AlchemicalConcoction attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ pushAll
        [ skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
        , CreateEffect "01060" Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
        ]
    _ -> AlchemicalConcoction <$> runMessage msg attrs
