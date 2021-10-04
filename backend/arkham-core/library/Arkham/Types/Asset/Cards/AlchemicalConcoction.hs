module Arkham.Types.Asset.Cards.AlchemicalConcoction
  ( alchemicalConcoction
  , AlchemicalConcoction(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

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
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Fight)) _ (AlchemicalConcoction a)
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
