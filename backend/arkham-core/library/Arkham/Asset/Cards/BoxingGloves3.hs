module Arkham.Asset.Cards.BoxingGloves3
  ( boxingGloves3
  , BoxingGloves3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( EnemyDefeated )
import Arkham.Card.CardType
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype BoxingGloves3 = BoxingGloves3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boxingGloves3 :: AssetCard BoxingGloves3
boxingGloves3 = asset BoxingGloves3 Cards.boxingGloves3

instance HasModifiersFor BoxingGloves3 where
  getModifiersFor _ (InvestigatorTarget iid) (BoxingGloves3 a) = pure
    [ toModifier a $ ActionSkillModifier Action.Fight SkillCombat 2
    | controlledBy a iid
    ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BoxingGloves3 where
  getAbilities (BoxingGloves3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (EnemyDefeated Timing.After You AnyEnemy)
        $ ExhaustCost
        $ toTarget a
    ]

instance RunMessage BoxingGloves3 where
  runMessage msg a@(BoxingGloves3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ Search
        iid
        source
        (InvestigatorTarget iid)
        [fromTopOfDeck 9]
        (CardWithType EventType <> CardWithTrait Spirit)
        (DrawFound iid 1)
      pure a
    _ -> BoxingGloves3 <$> runMessage msg attrs
