module Arkham.Asset.Cards.BoxingGloves
  ( boxingGloves
  , BoxingGloves(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyDefeated)
import Arkham.Card.CardType
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype BoxingGloves = BoxingGloves AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boxingGloves :: AssetCard BoxingGloves
boxingGloves = asset BoxingGloves Cards.boxingGloves

instance HasModifiersFor BoxingGloves where
  getModifiersFor (InvestigatorTarget iid) (BoxingGloves a) = pure
    [ toModifier a $ ActionSkillModifier Action.Fight SkillCombat 1
    | controlledBy a iid
    ]
  getModifiersFor _ _ = pure []

instance HasAbilities BoxingGloves where
  getAbilities (BoxingGloves a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (EnemyDefeated Timing.After You AnyEnemy)
        $ ExhaustCost
        $ toTarget a
    ]

instance RunMessage BoxingGloves where
  runMessage msg a@(BoxingGloves attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ Search
        iid
        source
        (InvestigatorTarget iid)
        [fromTopOfDeck 6]
        (CardWithType EventType <> CardWithTrait Spirit)
        (DrawFound iid 1)
      pure a
    _ -> BoxingGloves <$> runMessage msg attrs
