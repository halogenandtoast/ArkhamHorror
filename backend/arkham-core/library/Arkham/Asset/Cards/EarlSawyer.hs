module Arkham.Asset.Cards.EarlSawyer
  ( earlSawyer
  , EarlSawyer(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype EarlSawyer = EarlSawyer AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

earlSawyer :: AssetCard EarlSawyer
earlSawyer = ally EarlSawyer Cards.earlSawyer (3, 2)

instance HasAbilities EarlSawyer where
  getAbilities (EarlSawyer attrs) =
    [ restrictedAbility attrs 1 ControlsThis $ ReactionAbility
        (Matcher.EnemyEvaded Timing.After You AnyEnemy)
        (ExhaustCost $ toTarget attrs)
    ]

instance HasModifiersFor EarlSawyer where
  getModifiersFor (InvestigatorTarget iid) (EarlSawyer a) =
    pure [ toModifier a (SkillModifier SkillAgility 1) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance RunMessage EarlSawyer where
  runMessage msg a@(EarlSawyer attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ drawCards iid attrs 1
      pure a
    _ -> EarlSawyer <$> runMessage msg attrs
