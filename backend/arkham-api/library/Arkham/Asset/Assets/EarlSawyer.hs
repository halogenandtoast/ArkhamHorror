module Arkham.Asset.Assets.EarlSawyer (
  earlSawyer,
  EarlSawyer (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing

newtype EarlSawyer = EarlSawyer AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

earlSawyer :: AssetCard EarlSawyer
earlSawyer = ally EarlSawyer Cards.earlSawyer (3, 2)

instance HasAbilities EarlSawyer where
  getAbilities (EarlSawyer attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility (Matcher.EnemyEvaded Timing.After You AnyEnemy) (exhaust attrs)
    ]

instance HasModifiersFor EarlSawyer where
  getModifiersFor (EarlSawyer a) = controllerGets a [SkillModifier #agility 1]

instance RunMessage EarlSawyer where
  runMessage msg a@(EarlSawyer attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> EarlSawyer <$> runMessage msg attrs
