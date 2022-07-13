module Arkham.Asset.Cards.GuardDog
  ( GuardDog(..)
  , guardDog
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.DamageEffect
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype GuardDog = GuardDog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardDog :: AssetCard GuardDog
guardDog = ally GuardDog Cards.guardDog (3, 1)

instance HasAbilities GuardDog where
  getAbilities (GuardDog x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
            (AssetDealtDamage Timing.When (AssetWithId (toId x)))
            Free
    ]

instance RunMessage GuardDog where
  runMessage msg a@(GuardDog attrs) = case msg of
    UseCardAbility iid source [Window Timing.When (Window.DealtDamage (EnemySource eid) _ _)] 1 _
      | isSource attrs source
      -> a <$ push (EnemyDamage eid iid source NonAttackDamageEffect 1)
    _ -> GuardDog <$> runMessage msg attrs
