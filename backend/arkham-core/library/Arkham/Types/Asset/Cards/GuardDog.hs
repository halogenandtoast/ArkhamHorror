module Arkham.Types.Asset.Cards.GuardDog
  ( GuardDog(..)
  , guardDog
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing
import qualified Arkham.Types.Window as W

newtype GuardDog = GuardDog AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardDog :: AssetCard GuardDog
guardDog = ally GuardDog Cards.guardDog (3, 1)

instance HasModifiersFor env GuardDog

instance HasActions GuardDog where
  getActions (GuardDog x) =
    [ restrictedAbility
        x
        1
        OwnsThis
        (ReactionAbility
          (AssetDealtDamage Timing.When (AssetWithId (toId x)))
          Free
        )
    ]

instance (AssetRunner env) => RunMessage env GuardDog where
  runMessage msg a@(GuardDog attrs) = case msg of
    UseCardAbility _ source [W.Window Timing.When (W.DealtDamage (EnemySource eid) _)] 1 _
      | isSource attrs source
      -> a <$ push
        (chooseOne
          (getInvestigator attrs)
          [ EnemyDamage eid (getInvestigator attrs) (toSource attrs) 1
          , Continue "Do not use Guard Dog's ability"
          ]
        )
    _ -> GuardDog <$> runMessage msg attrs
