module Arkham.Investigator.Cards.AgnesBaker
  ( AgnesBaker(..)
  , agnesBaker
  ) where

import Arkham.Prelude

import Arkham.Query
import Arkham.Location.Types
import Arkham.Enemy.Types (Enemy)
import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.DamageEffect
import Arkham.GameValue
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype AgnesBaker = AgnesBaker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agnesBaker :: InvestigatorCard AgnesBaker
agnesBaker = investigator
  AgnesBaker
  Cards.agnesBaker
  Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 2
    , agility = 3
    }

instance HasAbilities AgnesBaker where
  getAbilities (AgnesBaker x) =
    [ limitedAbility (PlayerLimit PerPhase 1)
        $ restrictedAbility
            x
            1
            (Self <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
        $ ReactionAbility
            (PlacedCounter Timing.When You HorrorCounter (AtLeast $ Static 1))
            Free
    ]

instance HasTokenValue AgnesBaker where
  getTokenValue iid ElderSign (AgnesBaker attrs) | iid == toId attrs =
    pure $ TokenValue ElderSign $ PositiveModifier $ investigatorSanityDamage
      attrs
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage AgnesBaker where
  runMessage msg i@(AgnesBaker attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      targets <- map toId <$> selectG @Enemy (At $ iid `In` LocationInvestigators)
      push $ chooseOne
        iid
        [ targetLabel
            target
            [EnemyDamage target iid source NonAttackDamageEffect 1]
        | target <- targets
        ]
      pure i
    _ -> AgnesBaker <$> runMessage msg attrs
