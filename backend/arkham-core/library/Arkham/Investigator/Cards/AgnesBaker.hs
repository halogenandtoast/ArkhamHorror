module Arkham.Investigator.Cards.AgnesBaker
  ( AgnesBaker(..)
  , agnesBaker
  ) where

import Arkham.Prelude

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
  deriving anyclass (IsInvestigator, HasModifiersFor env)
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

instance HasTokenValue env AgnesBaker where
  getTokenValue iid ElderSign (AgnesBaker attrs) | iid == toId attrs =
    pure $ TokenValue ElderSign $ PositiveModifier $ investigatorSanityDamage
      attrs
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance InvestigatorRunner env => RunMessage env AgnesBaker where
  runMessage msg i@(AgnesBaker attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      enemyIds <- selectList $ EnemyAt YourLocation
      push $ chooseOne
        (toId attrs)
        [ EnemyDamage eid (toId attrs) source NonAttackDamageEffect 1
        | eid <- enemyIds
        ]
      pure i
    _ -> AgnesBaker <$> runMessage msg attrs
