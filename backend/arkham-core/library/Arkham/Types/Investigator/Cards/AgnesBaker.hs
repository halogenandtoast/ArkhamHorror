module Arkham.Types.Investigator.Cards.AgnesBaker
  ( AgnesBaker(..)
  , agnesBaker
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.DamageEffect
import Arkham.Types.GameValue
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher hiding (NonAttackDamageEffect)
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype AgnesBaker = AgnesBaker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

agnesBaker :: AgnesBaker
agnesBaker = AgnesBaker $ baseAttrs
  "01004"
  "Agnes Baker"
  Mystic
  Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 2
    , agility = 3
    }
  [Sorcerer]

instance HasAbilities AgnesBaker where
  getAbilities (AgnesBaker x) =
    [ restrictedAbility
          x
          1
          (Self <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
          (ReactionAbility
            (PlacedCounter Timing.When You HorrorCounter (AtLeast $ Static 1))
            Free
          )
        & (abilityLimitL .~ PlayerLimit PerPhase 1)
    ]

instance HasTokenValue env AgnesBaker where
  getTokenValue (AgnesBaker attrs) iid ElderSign | iid == toId attrs = do
    let tokenValue' = PositiveModifier $ investigatorSanityDamage attrs
    pure $ TokenValue ElderSign tokenValue'
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance InvestigatorRunner env => RunMessage env AgnesBaker where
  runMessage msg i@(AgnesBaker attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      enemyIds <- selectList (EnemyAt YourLocation)
      i <$ push
        (chooseOne
          (toId attrs)
          [ EnemyDamage eid (toId attrs) source NonAttackDamageEffect 1
          | eid <- enemyIds
          ]
        )
    _ -> AgnesBaker <$> runMessage msg attrs
