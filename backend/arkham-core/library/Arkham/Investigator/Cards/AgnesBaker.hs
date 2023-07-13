module Arkham.Investigator.Cards.AgnesBaker (
  AgnesBaker (..),
  agnesBaker,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.GameValue
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype AgnesBaker = AgnesBaker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agnesBaker :: InvestigatorCard AgnesBaker
agnesBaker =
  investigator
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
    [ limitedAbility (PlayerLimit PerPhase 1) $
        restrictedAbility x 1 (Self <> enemyExists (EnemyAt YourLocation)) $
          ReactionAbility
            (PlacedCounter Timing.When You AnySource HorrorCounter $ AtLeast $ Static 1)
            Free
    ]

instance HasChaosTokenValue AgnesBaker where
  getChaosTokenValue iid ElderSign (AgnesBaker attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier $ investigatorSanityDamage attrs
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AgnesBaker where
  runMessage msg i@(AgnesBaker attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      targets <-
        selectList $
          EnemyAt (locationWithInvestigator $ toId attrs)
            <> EnemyCanBeDamagedBySource (toAbilitySource attrs 1)
      push $
        chooseOne
          iid
          [ targetLabel target [EnemyDamage target $ nonAttack attrs 1]
          | target <- targets
          ]
      pure i
    _ -> AgnesBaker <$> runMessage msg attrs
