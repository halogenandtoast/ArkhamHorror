module Arkham.Investigator.Cards.StellaClark where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype StellaClark = StellaClark InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stellaClark :: InvestigatorCard StellaClark
stellaClark = investigator
  StellaClark
  Cards.stellaClark
  Stats
    { health = 8
    , sanity = 8
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 4
    }

instance HasAbilities StellaClark where
  getAbilities (StellaClark a) =
    [ restrictedAbility
          a
          1
          Self
          (ReactionAbility
            (SkillTestResult Timing.After You SkillTestWasFailed AnyResult)
            Free
          )
        & abilityLimitL
        .~ PlayerLimit PerRound 1
    ]

instance HasTokenValue StellaClark where
  getTokenValue iid ElderSign (StellaClark attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage StellaClark where
  runMessage msg i@(StellaClark attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      i <$ push (GainActions iid source 1)
    When (RevealToken _ iid token)
      | iid == toId attrs && tokenFace token == ElderSign -> do
        i <$ push
          (chooseOne
            iid
            [ Label "Resolve as Elder Sign" []
            , Label
              "Automatically fail this skill test to heal 1 damage and 1 horror"
              [ FailSkillTest
              , HealDamage (toTarget attrs) (toSource attrs) 1
              , HealHorror (toTarget attrs) (toSource attrs) 1
              ]
            ]
          )
    _ -> StellaClark <$> runMessage msg attrs
