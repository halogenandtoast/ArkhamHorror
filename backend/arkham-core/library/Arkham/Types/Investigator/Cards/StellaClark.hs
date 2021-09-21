module Arkham.Types.Investigator.Cards.StellaClark where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype StellaClark = StellaClark InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

stellaClark :: StellaClark
stellaClark = StellaClark $ baseAttrs
  "60501"
  ("Stella Clark" <:> "The Letter Carrier")
  Survivor
  Stats
    { health = 8
    , sanity = 8
    , willpower = 3
    , intellect = 2
    , combat = 3
    , agility = 4
    }
  [Chosen, Civic]

instance HasAbilities StellaClark where
  getAbilities (StellaClark a) =
    [ restrictedAbility a 1 Self
        $ ReactionAbility
            (SkillTestResult Timing.After You AnySkillTest AnyResult)
            Free
    ]

instance HasTokenValue env StellaClark where
  getTokenValue (StellaClark attrs) iid ElderSign | iid == toId attrs = do
    pure $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance InvestigatorRunner env => RunMessage env StellaClark where
  runMessage msg i@(StellaClark attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
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
              , HealDamage (toTarget attrs) 1
              , HealHorror (toTarget attrs) 1
              ]
            ]
          )
    _ -> StellaClark <$> runMessage msg attrs
