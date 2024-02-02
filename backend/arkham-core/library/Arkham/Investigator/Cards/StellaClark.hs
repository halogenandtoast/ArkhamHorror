module Arkham.Investigator.Cards.StellaClark where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Timing qualified as Timing

newtype StellaClark = StellaClark InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

stellaClark :: InvestigatorCard StellaClark
stellaClark =
  investigator StellaClark Cards.stellaClark
    $ Stats {health = 8, sanity = 8, willpower = 3, intellect = 2, combat = 3, agility = 4}

instance HasAbilities StellaClark where
  getAbilities (StellaClark a) =
    [ playerLimit PerRound
        $ restrictedAbility a 1 Self
        $ freeReaction (SkillTestResult Timing.After You SkillTestWasFailed AnyResult)
    ]

instance HasChaosTokenValue StellaClark where
  getChaosTokenValue iid ElderSign (StellaClark attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage StellaClark where
  runMessage msg i@(StellaClark attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ GainActions iid source 1
      pure i
    When (RevealChaosToken _ iid token) | iid == toId attrs -> do
      faces <- getModifiedChaosTokenFace token
      when (ElderSign `elem` faces) $ do
        healDamage <- canHaveDamageHealed attrs iid
        mHealHorror <- getHealHorrorMessage attrs 1 iid
        player <- getPlayer iid
        push
          $ chooseOne player
          $ [ Label "Resolve as Elder Sign" []
            , Label
                "Automatically fail this skill test to heal 1 damage and 1 horror"
                $ FailSkillTest
                : [HealDamage (toTarget attrs) (toSource attrs) 1 | healDamage]
                  <> maybeToList mHealHorror
            ]
      pure i
    _ -> StellaClark <$> runMessage msg attrs
