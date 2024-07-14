module Arkham.Investigator.Cards.StellaClark where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (healDamage)
import Arkham.Matcher hiding (RevealChaosToken)

newtype StellaClark = StellaClark InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stellaClark :: InvestigatorCard StellaClark
stellaClark =
  investigator StellaClark Cards.stellaClark
    $ Stats {health = 8, sanity = 8, willpower = 3, intellect = 2, combat = 3, agility = 4}

instance HasAbilities StellaClark where
  getAbilities (StellaClark a) =
    [ playerLimit PerRound
        $ restrictedAbility a 1 Self
        $ freeReaction (SkillTestResult #after You #failed #any)
    ]

instance HasChaosTokenValue StellaClark where
  getChaosTokenValue iid ElderSign (StellaClark attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage StellaClark where
  runMessage msg i@(StellaClark attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ GainActions iid (attrs.ability 1) 1
      pure i
    When (RevealChaosToken _ iid token) | iid == toId attrs -> do
      faces <- getModifiedChaosTokenFace token
      when (ElderSign `elem` faces) $ do
        healDamage <- canHaveDamageHealed attrs iid
        healHorror <- canHaveHorrorHealed attrs iid
        chooseOne
          iid
          [ Label "Resolve as Elder Sign" []
          , Label
              "Automatically fail this skill test to heal 1 damage and 1 horror"
              $ FailSkillTest
              : [HealDamage (toTarget attrs) (toSource attrs) 1 | healDamage]
                <> [HealHorror (toTarget attrs) (toSource attrs) 1 | healHorror]
          ]
      pure i
    _ -> StellaClark <$> liftRunMessage msg attrs
