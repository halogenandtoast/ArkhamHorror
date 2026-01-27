module Arkham.Investigator.Cards.DanielaReyes2 (danielaReyes2) where

import Arkham.Ability
import Arkham.Helpers.Window (getEnemy)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype DanielaReyes2 = DanielaReyes2 InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

danielaReyes2 :: InvestigatorCard DanielaReyes2
danielaReyes2 =
  investigator DanielaReyes2 Cards.danielaReyes2
    $ Stats {health = 9, sanity = 5, willpower = 3, intellect = 2, combat = 5, agility = 2}

instance HasAbilities DanielaReyes2 where
  getAbilities (DanielaReyes2 a) =
    [ playerLimit PerRound
        $ selfAbility_ a 1
        $ freeReaction (EnemyAttacks #after (InvestigatorAt YourLocation) AnyEnemyAttack AnyEnemy)
    ]

instance HasChaosTokenValue DanielaReyes2 where
  getChaosTokenValue iid ElderSign (DanielaReyes2 attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage DanielaReyes2 where
  runMessage msg i@(DanielaReyes2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      nextSkillTestModifier iid (attrs.ability 1) iid (MustFight enemy)
      performActionAction iid (attrs.ability 1) #fight
      pure i
    ElderSignEffect iid | attrs `is` iid -> do
      enemies <-
        select
          $ EnemyAt (locationWithInvestigator iid)
          <> EnemyCanBeDamagedBySource (ElderSignEffectSource iid)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (ElderSignEffectSource iid) 1
      pure i
    _ -> DanielaReyes2 <$> liftRunMessage msg attrs
