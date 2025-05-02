module Arkham.Investigator.Cards.DanielaReyes (danielaReyes) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Enemy.Types (Field (EnemyAttacking))
import Arkham.Helpers.History (hasHistory)
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype Metadata = Metadata {enemiesThatAttackedYouSinceTheEndOfYourLastTurn :: [EnemyId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DanielaReyes = DanielaReyes InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

danielaReyes :: InvestigatorCard DanielaReyes
danielaReyes =
  investigator DanielaReyes Cards.danielaReyes
    $ Stats {health = 8, sanity = 6, willpower = 4, intellect = 1, combat = 5, agility = 2}

instance HasAbilities DanielaReyes where
  getAbilities (DanielaReyes a) =
    [ restrictedAbility a 1 Self
        $ freeReaction
        $ EnemyAttacksEvenIfCancelled #after You (not_ AttackOfOpportunityAttackYouProvoked)
        $ oneOf [EnemyCanBeDamagedBySource (a.ability 1), EnemyCanBeEvadedBy (a.ability 1)]
    ]

instance HasChaosTokenValue DanielaReyes where
  getChaosTokenValue iid ElderSign (DanielaReyes attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage DanielaReyes where
  runMessage msg i@(DanielaReyes attrs) = runQueueT $ case msg of
    SetupInvestigator iid | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      pure . DanielaReyes $ attrs' & setMeta (Metadata [])
    UseCardAbility iid (isSource attrs -> True) 1 (getAttackDetails -> attackEnemy -> enemy) _ -> do
      canDamage <- enemy <=~> EnemyCanBeDamagedBySource (attrs.ability 1)
      canEvade <- enemy <=~> EnemyCanBeEvadedBy (attrs.ability 1)
      chooseOneM iid do
        when canDamage
          $ labeled "Deal 1 damage to the enemy"
          $ nonAttackEnemyDamage (Just attrs.id) attrs 1 enemy
        when canEvade $ labeled "Automatically evade the enemy" $ automaticallyEvadeEnemy attrs.id enemy

      pure i
    PerformEnemyAttack eid -> do
      attrs' <- liftRunMessage msg attrs
      fieldMay EnemyAttacking eid >>= \case
        Just (Just details) | any (isTarget attrs') details.targets -> do
          let meta = toResultDefault (Metadata []) attrs'.meta
          pure
            . DanielaReyes
            $ attrs'
            & setMeta (Metadata (eid : enemiesThatAttackedYouSinceTheEndOfYourLastTurn meta))
        _ -> pure $ DanielaReyes attrs'
    EndTurn iid | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      let meta = toResultDefault (Metadata []) attrs.meta
      pure . DanielaReyes $ attrs' & setMeta (meta {enemiesThatAttackedYouSinceTheEndOfYourLastTurn = []})
    ElderSignEffect iid | attrs `is` iid -> do
      whenM (hasHistory #round AttackedByAnyEnemies attrs.id) passSkillTest
      pure i
    _ -> DanielaReyes <$> liftRunMessage msg attrs
