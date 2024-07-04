module Arkham.Investigator.Cards.DanielaReyes (danielaReyes, DanielaReyes (..)) where

import Arkham.Ability
import Arkham.Attack
import Arkham.DamageEffect
import Arkham.Enemy.Types (Field (EnemyAttacking))
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

data Metadata = Metadata
  { enemiesThatAttackedYouSinceTheEndOfYourLastTurn :: [EnemyId]
  , elderSignAutoSucceeds :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DanielaReyes = DanielaReyes InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielaReyes :: InvestigatorCard DanielaReyes
danielaReyes =
  investigator
    DanielaReyes
    Cards.danielaReyes
    Stats {health = 8, sanity = 6, willpower = 4, intellect = 1, combat = 5, agility = 2}

instance HasAbilities DanielaReyes where
  getAbilities (DanielaReyes a) =
    [ restrictedAbility a 1 Self
        $ freeReaction
        $ EnemyAttacksEvenIfCancelled
          #after
          You
          (not_ AttackOfOpportunityAttackYouProvoked)
          ( oneOf
              [ EnemyCanBeDamagedBySource (a.ability 1)
              , EnemyCanBeEvadedBy (a.ability 1)
              ]
          )
    ]

instance HasChaosTokenValue DanielaReyes where
  getChaosTokenValue iid ElderSign (DanielaReyes attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

toEnemy :: [Window] -> EnemyId
toEnemy [] = error "invalid call"
toEnemy ((windowType -> Window.EnemyAttacksEvenIfCancelled details) : _) =
  attackEnemy details
toEnemy (_ : xs) = toEnemy xs

instance RunMessage DanielaReyes where
  runMessage msg i@(DanielaReyes attrs) = runQueueT $ case msg of
    SetupInvestigator iid | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      pure . DanielaReyes $ attrs' & setMeta (Metadata [] False)
    UseCardAbility iid (isSource attrs -> True) 1 (toEnemy -> enemy) _ -> do
      canDamage <- enemy <=~> EnemyCanBeDamagedBySource (attrs.ability 1)
      canEvade <- enemy <=~> EnemyCanBeEvadedBy (attrs.ability 1)
      chooseOne iid
        $ [Label "Deal 1 damage to the enemy" [EnemyDamage enemy $ nonAttack attrs 1] | canDamage]
        <> [Label "Automatically evade the enemy" [EnemyEvaded attrs.id enemy] | canEvade]

      pure i
    PerformEnemyAttack eid -> do
      field EnemyAttacking eid >>= \case
        Just details | isTarget attrs details.target -> do
          let meta = toResult attrs.meta
          pure
            . DanielaReyes
            $ attrs
            & setMeta (Metadata (eid : enemiesThatAttackedYouSinceTheEndOfYourLastTurn meta) True)
        _ -> pure i
    EndRound -> do
      attrs' <- liftRunMessage msg attrs
      let meta = toResult attrs.meta
      pure . DanielaReyes $ attrs' & setMeta (meta {elderSignAutoSucceeds = False})
    EndTurn iid | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      let meta = toResult attrs.meta
      pure . DanielaReyes $ attrs' & setMeta (meta {enemiesThatAttackedYouSinceTheEndOfYourLastTurn = []})
    ElderSignEffect iid | attrs `is` iid -> do
      pushWhen (elderSignAutoSucceeds $ toResult attrs.meta) PassSkillTest
      pure i
    _ -> DanielaReyes <$> liftRunMessage msg attrs
