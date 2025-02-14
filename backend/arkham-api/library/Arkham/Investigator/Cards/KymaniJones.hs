module Arkham.Investigator.Cards.KymaniJones (kymaniJones, kymaniJonesEffect) where

import Arkham.Ability
import Arkham.Effect.Import
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Helpers.Window (windowSkillTestId)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (SkillTestEnded)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype KymaniJones = KymaniJones InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

kymaniJones :: InvestigatorCard KymaniJones
kymaniJones =
  investigator KymaniJones Cards.kymaniJones
    $ Stats {health = 8, sanity = 6, willpower = 3, intellect = 2, combat = 2, agility = 5}

instance HasAbilities KymaniJones where
  getAbilities (KymaniJones x) =
    [ selfAbility x 1 (exists (CanEngageEnemy (x.ability 1) <> #exhausted <> enemyAtLocationWith x.id))
        $ FastAbility Free
    , selfAbility_ x 2 $ freeReaction $ AttemptToEvade #when You (NonEliteEnemy <> ExhaustedEnemy)
    ]

instance HasChaosTokenValue KymaniJones where
  getChaosTokenValue iid ElderSign (KymaniJones attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage KymaniJones where
  runMessage msg i@(KymaniJones attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ CanEngageEnemy (attrs.ability 1) <> #exhausted <> enemyAtLocationWith iid
      chooseTargetM iid enemies (engageEnemy iid)
      pure i
    UseCardAbility iid (isSource attrs -> True) 2 (windowSkillTestId -> sid) _ -> do
      skillTestModifier sid (attrs.ability 1) iid (AddSkillValue #intellect)
      createCardEffect Cards.kymaniJones Nothing (attrs.ability 1) iid
      pure i
    ElderSignEffect (is attrs -> True) -> do
      whenAny (ExhaustedEnemy <> enemyAtLocationWith attrs.id) passSkillTest
      pure i
    _ -> KymaniJones <$> liftRunMessage msg attrs

newtype KymaniJonesEffect = KymaniJonesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

kymaniJonesEffect :: EffectArgs -> KymaniJonesEffect
kymaniJonesEffect = cardEffect KymaniJonesEffect Cards.kymaniJones

instance RunMessage KymaniJonesEffect where
  runMessage msg e@(KymaniJonesEffect attrs) = runQueueT $ case msg of
    PassedThisSkillTestBy iid _source n -> do
      whenJustM getSkillTestTarget \target -> case target.enemy of
        Just enemy -> do
          mx <- field EnemyRemainingHealth enemy
          when (maybe False (n >=) mx) $ toDiscardBy iid attrs.source enemy
        _ -> pure ()
      pure e
    SkillTestEnded {} -> disableReturn e
    SkillTestEnds {} -> disableReturn e
    _ -> KymaniJonesEffect <$> liftRunMessage msg attrs
