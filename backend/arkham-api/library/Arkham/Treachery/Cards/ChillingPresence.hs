module Arkham.Treachery.Cards.ChillingPresence (chillingPresence, chillingPresenceEffect) where

import Arkham.Effect.Import
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Geist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ChillingPresence = ChillingPresence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillingPresence :: TreacheryCard ChillingPresence
chillingPresence = treachery ChillingPresence Cards.chillingPresence

instance RunMessage ChillingPresence where
  runMessage msg t@(ChillingPresence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      createCardEffect Cards.chillingPresence Nothing attrs sid
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid (toSource attrs) n
      pure t
    PassedThisSkillTestBy _ (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(PassedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      geists <- select $ enemyAtLocationWith iid <> EnemyWithTrait Geist
      when (notNull geists) $ do
        chooseOneM iid do
          labeled "Deal 1 damage to a Geist enemy at your location" do
            chooseTargetM iid geists \enemy -> do
              nonAttackEnemyDamage (toSource attrs) 1 enemy
              doStep (n - 1) msg'
          labeled "Skip" nothing
      pure t
    _ -> ChillingPresence <$> liftRunMessage msg attrs

newtype ChillingPresenceEffect = ChillingPresenceEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillingPresenceEffect :: EffectArgs -> ChillingPresenceEffect
chillingPresenceEffect = cardEffect ChillingPresenceEffect Cards.chillingPresence

instance RunMessage ChillingPresenceEffect where
  runMessage msg e@(ChillingPresenceEffect attrs) = runQueueT $ case msg of
    Msg.RevealChaosToken (SkillTestSource sid) iid token | isTarget sid attrs.target -> do
      when (token.face == #eldersign) do
        geists <- select $ EnemyWithTrait Geist
        unless (null geists) $ do
          chooseOneM iid do
            labeled "Deal 2 damage to a Geist enemy at any location" do
              chooseTargetM iid geists $ nonAttackEnemyDamage attrs.source 2
            labeled "Skip" nothing
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    _ -> ChillingPresenceEffect <$> liftRunMessage msg attrs
