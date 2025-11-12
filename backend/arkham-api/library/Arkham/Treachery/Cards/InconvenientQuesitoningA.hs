module Arkham.Treachery.Cards.InconvenientQuesitoningA (inconvenientQuesitoningA) where

import Arkham.Helpers.Location (withLocationOf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Trait (Trait (Casino))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InconvenientQuesitoningA = InconvenientQuesitoningA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inconvenientQuesitoningA :: TreacheryCard InconvenientQuesitoningA
inconvenientQuesitoningA = treachery InconvenientQuesitoningA Cards.inconvenientQuesitoningA

instance RunMessage InconvenientQuesitoningA where
  runMessage msg t@(InconvenientQuesitoningA attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyTo iid $ InPlayEnemy $ EnemyWithTrait Casino <> not_ UniqueEnemy
      if null enemies
        then gainSurge attrs
        else do
          withLocationOf iid \loc -> chooseTargetM iid enemies \x -> moveTowards attrs x loc
          sid <- getRandom
          revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      raiseAlarmLevel (attrs.ability 1) [iid]
      enemies <- select $ EnemyWithTrait Casino <> not_ UniqueEnemy <> at_ (locationWithInvestigator iid)
      for_ enemies \enemy -> do
        roundModifier attrs enemy (RemoveKeyword Keyword.Aloof)
        enemyCheckEngagement enemy
      pure t
    _ -> InconvenientQuesitoningA <$> liftRunMessage msg attrs
