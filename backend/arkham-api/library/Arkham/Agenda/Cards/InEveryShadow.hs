module Arkham.Agenda.Cards.InEveryShadow (inEveryShadow) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Treachery.Cards qualified as Treacheries

newtype InEveryShadow = InEveryShadow AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inEveryShadow :: AgendaCard InEveryShadow
inEveryShadow = agenda (3, A) InEveryShadow Cards.inEveryShadow (Static 7)

instance HasAbilities InEveryShadow where
  getAbilities (InEveryShadow x) =
    [ groupLimit PerTestOrAbility
        $ mkAbility x 1
        $ forced
        $ EnemySpawns #when Anywhere
        $ enemyIs Enemies.huntingHorror
    ]

instance RunMessage InEveryShadow where
  runMessage msg a@(InEveryShadow attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectOne (treacheryIs Treacheries.shadowSpawned) >>= \case
        Just tid -> placeTokens (attrs.ability 1) tid #resource 1
        Nothing -> do
          tid <- getRandom
          shadowSpawned <- fetchCard Treacheries.shadowSpawned
          huntingHorror <- selectJust $ enemyIs Enemies.huntingHorror
          push $ AttachStoryTreacheryTo tid shadowSpawned (toTarget huntingHorror)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> InEveryShadow <$> liftRunMessage msg attrs
