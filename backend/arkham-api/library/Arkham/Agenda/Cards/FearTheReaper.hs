module Arkham.Agenda.Cards.FearTheReaper (fearTheReaper) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Geist))

newtype FearTheReaper = FearTheReaper AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fearTheReaper :: AgendaCard FearTheReaper
fearTheReaper = agenda (3, A) FearTheReaper Cards.fearTheReaper (Static 4)

instance HasModifiersFor FearTheReaper where
  getModifiersFor (FearTheReaper a) = do
    modifySelect
      a
      (EnemyWithTrait Geist)
      [AddKeyword Keyword.Hunter, AddKeyword Keyword.Retaliate, CannotBeFlipped]

instance HasAbilities FearTheReaper where
  getAbilities (FearTheReaper a) =
    [ restricted a 1 (exists $ EnemyWithAnyCardsUnderneath <> enemyIs Enemies.tzuSanNiangOutForBlood)
        $ forced
        $ PlacedDoomCounter #when AnySource (targetIs a)
    ]

instance RunMessage FearTheReaper where
  runMessage msg a@(FearTheReaper attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      tzuSanNiang <- selectJust $ enemyIs Enemies.tzuSanNiangOutForBlood
      withLocationOf tzuSanNiang \loc -> do
        cards <-
          fieldMap EnemyCardsUnderneath (filterCards $ card_ $ #enemy <> CardWithTrait Geist) tzuSanNiang
        focusCards cards do
          leadChooseOneM $ targets cards (`spawnEnemyAt_` loc)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      push R3
      pure a
    _ -> FearTheReaper <$> liftRunMessage msg attrs
