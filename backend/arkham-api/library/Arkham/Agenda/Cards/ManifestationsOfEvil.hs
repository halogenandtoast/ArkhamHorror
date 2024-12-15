module Arkham.Agenda.Cards.ManifestationsOfEvil (manifestationsOfEvil) where

import Arkham.Ability
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCardsUnderneath, EnemyLocation))
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype ManifestationsOfEvil = ManifestationsOfEvil AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manifestationsOfEvil :: AgendaCard ManifestationsOfEvil
manifestationsOfEvil = agenda (7, A) ManifestationsOfEvil Cards.manifestationsOfEvil (Static 2)

instance HasAbilities ManifestationsOfEvil where
  getAbilities (ManifestationsOfEvil a) =
    [ restricted a 1 (exists $ enemyIs Enemies.seepingNightmare <> EnemyWithAnyCardsUnderneath)
      $ forced
      $ AgendaWouldAdvance #when DoomThreshold (be a)
    | onSide A a
    ]

instance RunMessage ManifestationsOfEvil where
  runMessage msg a@(ManifestationsOfEvil attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      select TargetWithDoom >>= \case
        [x] -> removeDoom (attrs.ability 1) x 2
        xs -> do
          chooseTargetM lead xs \x -> do
            removeDoom (attrs.ability 1) x 1
            doStep 2 msg

      seepingNightmares <-
        maxes
          . mapMaybe (\(x, cards) -> (,length cards) . (x,) <$> nonEmpty cards)
          <$> selectWithField EnemyCardsUnderneath (enemyIs Enemies.seepingNightmare)

      chooseOneM lead do
        for_ seepingNightmares \(x, (n :| _)) -> do
          field EnemyLocation x >>= traverse_ \lid ->
            targeting x do
              obtainCard n
              createEnemy_ n lid
      pure a
    DoStep 2 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      select TargetWithDoom >>= \case
        [x] -> removeDoom (attrs.ability 1) x 1
        xs -> do
          lead <- getLead
          chooseTargetM lead xs \x -> removeDoom (attrs.ability 1) x 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        whenM hasRemainingFrostTokens do
          labeled "Add 1 {frost} token to the chaos bag" $ addChaosToken #frost
        labeled
          "In player order, each investigator shuffles the top 2 cards of the Tekeli-li deck into their deck (each investigator who cannot takes 1 damage and 1 horror instead)."
          $ eachInvestigator (`forInvestigator` msg)
      advanceAgendaDeck attrs
      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      cards <- getTekelili 2
      if length cards == 2
        then addTekelili iid cards
        else assignDamageAndHorror iid attrs 1 1
      pure a
    _ -> ManifestationsOfEvil <$> liftRunMessage msg attrs
