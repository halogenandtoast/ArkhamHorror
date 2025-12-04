module Arkham.Agenda.Cards.RestlessDead (restlessDead) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Campaigns.TheScarletKeys.Concealed.Types
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Cost (getCanAffordCost, payEffectCost)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Geist))

newtype RestlessDead = RestlessDead AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor RestlessDead where
  getModifiersFor (RestlessDead a) = do
    n <- getPlayerCount
    modifySelf a [DoomThresholdModifier 1 | n == 1]

restlessDead :: AgendaCard RestlessDead
restlessDead = agenda (2, A) RestlessDead Cards.restlessDead (Static 3)

instance HasAbilities RestlessDead where
  getAbilities (RestlessDead a) =
    [ mkAbility a 1
        $ forced
        $ EnemyEngaged #when You (enemyIs Enemies.tzuSanNiangTheLadyWithTheRedParasol)
    ]

instance RunMessage RestlessDead where
  runMessage msg a@(RestlessDead attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mCost <- runMaybeT do
        loc <- MaybeT $ getLocationOf iid
        let cost = GroupClueCost (PerPlayer 1) (LocationWithId loc)
        liftGuardM $ getCanAffordCost iid (attrs.ability 1) [] [] cost
        pure cost
      maybe (drawEncounterCard iid attrs) (payEffectCost iid attrs) mCost
      pure a
    AdvanceAgenda (isSide A attrs -> True) -> do
      turnOverAllConcealed attrs
      RestlessDead <$> liftRunMessage msg attrs
    AdvanceAgenda (isSide B attrs -> True) -> do
      tzuSanNiang <- selectJust $ enemyIs Enemies.tzuSanNiangTheLadyWithTheRedParasol
      inTheShadows <- matches tzuSanNiang $ EnemyWithPlacement InTheShadows
      if inTheShadows
        then do
          selectEach IsDecoy removeFromGame
          tzuSanNiangMiniCard <- selectJust $ ConcealedCardIs TzuSanNiang
          mloc <- fieldMap ConcealedCardPlacement (preview _AtLocation) tzuSanNiangMiniCard.id
          when (isNothing mloc) $ error "invalid placement for Tzu San Niang's mini card"
          removeFromGame tzuSanNiangMiniCard
          for_ mloc \loc -> do
            place tzuSanNiang (AtLocation loc)
            geists <- selectWithField EnemyCard $ EnemyWithTrait Geist <> NonWeaknessEnemy <> enemyAt loc
            for_ geists \(geist, _) -> removeFromGame geist
            placeUnderneath tzuSanNiang $ map snd geists
            theShadeReaper <- selectJust $ scarletKeyIs Keys.theShadeReaper
            unless (null geists) $ placeTokens attrs theShadeReaper #charge 4
            doStep 1 msg
        else do
          lead <- getLead
          investigators <- select (InvestigatorAt $ locationWithEnemy tzuSanNiang)
          chooseOneAtATimeM lead $ targets investigators $ initiateEnemyAttack tzuSanNiang attrs
          forTarget tzuSanNiang msg
      revertAgenda attrs
      pure a
    DoStep 1 msg'@(AdvanceAgenda (isSide B attrs -> True)) -> do
      noGeists <- selectNone $ NonWeaknessEnemy <> EnemyWithTrait Geist
      if noGeists
        then advanceCurrentAct attrs
        else selectForMaybeM (enemyIs Enemies.tzuSanNiangTheLadyWithTheRedParasol) (`forTarget` msg')
      pure a
    ForTarget (EnemyTarget tzuSanNiang) (AdvanceAgenda (isSide B attrs -> True)) -> do
      lead <- getLead
      place tzuSanNiang InTheShadows
      mods <-
        map (,[CampaignModifier "noConcealed[TzuSanNiang]"])
          <$> select (not_ $ LocationWithEnemy $ EnemyWithTrait Geist)
      temporaryModifiersMany attrs mods do
        resolveConcealed lead tzuSanNiang
      pure a
    _ -> RestlessDead <$> liftRunMessage msg attrs
