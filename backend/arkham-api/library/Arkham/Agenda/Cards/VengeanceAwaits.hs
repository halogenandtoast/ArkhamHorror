module Arkham.Agenda.Cards.VengeanceAwaits (vengeanceAwaits) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy (getUniqueEnemyMaybe)
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype VengeanceAwaits = VengeanceAwaits AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeanceAwaits :: AgendaCard VengeanceAwaits
vengeanceAwaits = agenda (3, A) VengeanceAwaits Cards.vengeanceAwaits (Static 5)

instance HasAbilities VengeanceAwaits where
  getAbilities (VengeanceAwaits a) | onSide A a = do
    [forcedAbility a 1 $ AgendaAdvances #when $ AgendaWithId (toId a)]
  getAbilities (VengeanceAwaits a) =
    [ mkAbility a 2 $ Objective $ forced $ EnemyDefeated #when Anyone ByAny $ enemyIs Enemies.umordhoth
    ]

instance RunMessage VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      actIds <- select AnyAct
      ritualSite <-
        if "01146" `elem` actIds
          then placeSetAsideLocation Locations.ritualSite
          else do
            ritualSite <- getJustLocationByName "Ritual Site"
            selectEach (enemyAt ritualSite) (toDiscard attrs)
            pure ritualSite

      -- Umordhoth (and, in Return To, its attached Vault of Earthly Demise) is
      -- placed out of play during setup. Move that existing entity into play so
      -- the attached vault comes with it, rather than spawning a fresh copy.
      getUniqueEnemyMaybe Enemies.umordhoth >>= \case
        Just umordhoth -> place umordhoth ritualSite
        Nothing ->
          selectOne (OutOfPlayEnemy SetAsideZone $ enemyIs Enemies.umordhoth) >>= \case
            Just umordhoth -> place umordhoth ritualSite
            Nothing -> do
              umordhoth <- getSetAsideCard Enemies.umordhoth
              createEnemyAt_ umordhoth ritualSite

      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R2
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach AnyAct (toDiscard attrs)
      pure a
    _ -> VengeanceAwaits <$> liftRunMessage msg attrs
