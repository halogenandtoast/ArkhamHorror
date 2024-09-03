module Arkham.Agenda.Cards.VengeanceAwaits (VengeanceAwaits (..), vengeanceAwaits) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.GameValue
import Arkham.Helpers.Message qualified as Msg
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Treacheries

getIsReturnTo :: HasGame m => m Bool
getIsReturnTo = (== "50032") <$> selectJust TheScenario

newtype VengeanceAwaits = VengeanceAwaits AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeanceAwaits :: AgendaCard VengeanceAwaits
vengeanceAwaits = agenda (3, A) VengeanceAwaits Cards.vengeanceAwaits (Static 5)

instance HasAbilities VengeanceAwaits where
  getAbilities (VengeanceAwaits a) | onSide A a = do
    [forcedAbility a 1 $ AgendaAdvances #when $ AgendaWithId (toId a)]
  getAbilities (VengeanceAwaits a) =
    [ mkAbility a 2 $ Objective $ forced $ EnemyDefeated #after Anyone ByAny $ enemyIs Enemies.umordhoth
    ]

instance RunMessage VengeanceAwaits where
  runMessage msg a@(VengeanceAwaits attrs@AgendaAttrs {..}) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      actIds <- select AnyAct
      umordhoth <- getSetAsideCard Enemies.umordhoth
      if "01146" `elem` actIds
        then do
          (ritualSite, placeRitualSite) <- placeSetAsideLocation Locations.ritualSite
          createUmordhoth <- createEnemyAt_ umordhoth ritualSite Nothing
          pushAll [placeRitualSite, createUmordhoth]
        else do
          ritualSite <- getJustLocationByName "Ritual Site"
          enemies <- selectTargets $ enemyAt ritualSite
          isReturnTo <- getIsReturnTo
          createUmordhoth <-
            if isReturnTo
              then do
                vaultOfEarthlyDemise <- genCard Treacheries.vaultOfEarthlyDemise
                tid <- getRandom

                creation <- Msg.createEnemy umordhoth (OutOfPlay SetAsideZone)
                pure
                  $ [ toMessage
                        $ creation
                          { enemyCreationBefore =
                              [AttachStoryTreacheryTo tid vaultOfEarthlyDemise (toTarget $ enemyCreationEnemyId creation)]
                          }
                    , PlaceEnemy (enemyCreationEnemyId creation) (AtLocation ritualSite)
                    ]
              else (: []) <$> createEnemyAt_ umordhoth ritualSite Nothing

          pushAll $ map (toDiscard attrs) enemies <> createUmordhoth
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R2
      pure a
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      actIds <- select AnyAct
      pushAll $ map (toDiscard GameSource) actIds
      pure a
    _ -> VengeanceAwaits <$> runMessage msg attrs
