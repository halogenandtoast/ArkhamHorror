module Arkham.Agenda.Cards.TheThirdAct (theThirdAct) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Zone

newtype TheThirdAct = TheThirdAct AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdAct :: AgendaCard TheThirdAct
theThirdAct = agenda (1, A) TheThirdAct Cards.theThirdAct (Static 6)

instance RunMessage TheThirdAct where
  runMessage msg a@(TheThirdAct attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectOne (IncludeOutOfPlayEnemy $ enemyIs Cards.royalEmissary) >>= \case
        Just royalEmissary -> do
          theatre <- selectJust $ locationIs Cards.theatre
          push $ EnemySpawnFromOutOfPlay SetAsideZone Nothing theatre royalEmissary
        Nothing -> do
          mRoyalEmissary <-
            selectOne
              $ ExtendedCardWithOneOf
                [ SetAsideCardMatch $ cardIs Cards.royalEmissary
                , VictoryDisplayCardMatch $ basic $ cardIs Cards.royalEmissary
                ]
          royalEmissary <- maybe (genCard Cards.royalEmissary) pure mRoyalEmissary
          createEnemyAtLocationMatching_ royalEmissary $ locationIs Cards.theatre

      advanceAgendaDeck attrs
      pure a
    _ -> TheThirdAct <$> liftRunMessage msg attrs
