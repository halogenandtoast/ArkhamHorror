module Arkham.Agenda.Cards.TheTurn (theTurn) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Message.Lifted.Story
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Casino))

newtype TheTurn = TheTurn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTurn :: AgendaCard TheTurn
theTurn = agenda (3, A) TheTurn Cards.theTurn (Static 3)

instance HasAbilities TheTurn where
  getAbilities (TheTurn a) =
    [ restricted a 1 (exists $ InvestigatorAt $ LocationWithEnemy $ EnemyWithTrait Casino <> ReadyEnemy)
        $ forced
        $ PhaseBegins #when #enemy
    , mkAbility a 2 $ forced $ EnemyDefeated #after You ByAny (EnemyWithTrait Casino)
    ]

instance RunMessage TheTurn where
  runMessage msg a@(TheTurn attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      raiseAlarmLevel (attrs.ability 1)
        =<< select (InvestigatorAt $ LocationWithEnemy $ EnemyWithTrait Casino <> ReadyEnemy)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      raiseAlarmLevel (attrs.ability 2) [iid]
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      fortunesDisfavor <-
        shuffle
          =<< getSetAsideCardsMatching
            (mapOneOf cardIs [Stories.fortunesDisfavor25, Stories.fortunesDisfavor26, Stories.fortunesDisfavor27])
      for_ (nonEmpty fortunesDisfavor) \(card :| _) -> do
        resolveStory lead card
      advanceAgendaDeck attrs
      pure a
    _ -> TheTurn <$> liftRunMessage msg attrs
