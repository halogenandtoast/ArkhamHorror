module Arkham.Agenda.Cards.TheHouseStirsV1 (theHouseStirsV1) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.HemlockHouse.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Dormant))

newtype TheHouseStirsV1 = TheHouseStirsV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseStirsV1 :: AgendaCard TheHouseStirsV1
theHouseStirsV1 = agenda (2, A) TheHouseStirsV1 Cards.theHouseStirsV1 (Static 5)

instance HasAbilities TheHouseStirsV1 where
  getAbilities (TheHouseStirsV1 a) =
    [ mkAbility a 1 $ forced $ PhaseEnds #when #mythos
    , mkAbility a 2
        $ freeTrigger
        $ OrCost
          [ PlaceClueOnLocationCost (PerPlayer 1)
          , scenarioI18n
              $ LabeledCost (ikey' "cost.removeSeal")
              $ SpendTokenCost Resource (LocationTargetMatches YourLocation)
          ]
    ]

instance RunMessage TheHouseStirsV1 where
  runMessage msg a@(TheHouseStirsV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens lead (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid flipLocationOver
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      locations <-
        select $ NearestLocationToMost (LocationWithTrait Dormant <> LocationWithResources (atMost 0))
      leadChooseOrRunOneM do
        targets locations \location -> do
          card <- field LocationCard location
          push $ FlipToEnemyLocation location card
      advanceAgendaDeck attrs
      pure a
    _ -> TheHouseStirsV1 <$> liftRunMessage msg attrs
