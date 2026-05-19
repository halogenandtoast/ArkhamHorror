module Arkham.Agenda.Cards.TheHouseStirsV2 (theHouseStirsV2) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.HemlockHouse.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Dormant))
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheHouseStirsV2 = TheHouseStirsV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseStirsV2 :: AgendaCard TheHouseStirsV2
theHouseStirsV2 = agenda (2, A) TheHouseStirsV2 Cards.theHouseStirsV2 (Static 5)

instance HasAbilities TheHouseStirsV2 where
  getAbilities (TheHouseStirsV2 a) =
    [ mkAbility a 1 $ forced $ PhaseEnds #when #mythos
    , restricted a 2 (OnLocation LocationCanBeFlipped)
        $ freeTrigger
        $ OrCost
          [ PlaceClueOnLocationCost (PerPlayer 1)
          , scenarioI18n
              $ LabeledCost (ikey' "cost.removeSeal")
              $ SpendTokenCost Resource (LocationTargetMatches YourLocation)
          ]
    ]

instance RunMessage TheHouseStirsV2 where
  runMessage msg a@(TheHouseStirsV2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens lead (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid flipLocationOver
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      locations <-
        select $ NearestLocationToMost (LocationWithTrait Dormant <> LocationWithResources (atMost 0))
      leadChooseOrRunOneM do
        targets locations \location -> do
          card <- field LocationCard location
          push $ FlipToEnemyLocation location card
      fires <- getSetAsideCardsMatching (cardIs Treacheries.fire)
      for_ (nonEmpty fires) \(fireCard :| remainingFires) -> do
        mTarget <- selectOne $ HighestRow Anywhere <> LeftmostLocation
        for_ mTarget \target -> do
          tid <- getRandom
          push $ AttachStoryTreacheryTo tid fireCard (toTarget target)
        shuffleCardsIntoDeck Deck.EncounterDeck remainingFires
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> TheHouseStirsV2 <$> liftRunMessage msg attrs
