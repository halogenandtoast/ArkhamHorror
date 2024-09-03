module Arkham.Event.Cards.NatureOfTheBeast1 (natureOfTheBeast1, NatureOfTheBeast1 (..)) where

import Arkham.Card
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query (getPlayer)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message (SearchType (..))
import Arkham.Message qualified as Msg
import Arkham.Strategy

newtype NatureOfTheBeast1 = NatureOfTheBeast1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

natureOfTheBeast1 :: EventCard NatureOfTheBeast1
natureOfTheBeast1 = event NatureOfTheBeast1 Cards.natureOfTheBeast1

instance RunMessage NatureOfTheBeast1 where
  runMessage msg e@(NatureOfTheBeast1 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      push
        $ Search Revealing iid (toSource attrs) #encounterDeck [fromTopOfDeck 3] #any (defer attrs IsDraw)
      push $ DoStep 2 msg
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      investigators <- traverse (traverseToSnd getPlayer) =<< select (affectsOthers Anyone)
      let choices iid' =
            [ targetLabel card $ map AddToEncounterDiscard rest <> [InvestigatorDrewEncounterCard iid' card]
            | (card, rest) <- eachWithRest (onlyEncounterCards cards)
            ]
      chooseOne
        iid
        [targetLabel iid' [Msg.chooseOne player (choices iid')] | (iid', player) <- investigators]
      pure e
    DoStep 2 (Revelation owner (isSource attrs -> True)) -> do
      investigators <- select (affectsOthers Anyone)

      choices <- forMaybeM investigators \iid -> do
        player <- getPlayer iid
        locations <- select $ LocationWithDiscoverableCluesBy (InvestigatorWithId iid) <> RevealedLocation
        pure
          $ guard (notNull locations)
          $> targetLabel
            iid
            [ Msg.chooseOne player [targetLabel lid [DiscoverClues iid $ discover lid attrs 1] | lid <- locations]
            ]

      when (notNull choices) $ do
        chooseOne owner choices
      pure e
    _ -> NatureOfTheBeast1 <$> liftRunMessage msg attrs
