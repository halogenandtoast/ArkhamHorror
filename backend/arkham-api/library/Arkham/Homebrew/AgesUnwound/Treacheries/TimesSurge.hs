module Arkham.Homebrew.AgesUnwound.Treacheries.TimesSurge (timesSurge) where

import Arkham.Ability
import Arkham.Card (countCards, filterCards, toCard)
import Arkham.Helpers.Scenario (getEncounterDiscard, peekEncounterDeck)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (..))
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TimesSurge = TimesSurge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timesSurge :: TreacheryCard TimesSurge
timesSurge = treachery TimesSurge Cards.timesSurge

getDrawer :: [Window] -> Maybe InvestigatorId
getDrawer = \case
  [] -> Nothing
  ((windowType -> Window.WouldDrawEncounterCard iid _ _) : _) -> Just iid
  (_ : ws) -> getDrawer ws

instance HasAbilities TimesSurge where
  getAbilities (TimesSurge a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ freeReaction
        $ WouldDrawEncounterCard #when Anyone AnyPhase
    ]

instance RunMessage TimesSurge where
  runMessage msg t@(TimesSurge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      doStep 3 msg
      placeInThreatArea attrs iid
      pure t
    DoStep n r@(Revelation iid (isSource attrs -> True)) | n > 0 -> do
      cards <- peekEncounterDeck 1
      case filterCards (cardIs Cards.timesEbb) cards of
        ebb : _ -> do
          getPlayer iid >>= (`sendRevelation` toJSON (toCard ebb))
          addToEncounterDiscard (only ebb)
          doStep n r
        [] -> do
          discarded <- getEncounterDiscard RegularEncounterDeck
          if null cards && countCards (cardIs Cards.timesEbb) discarded < length discarded
            then do
              shuffleEncounterDiscardBackIn
              doStep n r
            else do
              drawEncounterCards iid attrs 1
              doStep (n - 1) r
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawer -> mDrawer) _ -> do
      for_ mDrawer \drawer -> do
        insteadOf (DoDrawCards drawer) $ gainActions drawer (attrs.ability 1) 1
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> TimesSurge <$> liftRunMessage msg attrs
