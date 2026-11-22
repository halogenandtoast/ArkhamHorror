module Arkham.Event.Events.CorsairsCache (corsairsCache) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Strategy

newtype CorsairsCache = CorsairsCache EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corsairsCache :: EventCard CorsairsCache
corsairsCache = event CorsairsCache Cards.corsairsCache

instance RunMessage CorsairsCache where
  runMessage msg e@(CorsairsCache attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      iids <- select $ affectsOthersKnown iid $ colocatedWith iid
      chooseOrRunOneM iid $ targets iids $ handleTarget iid attrs
      pure e
    -- An [[Item]] *card*, so the trait alone -- do not narrow to #asset.
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid') -> do
      search iid' attrs iid' [fromTopOfDeck 9] #item (defer attrs IsNotDraw)
      pure e
    -- Both the card and the branch are the SEARCHER's choice, not the choice of
    -- whoever played the event.
    SearchFound iid' (isTarget attrs -> True) _ cards | notNull cards -> do
      focusCards cards $ chooseOneM iid' $ targets cards \card ->
        chooseOneM iid' $ cardI18n $ scope "corsairsCache" do
          labeled "addToHand" $ addToHand iid' (only card)
          labeled "discardForResources" do
            addToDiscard iid' (only card)
            gainResources iid' attrs card.printedCost
      pure e
    _ -> CorsairsCache <$> liftRunMessage msg attrs
