module Arkham.Location.Cards.MirrorNest_167 (mirrorNest_167) where

import Arkham.Ability
import Arkham.Helpers.History
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype MirrorNest_167 = MirrorNest_167 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorNest_167 :: LocationCard MirrorNest_167
mirrorNest_167 = location MirrorNest_167 Cards.mirrorNest_167 3 (PerPlayer 2)

instance HasAbilities MirrorNest_167 where
  getAbilities (MirrorNest_167 a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage MirrorNest_167 where
  runMessage msg l@(MirrorNest_167 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      successfulEvasions <- getHistoryField RoundHistory iid HistorySuccessfulEvasions
      when (successfulEvasions == 0) do
        clueCount <- field InvestigatorClues iid
        hasCards <- selectAny $ inHandOf NotForPlay iid
        chooseOrRunOneM iid $ withI18n do
          when (clueCount > 0)
            $ countVar 1
            $ labeled' "placeCluesOnYourLocation"
            $ placeCluesOnLocation iid (attrs.ability 1) 1
          when hasCards
            $ countVar 1
            $ labeled' "discardRandomCardsFromHand"
            $ randomDiscardN iid (attrs.ability 1) 1
      pure l
    _ -> MirrorNest_167 <$> liftRunMessage msg attrs
