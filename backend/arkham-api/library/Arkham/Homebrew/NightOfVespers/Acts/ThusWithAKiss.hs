module Arkham.Homebrew.NightOfVespers.Acts.ThusWithAKiss (thusWithAKiss) where

import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.NightOfVespers.CardDefs.Acts qualified as Cards
import Arkham.Matcher.Base

newtype ThusWithAKiss = ThusWithAKiss ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thusWithAKiss :: ActCard ThusWithAKiss
thusWithAKiss = act (1, A) ThusWithAKiss Cards.thusWithAKiss Nothing

instance RunMessage ThusWithAKiss where
  runMessage msg a@(ThusWithAKiss attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      getSetAsideCardsMatching (oneOf ["Trattoria Kitchens", "Bar L'Ammaliatrice"])
        >>= traverse_ placeLocation
      advanceActDeck attrs
      pure a
    _ -> ThusWithAKiss <$> liftRunMessage msg attrs
