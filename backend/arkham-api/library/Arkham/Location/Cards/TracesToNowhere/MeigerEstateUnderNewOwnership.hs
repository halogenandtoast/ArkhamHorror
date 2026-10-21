module Arkham.Location.Cards.TracesToNowhere.MeigerEstateUnderNewOwnership (
  meigerEstateUnderNewOwnership,
) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Stories

newtype MeigerEstateUnderNewOwnership = MeigerEstateUnderNewOwnership LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meigerEstateUnderNewOwnership :: LocationCard MeigerEstateUnderNewOwnership
meigerEstateUnderNewOwnership =
  location MeigerEstateUnderNewOwnership Cards.meigerEstateUnderNewOwnership 4 (PerPlayer 1)

instance HasAbilities MeigerEstateUnderNewOwnership where
  getAbilities (MeigerEstateUnderNewOwnership a) =
    extendRevealed
      a
      [restricted a 1 (Here <> thisIs a LocationWithoutClues) actionAbility, hireACab a 2]

instance RunMessage MeigerEstateUnderNewOwnership where
  runMessage msg l@(MeigerEstateUnderNewOwnership attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.unlockingTheTruth
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hireACabEffect iid (attrs.ability 2)
      pure l
    _ -> MeigerEstateUnderNewOwnership <$> liftRunMessage msg attrs
