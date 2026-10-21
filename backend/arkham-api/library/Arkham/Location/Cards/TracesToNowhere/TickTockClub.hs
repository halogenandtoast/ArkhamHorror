module Arkham.Location.Cards.TracesToNowhere.TickTockClub (tickTockClub) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Stories

newtype TickTockClub = TickTockClub LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tickTockClub :: LocationCard TickTockClub
tickTockClub = location TickTockClub Cards.tickTockClub 1 (PerPlayer 3)

instance HasAbilities TickTockClub where
  getAbilities (TickTockClub a) =
    extendRevealed a [restricted a 1 Here actionAbility, hireACab a 2]

instance RunMessage TickTockClub where
  runMessage msg l@(TickTockClub attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.gamblingYourTimeAway
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hireACabEffect iid (attrs.ability 2)
      pure l
    _ -> TickTockClub <$> liftRunMessage msg attrs
