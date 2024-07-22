module Arkham.Treachery.Cards.StrangeSigns (strangeSigns, StrangeSigns (..)) where

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Helpers.Investigator
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype StrangeSigns = StrangeSigns TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSigns :: TreacheryCard StrangeSigns
strangeSigns = treachery StrangeSigns Cards.strangeSigns

instance RunMessage StrangeSigns where
  runMessage msg t@(StrangeSigns attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      playerCount <- getPlayerCount
      lid <- getJustLocation iid
      let clueCount = if playerCount == 3 || playerCount == 4 then 2 else 1
      push $ PlaceClues (toSource attrs) (LocationTarget lid) clueCount
      pure t
    _ -> StrangeSigns <$> runMessage msg attrs
