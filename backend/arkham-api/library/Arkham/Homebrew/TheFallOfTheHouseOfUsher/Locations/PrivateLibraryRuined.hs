module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.PrivateLibraryRuined (privateLibraryRuined) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTest, skillTestMatches)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PrivateLibraryRuined = PrivateLibraryRuined LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

privateLibraryRuined :: LocationCard PrivateLibraryRuined
privateLibraryRuined = location PrivateLibraryRuined Cards.privateLibraryRuined 3 (Static 0)

instance HasModifiersFor PrivateLibraryRuined where
  getModifiersFor (PrivateLibraryRuined a) = whenRevealed a do
    getSkillTest >>= traverse_ \st ->
      whenM (skillTestMatches st.investigator (toSource a) st SkillTestFromRevelation)
        $ hereGets a [AnySkillValue (-2)]

instance RunMessage PrivateLibraryRuined where
  runMessage msg (PrivateLibraryRuined attrs) =
    runQueueT $ PrivateLibraryRuined <$> liftRunMessage msg attrs
