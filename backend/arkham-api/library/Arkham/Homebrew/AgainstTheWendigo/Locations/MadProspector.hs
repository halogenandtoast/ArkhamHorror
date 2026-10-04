module Arkham.Homebrew.AgainstTheWendigo.Locations.MadProspector (madProspector) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Stories
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MadProspector = MadProspector LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madProspector :: LocationCard MadProspector
madProspector = location MadProspector Cards.madProspector 3 (Static 0)

instance HasModifiersFor MadProspector where
  -- The unrevealed Mountain Range prints "You cannot move into the Mountain Range."
  getModifiersFor (MadProspector a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance HasAbilities MadProspector where
  getAbilities (MadProspector a) =
    extendRevealed1 a
      -- "{reaction} If there are no more clues on the Mad Prospector: Read the
      -- first part of the Hanninah's Gold story card."
      $ restricted
        a
        1
        ( Here
            <> notExists (be a <> LocationWithAnyClues)
            <> not_ (exists $ StoryIs $ Stories.hanninahsGold.cardCode)
        )
      $ triggered (DiscoverClues #after Anyone (be a) AnyValue) mempty

instance RunMessage MadProspector where
  runMessage msg l@(MadProspector attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.hanninahsGold
      pure l
    _ -> MadProspector <$> liftRunMessage msg attrs
