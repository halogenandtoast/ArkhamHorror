module Arkham.Homebrew.AgainstTheWendigo.Locations.MadProspector (madProspector) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgainstTheWendigo.Helpers (
  connectsEastOrWest,
  mountainRangeReveal,
  readStoryAndChoose,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MadProspector = MadProspector LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madProspector :: LocationCard MadProspector
madProspector = locationWith MadProspector Cards.madProspector 3 (Static 0) connectsEastOrWest

instance HasModifiersFor MadProspector where
  -- The unrevealed Mountain Range prints "You cannot move into the Mountain Range."
  getModifiersFor (MadProspector a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance HasAbilities MadProspector where
  getAbilities (MadProspector a) =
    [mountainRangeReveal a | not a.revealed]
      -- "{reaction} If there are no more clues on the Mad Prospector: Read the
      -- first part of the Hanninah's Gold story card."
      <> extendRevealed1
        a
        ( onlyOnce
            $ restricted
              a
              1
              ( Here
                  <> thisExists a LocationWithoutClues
                  <> not_ (exists $ StoryIs Stories.hanninahsGold.cardCode)
              )
            $ freeReaction AnyWindow
        )

instance RunMessage MadProspector where
  runMessage msg l@(MadProspector attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 98 -> do
      reveal attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStoryAndChoose iid Stories.hanninahsGold
      pure l
    _ -> MadProspector <$> liftRunMessage msg attrs
