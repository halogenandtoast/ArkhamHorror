module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.MudTracks (mudTracks) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window (discoveredClues)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Scenarios.TheInnsmouthConspiracy.HorrorInHighGear.Helpers

newtype MudTracks = MudTracks LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mudTracks :: LocationCard MudTracks
mudTracks =
  locationWith MudTracks Cards.mudTracks 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

{- | "While there are any clues on this location, vehicles cannot leave it."

Granted to the vehicles standing here as 'VehicleCannotMove', the same modifier Long Way
Around uses, which each car checks before moving. Note this is the investigators' cars
only -- the designer's FAQ is explicit that Vehicle *enemies* are not impeded, which is
what distinguishes Mud Track from Straight Section.
-}
instance HasModifiersFor MudTracks where
  getModifiersFor (MudTracks a) =
    when (a.clues > 0) $ modifySelect a (#vehicle <> assetAt a) [VehicleCannotMove]

instance HasAbilities MudTracks where
  getAbilities (MudTracks a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , {- "Forced - After an investigator discovers one or more clues on Mud Tracks:
        Return those clues to the token bank." A real Forced so the players see it fire;
        the window carries how many were discovered. 'You' rather than 'Anyone' keeps it
        with the investigator who discovered them, whose clues go back. -}
        mkAbility a 2 $ forced $ DiscoverClues #after You (be a) (atLeast 1)
      ]

instance RunMessage MudTracks where
  runMessage msg l@(MudTracks attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (discoveredClues -> n) _ -> do
      push $ Msg.InvestigatorSpendClues iid n
      pure l
    _ -> MudTracks <$> liftRunMessage msg attrs
