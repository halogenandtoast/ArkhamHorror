module Arkham.Homebrew.ReturnToInnsmouth.Locations.MudTrack (mudTrack) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Discover (DiscoverLocation (..))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ReturnToInnsmouth.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Scenarios.TheInnsmouthConspiracy.HorrorInHighGear.Helpers

newtype MudTrack = MudTrack LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mudTrack :: LocationCard MudTrack
mudTrack =
  locationWith MudTrack Cards.mudTrack 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

{- | "While there are any clues on this location, vehicles cannot leave it."

Granted to the vehicles standing here as 'VehicleCannotMove', the same modifier Long Way
Around uses, which each car checks before moving. Note this is the investigators' cars
only -- the designer's FAQ is explicit that Vehicle *enemies* are not impeded, which is
what distinguishes Mud Track from Straight Section.
-}
instance HasModifiersFor MudTrack where
  getModifiersFor (MudTrack a) =
    when (a.clues > 0) $ modifySelect a (#vehicle <> assetAt a) [VehicleCannotMove]

instance HasAbilities MudTrack where
  getAbilities (MudTrack a) =
    extendRevealed1 a $ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)

{- | "Forced - After an investigator discovers one or more clues on Mud Track: Return
those clues to the token bank." Handled by intercepting the discovery rather than as an
ability, because the amount discovered is only on the message.
-}
instance RunMessage MudTrack where
  runMessage msg l@(MudTrack attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    Msg.DiscoverClues iid d | d.location == DiscoverAtLocation attrs.id -> do
      l' <- liftRunMessage msg attrs
      push $ Msg.InvestigatorSpendClues iid d.count
      pure $ MudTrack l'
    _ -> MudTrack <$> liftRunMessage msg attrs
