module Arkham.Homebrew.DarkMatter.Acts.PublicSchool187V30 (publicSchool187V30) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Matcher qualified as Matcher

{- | One of the three printings of act 1; they are mechanically identical and
only the printed starting schematic differs, so setup picks one at random.

"Locations cannot be switched with each other." is enforced centrally by the
scenario's @switchLocations@ handler, which suppresses switching during act 1.

"Objective - After an investigator reveals the Entrance Hall, immediately
advance."
-}
newtype PublicSchool187V30 = PublicSchool187V30 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

publicSchool187V30 :: ActCard PublicSchool187V30
publicSchool187V30 = act (1, A) PublicSchool187V30 Cards.publicSchool187V30 Nothing

instance HasAbilities PublicSchool187V30 where
  getAbilities (PublicSchool187V30 a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ Matcher.RevealLocation #after Anyone (locationIs Locations.entranceHall)
    ]

instance RunMessage PublicSchool187V30 where
  runMessage msg a@(PublicSchool187V30 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- top row
      placeLocationInGrid_ (Pos (-1) 2) =<< fetchCard Locations.gymnasium
      placeLocationInGrid_ (Pos 0 2) =<< fetchCard Locations.library
      placeLocationInGrid_ (Pos 1 2) =<< fetchCard Locations.biologyLab
      -- middle outside locations
      placeLocationInGrid_ (Pos (-1) 1) =<< fetchCard Locations.cafeteria
      placeLocationInGrid_ (Pos 1 1) =<< fetchCard Locations.classroomK2
      advanceActDeck attrs
      pure a
    _ -> PublicSchool187V30 <$> liftRunMessage msg attrs
