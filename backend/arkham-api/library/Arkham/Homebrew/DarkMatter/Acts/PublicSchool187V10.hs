module Arkham.Homebrew.DarkMatter.Acts.PublicSchool187V10 (publicSchool187V10) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Matcher qualified as Matcher

{- | One of the three printings of act 1; they are mechanically identical and
only the printed starting schematic differs, so setup picks one at random.

"Locations cannot be switched with each other." is enforced centrally by the
scenario's @switchLocations@ handler, which suppresses switching during act 1.

"Objective - After an investigator reveals the Entrance Hall, immediately
advance."
-}
newtype PublicSchool187V10 = PublicSchool187V10 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

publicSchool187V10 :: ActCard PublicSchool187V10
publicSchool187V10 = act (1, A) PublicSchool187V10 Cards.publicSchool187V10 Nothing

instance HasAbilities PublicSchool187V10 where
  getAbilities (PublicSchool187V10 a) =
    [ mkAbility a 1
        $ forced
        $ Matcher.RevealLocation #after Anyone (locationIs Locations.entranceHall)
    ]

instance RunMessage PublicSchool187V10 where
  runMessage msg a@(PublicSchool187V10 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> PublicSchool187V10 <$> liftRunMessage msg attrs
