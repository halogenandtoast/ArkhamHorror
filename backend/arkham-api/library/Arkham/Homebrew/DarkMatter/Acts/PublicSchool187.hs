module Arkham.Homebrew.DarkMatter.Acts.PublicSchool187 (
  publicSchool187V10,
  publicSchool187V20,
  publicSchool187V30,
) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Matcher qualified as Matcher

{- | The three versions of act 1 are mechanically identical — only the printed
starting schematic differs, and one is chosen at random during setup.

"Locations cannot be switched with each other." is enforced centrally by the
scenario's @switchLocations@ handler, which suppresses switching during act 1.

"Objective - After an investigator reveals the Entrance Hall, immediately
advance."
-}
revealEntranceHall :: ActAttrs -> Ability
revealEntranceHall a =
  mkAbility a 1
    $ forced
    $ Matcher.RevealLocation #after Anyone (locationIs Locations.entranceHall)

newtype PublicSchool187V10 = PublicSchool187V10 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

publicSchool187V10 :: ActCard PublicSchool187V10
publicSchool187V10 = act (1, A) PublicSchool187V10 Cards.publicSchool187V10 Nothing

instance HasAbilities PublicSchool187V10 where
  getAbilities (PublicSchool187V10 a) = [revealEntranceHall a]

instance RunMessage PublicSchool187V10 where
  runMessage msg a@(PublicSchool187V10 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> PublicSchool187V10 <$> liftRunMessage msg attrs

newtype PublicSchool187V20 = PublicSchool187V20 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

publicSchool187V20 :: ActCard PublicSchool187V20
publicSchool187V20 = act (1, A) PublicSchool187V20 Cards.publicSchool187V20 Nothing

instance HasAbilities PublicSchool187V20 where
  getAbilities (PublicSchool187V20 a) = [revealEntranceHall a]

instance RunMessage PublicSchool187V20 where
  runMessage msg a@(PublicSchool187V20 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> PublicSchool187V20 <$> liftRunMessage msg attrs

newtype PublicSchool187V30 = PublicSchool187V30 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

publicSchool187V30 :: ActCard PublicSchool187V30
publicSchool187V30 = act (1, A) PublicSchool187V30 Cards.publicSchool187V30 Nothing

instance HasAbilities PublicSchool187V30 where
  getAbilities (PublicSchool187V30 a) = [revealEntranceHall a]

instance RunMessage PublicSchool187V30 where
  runMessage msg a@(PublicSchool187V30 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> PublicSchool187V30 <$> liftRunMessage msg attrs
