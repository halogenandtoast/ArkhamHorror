module Arkham.Location.Cards.TombOfTheAncients (tombOfTheAncients) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype TombOfTheAncients = TombOfTheAncients LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tombOfTheAncients :: LocationCard TombOfTheAncients
tombOfTheAncients =
  location TombOfTheAncients Cards.tombOfTheAncients 4 (PerPlayer 1)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities TombOfTheAncients where
  getAbilities (TombOfTheAncients a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted
        a
        1
        (Here <> HasSupply Rope <> exists (LocationWithDistanceFromAtMost 3 (be a) RevealedLocation))
        actionAbility

instance RunMessage TombOfTheAncients where
  runMessage msg l@(TombOfTheAncients attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ LocationWithDistanceFromAtMost 3 (be attrs) RevealedLocation
      chooseTargetM iid locations \loc -> do
        moveTo (attrs.ability 1) iid loc
        roundModifier (attrs.ability 1) loc $ ConnectedToWhen Anywhere (LocationWithId attrs.id)
        roundModifier (attrs.ability 1) attrs $ ConnectedToWhen Anywhere (LocationWithId loc)
      pure l
    _ -> TombOfTheAncients <$> liftRunMessage msg attrs
