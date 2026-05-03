{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.MiskatonicQuad_MiskatonicUniversity (miskatonicQuad_MiskatonicUniversity) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Location.Cards qualified as Cards (miskatonicQuad_MiskatonicUniversity)
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MiskatonicQuad_MiskatonicUniversity = MiskatonicQuad_MiskatonicUniversity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicQuad_MiskatonicUniversity :: LocationCard MiskatonicQuad_MiskatonicUniversity
miskatonicQuad_MiskatonicUniversity = location MiskatonicQuad_MiskatonicUniversity Cards.miskatonicQuad_MiskatonicUniversity 1 (Static 0)

instance HasAbilities MiskatonicQuad_MiskatonicUniversity where
  getAbilities (MiskatonicQuad_MiskatonicUniversity a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted
        a
        1
        ( Here
            <> oneOf (map PlayerCountIs [1, 2])
            <> DuringTurn You
            <> CanMoveTo (ConnectedLocation ForMovement)
        )
      $ FastAbility' Free #move

instance RunMessage MiskatonicQuad_MiskatonicUniversity where
  runMessage msg l@(MiskatonicQuad_MiskatonicUniversity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    _ -> MiskatonicQuad_MiskatonicUniversity <$> liftRunMessage msg attrs
