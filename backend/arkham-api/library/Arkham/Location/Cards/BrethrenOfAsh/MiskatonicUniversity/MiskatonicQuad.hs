{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.BrethrenOfAsh.MiskatonicUniversity.MiskatonicQuad (miskatonicQuad) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Location.CardDefs.BrethrenOfAsh.MiskatonicUniversity qualified as Cards (
  miskatonicQuad,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MiskatonicQuad = MiskatonicQuad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicQuad :: LocationCard MiskatonicQuad
miskatonicQuad =
  location MiskatonicQuad Cards.miskatonicQuad 1 (Static 0)

instance HasAbilities MiskatonicQuad where
  getAbilities (MiskatonicQuad a) =
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

instance RunMessage MiskatonicQuad where
  runMessage msg l@(MiskatonicQuad attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    _ -> MiskatonicQuad <$> liftRunMessage msg attrs
