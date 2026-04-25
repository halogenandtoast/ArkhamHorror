module Arkham.Location.Cards.MiskatonicQuad_c2026 (miskatonicQuad_c2026) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Location.Cards qualified as Cards (miskatonicQuad_c2026)
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MiskatonicQuad_c2026 = MiskatonicQuad_c2026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicQuad_c2026 :: LocationCard MiskatonicQuad_c2026
miskatonicQuad_c2026 = location MiskatonicQuad_c2026 Cards.miskatonicQuad_c2026 1 (Static 0)

instance HasAbilities MiskatonicQuad_c2026 where
  getAbilities (MiskatonicQuad_c2026 a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> oneOf (map PlayerCountIs [1, 2]) <> DuringTurn You <> CanMoveTo (ConnectedLocation ForMovement))
      $ FastAbility' Free #move

instance RunMessage MiskatonicQuad_c2026 where
  runMessage msg l@(MiskatonicQuad_c2026 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    _ -> MiskatonicQuad_c2026 <$> liftRunMessage msg attrs
