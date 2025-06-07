module Arkham.Location.Cards.ThroneRoom (throneRoom) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Direction
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ThroneRoom = ThroneRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throneRoom :: LocationCard ThroneRoom
throneRoom =
  location ThroneRoom Cards.throneRoom 5 (PerPlayer 1)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasModifiersFor ThroneRoom where
  getModifiersFor (ThroneRoom a) =
    whenRevealed a $ modifySelfWhen a (a.doom > 0) [InVictoryDisplayForCountingVengeance]

instance HasAbilities ThroneRoom where
  getAbilities (ThroneRoom a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        (Here <> thisExists a (LocationWithoutClues <> LocationWithDoom (static 0)))
        actionAbility

instance RunMessage ThroneRoom where
  runMessage msg l@(ThroneRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ PickSupply iid MysteriousScepter
      pure l
    _ -> ThroneRoom <$> liftRunMessage msg attrs
