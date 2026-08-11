module Arkham.Homebrew.DarkMatter.Locations.ShipsBridge (shipsBridge) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ShipsBridge = ShipsBridge LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shipsBridge :: LocationCard ShipsBridge
shipsBridge = symbolLabel $ location ShipsBridge Cards.shipsBridge 3 (PerPlayer 1)

{- | Unrevealed: "Investigators cannot enter the Ship's Bridge from the Mess
Hall. Your access level is restricted. You will need to find another way
inside."
-}
instance HasModifiersFor ShipsBridge where
  getModifiersFor (ShipsBridge a) =
    modifySelectWhen a (not a.revealed) (InvestigatorAt $ locationIs Cards.messHall) [CannotEnter a.id]

{- | "Forced - After you perform a scan at this location, if there are no clues
on it: Add 1[per_investigator] clues on it from the token bank."
-}
instance HasAbilities ShipsBridge where
  getAbilities (ShipsBridge a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithoutClues)
      $ forced
      $ ScenarioEvent #after (Just You) "scan"

instance RunMessage ShipsBridge where
  runMessage msg l@(ShipsBridge attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      placeClues (attrs.ability 1) attrs n
      pure l
    _ -> ShipsBridge <$> liftRunMessage msg attrs
