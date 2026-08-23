module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.FoyerBoringParty (foyerBoringParty) where

import Arkham.Ability hiding (resignAction)
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FoyerBoringParty = FoyerBoringParty LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyerBoringParty :: LocationCard FoyerBoringParty
foyerBoringParty = symbolLabel $ location FoyerBoringParty Cards.foyerBoringParty 2 (Static 0)

topUpTargets :: LocationMatcher
topUpTargets = RevealedLocation <> not_ LocationWithVictory <> LocationNotAtClueLimit

instance HasAbilities FoyerBoringParty where
  getAbilities (FoyerBoringParty a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> exists topUpTargets) actionAbility
      , resignAction a
      ]

instance RunMessage FoyerBoringParty where
  runMessage msg l@(FoyerBoringParty attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach topUpTargets \lid -> placeClues (attrs.ability 1) lid 1
      pure l
    _ -> FoyerBoringParty <$> liftRunMessage msg attrs
