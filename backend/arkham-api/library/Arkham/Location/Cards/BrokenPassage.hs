module Arkham.Location.Cards.BrokenPassage (brokenPassage) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BrokenPassage = BrokenPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenPassage :: LocationCard BrokenPassage
brokenPassage = symbolLabel $ location BrokenPassage Cards.brokenPassage 3 (Static 0)

instance HasAbilities BrokenPassage where
  getAbilities (BrokenPassage a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> youExist (not_ $ InvestigatorWithSupply Pickaxe))
      $ forced
      $ AttemptExplore #when You

instance RunMessage BrokenPassage where
  runMessage msg l@(BrokenPassage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> BrokenPassage <$> liftRunMessage msg attrs
