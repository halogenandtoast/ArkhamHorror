module Arkham.Location.Cards.ChthonianDepths (chthonianDepths) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ChthonianDepths = ChthonianDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chthonianDepths :: LocationCard ChthonianDepths
chthonianDepths = location ChthonianDepths Cards.chthonianDepths 3 (PerPlayer 1)

instance HasAbilities ChthonianDepths where
  getAbilities (ChthonianDepths a) =
    extendRevealed1 a
      $ restricted a 2 (notExists $ InvestigatorWithSupply Torches <> at_ (be a))
      $ forced
      $ DiscoveringLastClue #after Anyone (be a)

instance RunMessage ChthonianDepths where
  runMessage msg l@(ChthonianDepths attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> ChthonianDepths <$> liftRunMessage msg attrs
