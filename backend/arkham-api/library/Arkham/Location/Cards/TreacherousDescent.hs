module Arkham.Location.Cards.TreacherousDescent (treacherousDescent) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype TreacherousDescent = TreacherousDescent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousDescent :: LocationCard TreacherousDescent
treacherousDescent = symbolLabel $ location TreacherousDescent Cards.treacherousDescent 6 (PerPlayer 1)

instance HasAbilities TreacherousDescent where
  getAbilities (TreacherousDescent a) =
    extendRevealed1 a
      $ limited (GroupLimit PerGame 3)
      $ restricted a 1 (Here <> youExist (InvestigatorWithSupply Pickaxe)) actionAbility

instance RunMessage TreacherousDescent where
  runMessage msg l@(TreacherousDescent attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      gameModifier (attrs.ability 1) attrs (ShroudModifier (-2))
      pure l
    _ -> TreacherousDescent <$> liftRunMessage msg attrs
