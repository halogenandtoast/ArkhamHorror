module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.Office (office) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Discover (insteadOfDiscoveringClues)

newtype Office = Office LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

office :: LocationCard Office
office = symbolLabel $ location Office Cards.office 4 (PerPlayer 1)

instance HasAbilities Office where
  getAbilities (Office a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ freeReaction
      $ WouldDiscoverClues #when You (be a) (atLeast 1)

instance RunMessage Office where
  runMessage msg l@(Office attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      insteadOfDiscoveringClues iid \discover -> gainResources iid (attrs.ability 1) (2 * discover.count)
      pure l
    _ -> Office <$> liftRunMessage msg attrs
