module Arkham.Location.Cards.BancoDeLaProvincia (bancoDeLaProvincia) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BancoDeLaProvincia = BancoDeLaProvincia LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bancoDeLaProvincia :: LocationCard BancoDeLaProvincia
bancoDeLaProvincia = setLabel "d" $ location BancoDeLaProvincia Cards.bancoDeLaProvincia 3 (PerPlayer 1)

instance HasAbilities BancoDeLaProvincia where
  getAbilities (BancoDeLaProvincia a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithConcealedCard)
      $ actionAbilityWithCost (XCost $ clueCost 1 <> ResourceCost 3)

instance RunMessage BancoDeLaProvincia where
  runMessage msg l@(BancoDeLaProvincia attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let x = totalCluePayment payments
      concealedCards <- map toId <$> getConcealedAtAll attrs.id
      chooseNM iid x $ targets concealedCards $ revealConcealed iid (attrs.ability 1)
      pure l
    _ -> BancoDeLaProvincia <$> liftRunMessage msg attrs
