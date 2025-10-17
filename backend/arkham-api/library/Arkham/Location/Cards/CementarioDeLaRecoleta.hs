module Arkham.Location.Cards.CementarioDeLaRecoleta (cementarioDeLaRecoleta) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CementarioDeLaRecoleta = CementarioDeLaRecoleta LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cementarioDeLaRecoleta :: LocationCard CementarioDeLaRecoleta
cementarioDeLaRecoleta = setLabel "h" $ location CementarioDeLaRecoleta Cards.cementarioDeLaRecoleta 2 (Static 3)

instance HasAbilities CementarioDeLaRecoleta where
  getAbilities (CementarioDeLaRecoleta a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithConcealedCard)
      $ actionAbilityWithCost (XCost $ clueCost 1 <> HorrorCost (a.ability 1) YouTarget 1)

instance RunMessage CementarioDeLaRecoleta where
  runMessage msg l@(CementarioDeLaRecoleta attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payments -> do
      let x = totalCluePayment payments
      concealedCards <- map toId <$> getConcealedAtAll attrs.id
      chooseNM iid x $ targets concealedCards $ revealConcealed iid (attrs.ability 1)
      pure l
    _ -> CementarioDeLaRecoleta <$> liftRunMessage msg attrs
