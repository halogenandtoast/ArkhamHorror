module Arkham.Treachery.Cards.LitByDeathFire (litByDeathFire, LitByDeathFire (..)) where

import Arkham.Helpers.Message.Discard
import Arkham.Matcher
import Arkham.Trait (Trait (Depths, Vale))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LitByDeathFire = LitByDeathFire TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

litByDeathFire :: TreacheryCard LitByDeathFire
litByDeathFire = treachery LitByDeathFire Cards.litByDeathFire

instance RunMessage LitByDeathFire where
  runMessage msg t@(LitByDeathFire attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      eachInvestigator \iid -> push $ LoseResources iid (toSource attrs) 1
      selectEach (InvestigatorAt $ oneOf [LocationWithTrait Depths, LocationWithTrait Vale]) \iid -> do
        push $ toMessage $ chooseAndDiscardCard iid (toSource attrs)
      selectEach (InvestigatorAt $ LocationWithTrait Depths) \iid -> do
        push $ LoseActions iid (toSource attrs) 1

      pure t
    _ -> LitByDeathFire <$> lift (runMessage msg attrs)
