module Arkham.Treachery.Cards.CyclopeanArchitecture (cyclopeanArchitecture) where

import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Location.Types (Field (LocationPrintedShroud))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CyclopeanArchitecture = CyclopeanArchitecture TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanArchitecture :: TreacheryCard CyclopeanArchitecture
cyclopeanArchitecture = treachery CyclopeanArchitecture Cards.cyclopeanArchitecture

instance RunMessage CyclopeanArchitecture where
  runMessage msg t@(CyclopeanArchitecture attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      difficulty <- runMaybeT do
        lid <- MaybeT $ getMaybeLocation iid
        value <- MaybeT $ field LocationPrintedShroud lid
        lift $ getGameValue value
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed $ fromMaybe 0 difficulty)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) (min 3 -> n) -> do
      chooseAndDiscardCards iid attrs n
      pure t
    _ -> CyclopeanArchitecture <$> liftRunMessage msg attrs
