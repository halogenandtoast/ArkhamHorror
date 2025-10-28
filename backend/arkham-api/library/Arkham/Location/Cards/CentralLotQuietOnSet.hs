module Arkham.Location.Cards.CentralLotQuietOnSet (centralLotQuietOnSet) where

import Arkham.Ability
import Arkham.Card
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))

newtype CentralLotQuietOnSet = CentralLotQuietOnSet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centralLotQuietOnSet :: LocationCard CentralLotQuietOnSet
centralLotQuietOnSet = location CentralLotQuietOnSet Cards.centralLotQuietOnSet 2 (Static 0)

instance HasAbilities CentralLotQuietOnSet where
  getAbilities (CentralLotQuietOnSet a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> criteria)
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
   where
    criteria = if null a.underneath then Never else NoRestriction

instance RunMessage CentralLotQuietOnSet where
  runMessage msg l@(CentralLotQuietOnSet attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      case attrs.underneath of
        [] -> error "no cards underneath"
        (x : xs) -> do
          addToVictory x
          pure $ CentralLotQuietOnSet $ attrs & cardsUnderneathL .~ xs
    Flip _ _ (isTarget attrs -> True) -> do
      let blurred = lookupCard Cards.centralLotBlurred attrs.cardId
      push $ ReplaceLocation attrs.id blurred Swap
      pure l
    _ -> CentralLotQuietOnSet <$> liftRunMessage msg attrs
