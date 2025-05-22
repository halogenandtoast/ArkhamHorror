module Arkham.Location.Cards.ReturnToPalaceOfTheKing (returnToPalaceOfTheKing) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype ReturnToPalaceOfTheKing = ReturnToPalaceOfTheKing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

returnToPalaceOfTheKing :: LocationCard ReturnToPalaceOfTheKing
returnToPalaceOfTheKing =
  locationWith ReturnToPalaceOfTheKing Cards.returnToPalaceOfTheKing 1 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasModifiersFor ReturnToPalaceOfTheKing where
  getModifiersFor (ReturnToPalaceOfTheKing attrs) = do
    mHastur <- selectOne $ EnemyWithTitle "Hastur"
    modifiers' <- case mHastur of
      Nothing -> pure [CannotBeFlipped]
      Just hastur -> do
        n <- getPlayerCountValue (PerPlayer 2)
        notEnough <- fieldP EnemyRemainingHealth ((> n) . fromMaybe 0) hastur
        pure [CannotBeFlipped | notEnough]
    modifySelf attrs modifiers'

instance RunMessage ReturnToPalaceOfTheKing where
  runMessage msg (ReturnToPalaceOfTheKing attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.hastursLastStand
      pure . ReturnToPalaceOfTheKing $ attrs & canBeFlippedL .~ False
    _ -> ReturnToPalaceOfTheKing <$> liftRunMessage msg attrs
