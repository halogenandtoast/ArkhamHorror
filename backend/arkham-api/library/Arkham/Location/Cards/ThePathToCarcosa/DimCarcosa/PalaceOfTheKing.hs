module Arkham.Location.Cards.ThePathToCarcosa.DimCarcosa.PalaceOfTheKing (palaceOfTheKing) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.ThePathToCarcosa.DimCarcosa qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.ThePathToCarcosa.DimCarcosa.Helpers
import Arkham.Story.CardDefs.ThePathToCarcosa.DimCarcosa qualified as Story

newtype PalaceOfTheKing = PalaceOfTheKing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

palaceOfTheKing :: LocationCard PalaceOfTheKing
palaceOfTheKing =
  locationWith PalaceOfTheKing Cards.palaceOfTheKing 2 (PerPlayer 3)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasModifiersFor PalaceOfTheKing where
  getModifiersFor (PalaceOfTheKing attrs) = do
    mHastur <- selectOne $ EnemyWithTitle "Hastur"
    modifiers' <- case mHastur of
      Nothing -> pure [CannotBeFlipped]
      Just hastur -> do
        n <- getPlayerCountValue (PerPlayer 5)
        hasEnoughDamage <- fieldP EnemyDamage (>= n) hastur
        pure [CannotBeFlipped | not hasEnoughDamage]
    modifySelf attrs modifiers'

instance RunMessage PalaceOfTheKing where
  runMessage msg (PalaceOfTheKing attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.hastursEnd
      pure . PalaceOfTheKing $ attrs & canBeFlippedL .~ False
    _ -> PalaceOfTheKing <$> liftRunMessage msg attrs
