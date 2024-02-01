module Arkham.Location.Cards.PalaceOfTheKing (
  palaceOfTheKing,
  PalaceOfTheKing (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype PalaceOfTheKing = PalaceOfTheKing LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

palaceOfTheKing :: LocationCard PalaceOfTheKing
palaceOfTheKing =
  locationWith
    PalaceOfTheKing
    Cards.palaceOfTheKing
    2
    (PerPlayer 3)
    ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasModifiersFor PalaceOfTheKing where
  getModifiersFor (LocationTarget lid) (PalaceOfTheKing attrs)
    | lid == toId attrs = do
        mHastur <- selectOne $ EnemyWithTitle "Hastur"
        modifiers' <- case mHastur of
          Nothing -> pure [CannotBeFlipped]
          Just hastur -> do
            n <- getPlayerCountValue (PerPlayer 5)
            hasEnoughDamage <- fieldP EnemyDamage (>= n) hastur
            pure [CannotBeFlipped | not hasEnoughDamage]
        pure $ toModifiers attrs modifiers'
  getModifiersFor _ _ = pure []

instance RunMessage PalaceOfTheKing where
  runMessage msg (PalaceOfTheKing attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.hastursEnd
      pure . PalaceOfTheKing $ attrs & canBeFlippedL .~ False
    _ -> PalaceOfTheKing <$> runMessage msg attrs
