module Arkham.Location.Cards.TheThroneRoom (theThroneRoom) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype TheThroneRoom = TheThroneRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theThroneRoom :: LocationCard TheThroneRoom
theThroneRoom =
  locationWith TheThroneRoom Cards.theThroneRoom 2 (PerPlayer 2)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasModifiersFor TheThroneRoom where
  getModifiersFor (TheThroneRoom a) = do
    modifySelect
      a
      (EnemyWithTitle "Hastur")
      [ CannotMove
      , CannotBeMoved
      , DoNotExhaust
      , CannotMakeAttacksOfOpportunity
      ]

instance RunMessage TheThroneRoom where
  runMessage msg (TheThroneRoom attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theEntity
      pure . TheThroneRoom $ attrs & canBeFlippedL .~ False
    _ -> TheThroneRoom <$> liftRunMessage msg attrs
