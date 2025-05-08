module Arkham.Location.Cards.StageOfTheWardTheatre (stageOfTheWardTheatre) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype StageOfTheWardTheatre = StageOfTheWardTheatre LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

stageOfTheWardTheatre :: LocationCard StageOfTheWardTheatre
stageOfTheWardTheatre =
  locationWith StageOfTheWardTheatre Cards.stageOfTheWardTheatre 0 (Static 0)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasModifiersFor StageOfTheWardTheatre where
  getModifiersFor (StageOfTheWardTheatre a) = do
    mHastur <- selectOne $ EnemyWithTitle "Hastur"
    for_ mHastur \hastur -> do
      modifySelect a (InvestigatorAt $ be a) [CannotAttackEnemy hastur]
      whenM (hastur <=~> MovingEnemy) do
        modifySelect
          a
          (locationWithEnemy hastur)
          [ConnectedToWhen Anywhere (LocationWithId a.id <> LocationWithInvestigator Anyone)]

instance RunMessage StageOfTheWardTheatre where
  runMessage msg (StageOfTheWardTheatre attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theDelusion
      pure . StageOfTheWardTheatre $ attrs & canBeFlippedL .~ False
    _ -> StageOfTheWardTheatre <$> liftRunMessage msg attrs
