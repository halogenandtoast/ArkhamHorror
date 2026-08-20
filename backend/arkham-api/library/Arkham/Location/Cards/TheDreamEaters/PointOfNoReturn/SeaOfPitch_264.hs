module Arkham.Location.Cards.TheDreamEaters.PointOfNoReturn.SeaOfPitch_264 (seaOfPitch_264) where

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story (readStory)
import Arkham.Location.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Story

newtype SeaOfPitch_264 = SeaOfPitch_264 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfPitch_264 :: LocationCard SeaOfPitch_264
seaOfPitch_264 = withXShroud $ location SeaOfPitch_264 Cards.seaOfPitch_264 0 (PerPlayer 1)

instance HasModifiersFor SeaOfPitch_264 where
  getModifiersFor (SeaOfPitch_264 a) = whenRevealed a do
    n <- scenarioCount Distortion
    modifySelf a [ShroudModifier n]

instance HasAbilities SeaOfPitch_264 where
  getAbilities (SeaOfPitch_264 attrs) = veiled attrs []

instance RunMessage SeaOfPitch_264 where
  runMessage msg l@(SeaOfPitch_264 attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      -- Rolling Pits re-enables every Sea of Pitch itself, so this is not special-cased here
      readStory iid (toId attrs) Story.rollingPits
      pure . SeaOfPitch_264 $ attrs & canBeFlippedL .~ False
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let storyCard = lookupCard Story.rollingPits (toCardId attrs)
      focusCards [storyCard] $ continue_ iid
      pure l
    _ -> SeaOfPitch_264 <$> liftRunMessage msg attrs
