module Arkham.Location.Cards.TrainTracks (trainTracks) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TrainTracks = TrainTracks LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trainTracks :: LocationCard TrainTracks
trainTracks = location TrainTracks Cards.trainTracks 3 (PerPlayer 1)

instance HasModifiersFor TrainTracks where
  getModifiersFor (TrainTracks a) = do
    modifySelectMap a (locationIs Cards.northside) \lid -> [ConnectedToWhen (LocationWithId lid) (be a)]

instance HasAbilities TrainTracks where
  getAbilities (TrainTracks a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (ClueCost $ Static 1)

instance RunMessage TrainTracks where
  runMessage msg l@(TrainTracks attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 4
      pure l
    _ -> TrainTracks <$> liftRunMessage msg attrs
