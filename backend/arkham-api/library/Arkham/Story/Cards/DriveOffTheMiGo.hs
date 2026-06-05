module Arkham.Story.Cards.DriveOffTheMiGo (driveOffTheMiGo) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DriveOffTheMiGo = DriveOffTheMiGo StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

driveOffTheMiGo :: StoryCard DriveOffTheMiGo
driveOffTheMiGo = story DriveOffTheMiGo Cards.driveOffTheMiGo

instance RunMessage DriveOffTheMiGo where
  runMessage msg (DriveOffTheMiGo attrs) = DriveOffTheMiGo <$> runMessage msg attrs
