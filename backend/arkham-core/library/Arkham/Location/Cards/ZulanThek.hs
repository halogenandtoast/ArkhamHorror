module Arkham.Location.Cards.ZulanThek (zulanThek, ZulanThek (..)) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype ZulanThek = ZulanThek LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

zulanThek :: LocationCard ZulanThek
zulanThek = location ZulanThek Cards.zulanThek 4 (PerPlayer 1)

instance HasAbilities ZulanThek where
  getAbilities (ZulanThek attrs) =
    veiled
      attrs
      [ restrictedAbility attrs 1 (exists Enemies.hordeOfNight <> exists (investigatorAt attrs.id))
          $ forced
          $ RoundEnds #when
      ]

instance RunMessage ZulanThek where
  runMessage msg l@(ZulanThek attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      hordeOfNight <- selectJust $ enemyIs Enemies.hordeOfNight
      n <- selectCount $ investigatorAt attrs.id
      lead <- getLead
      push $ PlaceSwarmCards lead hordeOfNight n
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theCryptOfZulanThek
      pure . ZulanThek $ attrs & canBeFlippedL .~ False
    _ -> ZulanThek <$> runMessage msg attrs
