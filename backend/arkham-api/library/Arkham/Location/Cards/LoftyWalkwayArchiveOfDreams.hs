module Arkham.Location.Cards.LoftyWalkwayArchiveOfDreams (loftyWalkwayArchiveOfDreams) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.CourtOfTheAncients.Helpers
import Arkham.Trait (Trait (Passageway))

newtype LoftyWalkwayArchiveOfDreams = LoftyWalkwayArchiveOfDreams LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loftyWalkwayArchiveOfDreams :: LocationCard LoftyWalkwayArchiveOfDreams
loftyWalkwayArchiveOfDreams = location LoftyWalkwayArchiveOfDreams Cards.loftyWalkwayArchiveOfDreams 5 (Static 1)

instance HasModifiersFor LoftyWalkwayArchiveOfDreams where
  getModifiersFor (LoftyWalkwayArchiveOfDreams a) = do
    n <- getVictoryGlyphCount
    when (n > 0) $ modifySelf a [ShroudModifier (-n)]

instance HasAbilities LoftyWalkwayArchiveOfDreams where
  getAbilities (LoftyWalkwayArchiveOfDreams a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , restricted a 2 (Here <> exists (not_ (be a) <> RevealedLocation <> LocationWithTrait Passageway)) actionAbility
      ]

instance RunMessage LoftyWalkwayArchiveOfDreams where
  runMessage msg l@(LoftyWalkwayArchiveOfDreams attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      starSpawn <- getSetAsideCard Enemies.starSpawnObserver
      createEnemyAt_ starSpawn attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      passageways <- select $ not_ (be attrs) <> RevealedLocation <> LocationWithTrait Passageway
      chooseTargetM iid passageways $ moveTo (attrs.ability 2) iid
      pure l
    _ -> LoftyWalkwayArchiveOfDreams <$> liftRunMessage msg attrs
