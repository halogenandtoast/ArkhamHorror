module Arkham.Homebrew.DarkMatter.Locations.MartianRuins (martianRuins) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (pattern CannotBeScannedFor)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MartianRuins = MartianRuins LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

martianRuins :: LocationCard MartianRuins
martianRuins = symbolLabel $ location MartianRuins Cards.martianRuins 4 (PerPlayer 1)

{- | "You cannot scan Martian Ruins while there is a ready enemy at this
location." A ban on the scan *target* (Martian Ruins's own printed symbol),
not on co-located investigators taking the Scan action at all — see
'CannotBeScannedFor'.
-}
instance HasModifiersFor MartianRuins where
  getModifiersFor (MartianRuins a) = do
    blocked <- selectAny $ enemyAt a.id <> ReadyEnemy
    when blocked $ modifySelf a [CannotBeScannedFor]

instance RunMessage MartianRuins where
  runMessage msg (MartianRuins attrs) = runQueueT $ case msg of
    {- "Revelation - Put this location into play and spawn the set aside Yithian
    Guard at this location."

    The engine puts the location into play when the scanned card is drawn, so the
    revelation only has the guard left to do. It has to happen here rather than
    on reveal: the printed "you cannot scan Martian Ruins while there is a ready
    enemy at this location" assumes the guard is already standing there. -}
    Revelation _ (isSource attrs -> True) -> do
      createEnemyAt_ Enemies.yithianGuard attrs.id
      MartianRuins <$> liftRunMessage msg attrs
    _ -> MartianRuins <$> liftRunMessage msg attrs
