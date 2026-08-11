module Arkham.Homebrew.DarkMatter.Locations.MartianRuins (martianRuins) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MartianRuins = MartianRuins LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

martianRuins :: LocationCard MartianRuins
martianRuins = location MartianRuins Cards.martianRuins 4 (PerPlayer 1)

-- | "You cannot scan Martian Ruins while there is a ready enemy at this location."
instance HasModifiersFor MartianRuins where
  getModifiersFor (MartianRuins a) = do
    blocked <- selectAny $ enemyAt a.id <> ReadyEnemy
    when blocked $ modifySelect a (investigatorAt a.id) [CannotTakeAction $ IsAction Scan]

{- | "Revelation - Put this location into play and spawn the set aside Yithian
Guard at this location."
-}
instance HasAbilities MartianRuins where
  getAbilities (MartianRuins a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage MartianRuins where
  runMessage msg l@(MartianRuins attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      createEnemyAt_ Enemies.yithianGuard attrs.id
      pure l
    _ -> MartianRuins <$> liftRunMessage msg attrs
