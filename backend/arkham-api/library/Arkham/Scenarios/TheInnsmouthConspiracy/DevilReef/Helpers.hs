module Arkham.Scenarios.TheInnsmouthConspiracy.DevilReef.Helpers where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Classes.HasGame
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario (getGrid)
import Arkham.I18n
import Arkham.Id
import Arkham.Key
import Arkham.Location.Grid
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Text

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "devilReef" a

noKeyAbilities :: [Ability] -> [Ability]
noKeyAbilities = filter \ab -> not (ab.index >= 500 && ab.index <= 520)

data Flashback = Flashback9 | Flashback10 | Flashback11

flashback :: ReverseQueue m => InvestigatorId -> Flashback -> m ()
flashback iid f = case f of
  Flashback9 -> do
    scenarioI18n $ story $ i18nWithTitle "flashback9"
    recoverMemory DiscoveryOfAStrangeIdol
    placeKey iid PurpleKey
    wavewornIdol <- getSetAsideCard Assets.wavewornIdol
    takeControlOfSetAsideAsset iid wavewornIdol
  Flashback10 -> do
    scenarioI18n $ story $ i18nWithTitle "flashback10"
    recoverMemory DiscoveryOfAnUnholyMantle
    placeKey iid WhiteKey
    awakenedMantle <- getSetAsideCard Assets.awakenedMantle
    takeControlOfSetAsideAsset iid awakenedMantle
  Flashback11 -> do
    scenarioI18n $ story $ i18nWithTitle "flashback11"
    recoverMemory DiscoveryOfAMysticalRelic
    placeKey iid BlackKey
    headdressOfYhaNthlei <- getSetAsideCard Assets.headdressOfYhaNthlei
    takeControlOfSetAsideAsset iid headdressOfYhaNthlei

{- | A cell beside an island, named in the island's own frame rather than by grid position:
'out' points away from Churning Waters and 'side' runs along the ring. Combine with '<>'.
-}
data IslandCell = IslandCell {outward :: Int, alongside :: Int}

instance Semigroup IslandCell where
  a <> b = IslandCell (a.outward + b.outward) (a.alongside + b.alongside)

instance Monoid IslandCell where
  mempty = IslandCell 0 0

out, back, side, otherSide :: IslandCell
out = IslandCell 1 0
back = IslandCell (-1) 0
side = IslandCell 0 1
otherSide = IslandCell 0 (-1)

{- | The cells an island reveals cards into, resolved against the seat it is actually in.

The ring has two kinds of seat: a pole, with open water beyond it, and a corner, which
faces outward along the row. The four corners are reflections of one another and the two
poles mirror each other, so an island describes itself twice -- once for a pole, once for a
corner -- and every seat follows, including the sixth seat the Return to box shuffles Cave
Mouth into.
-}
islandCells :: HasGame m => LocationId -> ([IslandCell], [IslandCell]) -> m [Pos]
islandCells lid (pole, corner) = do
  grid <- getGrid
  case findInGrid lid grid of
    Nothing -> error "a Devil Reef island is not on the grid"
    Just seat@(Pos x y)
      | x == 0 -> pure $ resolve seat (Pos 0 (signum y)) (Pos 1 0) pole
      | otherwise -> pure $ resolve seat (Pos (signum x) 0) (Pos 0 (negate (signum y))) corner
 where
  resolve (Pos sx sy) (Pos ux uy) (Pos vx vy) =
    map \c ->
      Pos (sx + c.outward * ux + c.alongside * vx) (sy + c.outward * uy + c.alongside * vy)
