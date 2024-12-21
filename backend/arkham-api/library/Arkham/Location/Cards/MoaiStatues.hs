module Arkham.Location.Cards.MoaiStatues (moaiStatues) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype MoaiStatues = MoaiStatues LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moaiStatues :: LocationCard MoaiStatues
moaiStatues = location MoaiStatues Cards.moaiStatues 5 (PerPlayer 2)

instance HasModifiersFor MoaiStatues where
  getModifiersFor (MoaiStatues a) = do
    modifySelfWhenM
      a
      ( selectAny
          $ mapOneOf
            assetIs
            [Assets.roaldEllsworthIntrepidExplorer, Assets.roaldEllsworthIntrepidExplorerResolute]
          <> at_ (be a)
      )
      [ShroudModifier (-2)]

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfAnAlienTranslation]

instance HasAbilities MoaiStatues where
  getAbilities (MoaiStatues a) =
    extendRevealed a [mirage a 2 mirageCards]

instance RunMessage MoaiStatues where
  runMessage msg (MoaiStatues attrs) = runQueueT $ case msg of
    _ -> MoaiStatues <$> mirageRunner Stories.moaiStatues mirageCards 2 msg attrs
