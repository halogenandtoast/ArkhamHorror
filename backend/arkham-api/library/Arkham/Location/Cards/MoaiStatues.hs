module Arkham.Location.Cards.MoaiStatues (moaiStatues) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Message hiding (gainSurge)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Import.Lifted (gainSurge)

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
    clearedOfMirages a mirageCards

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfAnAlienTranslation]

instance HasAbilities MoaiStatues where
  getAbilities (MoaiStatues a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , groupLimit PerPhase $ mkAbility a 1 $ forced $ AttachCard #when Nothing #treachery (targetIs a)
      ]

instance RunMessage MoaiStatues where
  runMessage msg l@(MoaiStatues attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (attachedCard -> card) _ -> do
      mTreachery <- select $ TreacheryWithCardId card.id
      for_ mTreachery gainSurge
      pure l
    _ -> MoaiStatues <$> mirageRunner Stories.moaiStatues mirageCards 2 msg attrs
