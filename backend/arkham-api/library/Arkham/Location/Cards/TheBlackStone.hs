module Arkham.Location.Cards.TheBlackStone (theBlackStone) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype TheBlackStone = TheBlackStone LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackStone :: LocationCard TheBlackStone
theBlackStone = location TheBlackStone Cards.theBlackStone 4 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfATerribleDiscovery]

instance HasModifiersFor TheBlackStone where
  getModifiersFor (TheBlackStone a) = do
    modifySelfWhenM
      a
      ( selectAny
          $ mapOneOf
            assetIs
            [ Assets.averyClaypoolAntarcticGuide
            , Assets.averyClaypoolAntarcticGuideResolute
            ]
          <> at_ (be a)
      )
      [ShroudModifier (-2)]
    clearedOfMirages a mirageCards

instance HasAbilities TheBlackStone where
  getAbilities (TheBlackStone a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , groupLimit PerGame
          $ restricted a 1 (Here <> thisExists a LocationWithAnyClues)
          $ FastAbility (AddFrostTokenCost 1)
      ]

instance RunMessage TheBlackStone where
  runMessage msg l@(TheBlackStone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) attrs 2
      pure l
    _ -> TheBlackStone <$> mirageRunner Stories.theBlackStone mirageCards 2 msg attrs
