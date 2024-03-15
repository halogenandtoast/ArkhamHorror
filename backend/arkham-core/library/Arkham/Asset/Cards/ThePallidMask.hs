module Arkham.Asset.Cards.ThePallidMask (thePallidMask, ThePallidMask (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (PlayCard)
import Arkham.Prelude
import Arkham.Window (defaultWindows)

newtype ThePallidMask = ThePallidMask AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePallidMask :: AssetCard ThePallidMask
thePallidMask = assetWith ThePallidMask Cards.thePallidMask (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor ThePallidMask where
  getModifiersFor (InvestigatorTarget iid) (ThePallidMask a) | controlledBy a iid = do
    pure $ toModifiers a [SanityModifier (-2)]
  getModifiersFor _ _ = pure []

instance RunMessage ThePallidMask where
  runMessage msg a@(ThePallidMask attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      theManInThePallidMask <- selectJust (enemyIs Enemies.theManInThePallidMask)
      hasturTheTatteredKing <- getSetAsideCard Enemies.hasturTheTatteredKing
      palaceOfTheKing <- getJustLocationByName "Palace of the King"
      createHasturTheTatteredKing <- createEnemyAt_ hasturTheTatteredKing palaceOfTheKing Nothing
      pushAll
        [ PlayCard iid (toCard attrs) Nothing NoPayment (defaultWindows iid) False
        , RemoveEnemy theManInThePallidMask
        , createHasturTheTatteredKing
        ]
      pure a
    _ -> ThePallidMask <$> runMessage msg attrs
