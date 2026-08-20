module Arkham.Act.Cards.TheCircleUndone.ForTheGreaterGood.WarmWelcome (warmWelcome) where

import Arkham.Act.CardDefs.TheCircleUndone.ForTheGreaterGood qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.TheCircleUndone.ForTheGreaterGood qualified as Enemies
import Arkham.Location.CardDefs.TheCircleUndone.ForTheGreaterGood qualified as Locations
import Arkham.Matcher
import Arkham.Placement

newtype WarmWelcome = WarmWelcome ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

warmWelcome :: ActCard WarmWelcome
warmWelcome = act (1, A) WarmWelcome Cards.warmWelcome (groupClueCost (PerPlayer 3))

instance RunMessage WarmWelcome where
  runMessage msg a@(WarmWelcome attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      library <-
        selectOne (locationIs Locations.library) >>= \case
          Just library -> pure library
          Nothing -> placeSetAsideLocation Locations.library
      nathanWickCard <- fetchCard Enemies.nathanWickMasterOfInitiation
      nathanWick <- createEnemyAt nathanWickCard library
      puzzleBox <- fetchCard Assets.puzzleBox
      createAssetAt_ puzzleBox (AttachedToEnemy nathanWick)
      advanceActDeck attrs
      pure a
    _ -> WarmWelcome <$> liftRunMessage msg attrs
