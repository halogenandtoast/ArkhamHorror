module Arkham.Act.Cards.OnTheTrail (onTheTrail) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.ForMovement
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Dark))

newtype OnTheTrail = OnTheTrail ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheTrail :: ActCard OnTheTrail
onTheTrail = act (2, A) OnTheTrail Cards.onTheTrail Nothing

instance HasAbilities OnTheTrail where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
    , mkAbility a 2
        $ Objective
        $ forced
        $ Enters #after Anyone
        $ locationIs Locations.fungalCave
    ]

instance RunMessage OnTheTrail where
  runMessage msg a@(OnTheTrail attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (ConnectedFrom NotForMovement (locationWithInvestigator iid) <> LocationWithTrait Dark)
      chooseTargetM iid enemies \eid -> do
        enemyMoveTo attrs eid =<< selectJust (locationWithInvestigator iid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      fungalCave <- selectJust $ locationIs Locations.fungalCave
      createSetAsideEnemy_ Enemies.limulusHybridInTheLight fungalCave
      selectForMaybeM (assetIs Assets.helenPetersTheEldestSister) removeFromGame
      selectForMaybeM (ControlsAsset (assetIs Assets.theoPetersJackOfAllTrades)) \iid ->
        codex iid attrs Sigma
      advanceActDeck attrs
      pure a
    _ -> OnTheTrail <$> liftRunMessage msg attrs
