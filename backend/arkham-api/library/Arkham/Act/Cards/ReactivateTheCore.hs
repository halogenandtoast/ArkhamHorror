module Arkham.Act.Cards.ReactivateTheCore (reactivateTheCore) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype ReactivateTheCore = ReactivateTheCore ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reactivateTheCore :: ActCard ReactivateTheCore
reactivateTheCore = act (1, A) ReactivateTheCore Cards.reactivateTheCore Nothing

instance HasModifiersFor ReactivateTheCore where
  getModifiersFor (ReactivateTheCore a) = do
    modifySelectMapM a Anywhere \loc -> do
      connections <- runDefaultMaybeT [] do
        pos <- MaybeT $ field LocationPosition loc
        select $ mapOneOf LocationInPosition (adjacentPositions pos)
      pure
        [ ConnectedToWhen (LocationWithId loc) (mapOneOf LocationWithId connections)
        | notNull connections
        ]

instance HasAbilities ReactivateTheCore where
  getAbilities (ReactivateTheCore a) =
    extend
      a
      [ restricted a 1 (youExist $ at_ FloodedLocation)
          $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
      , restricted
          a
          2
          (EachUndefeatedInvestigator (at_ $ LocationWithTitle "Barrier Core" <> locationIs Locations.barrierCoreActive))
          $ Objective
          $ forced (RoundEnds #when)
      ]

instance RunMessage ReactivateTheCore where
  runMessage msg a@(ReactivateTheCore attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> push $ DecreaseFloodLevel lid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide A attrs -> True) _ _ -> do
      controlsBarrierNode <- selectAny $ assetIs Assets.barrierNode <> AssetControlledBy Anyone
      push $ if controlsBarrierNode then R1 else R2
      pure a
    _ -> ReactivateTheCore <$> liftRunMessage msg attrs
