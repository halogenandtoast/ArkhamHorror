module Arkham.Act.Cards.FalseStepV1 (falseStepV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Coterie))

newtype FalseStepV1 = FalseStepV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseStepV1 :: ActCard FalseStepV1
falseStepV1 = act (1, A) FalseStepV1 Cards.falseStepV1 (groupClueCost (PerPlayer 3))

instance HasAbilities FalseStepV1 where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ ActionAbility [#resign] (ActionCost 1)
    , mkAbility a 2 $ forced $ AssetWouldLeavePlay #when (assetIs Assets.desiderioDelgadoAlvarez)
    ]

instance RunMessage FalseStepV1 where
  runMessage msg a@(FalseStepV1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 ws@(assetLeavingPlay -> aid) _ -> do
      cancelWindowBatch ws
      setCardAside =<< fetchCard aid
      eachInvestigator \iid -> assignHorror iid (attrs.ability 2) 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (assetIs Assets.desiderioDelgadoAlvarez) (fetchCard >=> setCardAside)
      desi <- fetchCard Enemies.desiderioDelgadoAlvarez106
      lead <- getLead
      drawCard lead desi
      eachInvestigator \iid -> do
        unless (iid == lead) $ forInvestigator iid msg
      doStep 1 msg
      advanceActDeck attrs
      pure a
    ForInvestigator iid (AdvanceAct (isSide B attrs -> True) _ _) -> do
      cards <- getSetAsideCardsMatching (#enemy <> CardWithTrait Coterie)
      focusCards cards do
        chooseTargetM iid cards $ drawCard iid
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.CrimsonConspiracy
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.CleanupCrew
      pure a
    _ -> FalseStepV1 <$> liftRunMessage msg attrs
