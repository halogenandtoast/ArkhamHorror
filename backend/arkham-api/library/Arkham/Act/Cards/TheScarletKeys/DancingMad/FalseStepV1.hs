module Arkham.Act.Cards.TheScarletKeys.DancingMad.FalseStepV1 (falseStepV1) where

import Arkham.Ability
import Arkham.Act.CardDefs.TheScarletKeys.DancingMad qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Effect.Window (EffectWindow (EffectHollowWindow))
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.TheScarletKeys.DancingMad qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Message (resolve)
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Coterie))
import Arkham.Window qualified as Window

newtype FalseStepV1 = FalseStepV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseStepV1 :: ActCard FalseStepV1
falseStepV1 = act (1, A) FalseStepV1 Cards.falseStepV1 (groupClueCost (PerPlayer 3))

instance HasAbilities FalseStepV1 where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ ActionAbility #resign Nothing (ActionCost 1)
    , mkAbility a 2 $ forced $ AssetWouldLeavePlay #when (assetIs Assets.desiderioDelgadoAlvarez)
    ]

instance RunMessage FalseStepV1 where
  runMessage msg a@(FalseStepV1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 ws@(assetLeavingPlay -> aid) _ -> do
      cancelWindowBatch ws
      card <- fetchCard aid
      -- Desiderio is set aside out of play "instead" of leaving play normally, so
      -- a Body Snatched hollow that triggered this window must not tag him as a
      -- hollow: drop the queued Hollow/"hollowed" modifier effects and the
      -- "hollowed" campaign-event window before setting him aside plainly. The
      -- hollow's own set aside goes with them, since this ability does that part
      -- itself and two of them would stack a second copy in the pile.
      let isHollowedWindow = \case
            (Window.windowType -> Window.CampaignEvent "hollowed" _ _) -> True
            _ -> False
      allMatchingDon't \case
        CreateWindowModifierEffect (EffectHollowWindow cid) _ _ _ -> cid == card.id
        SetAsideCards cards -> card `elem` cards
        CheckWindows ws' -> any isHollowedWindow ws'
        Do (CheckWindows ws') -> any isHollowedWindow ws'
        _ -> False
      -- The cancelled batch took the asset's 'RemovedFromPlay' with it, so without
      -- this Desiderio sits in the play area and in the set aside pile at once
      -- (#6041). Quietly: the leave-play windows have already been raised by
      -- whatever was removing him, and a second 'RemoveFromPlay' would retrigger
      -- this very ability.
      pushAll $ RemovedFromPlay (toSource aid) : resolve (RemoveAsset aid)
      setCardAside card
      eachInvestigator \iid -> assignHorror iid (attrs.ability 2) 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (assetIs Assets.desiderioDelgadoAlvarez) \aid -> do
        card <- fetchCard aid
        removeFromGame aid
        setCardAside card
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
