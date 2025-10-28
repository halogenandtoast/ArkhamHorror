module Arkham.Act.Cards.RestrictedAccess (restrictedAccess) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype RestrictedAccess = RestrictedAccess ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restrictedAccess :: ActCard RestrictedAccess
restrictedAccess = act (2, A) RestrictedAccess Cards.restrictedAccess Nothing

instance HasAbilities RestrictedAccess where
  getAbilities = actAbilities1 \a ->
    restricted
      a
      1
      ( AtLeastNCriteriaMet
          3
          [ AssetExists (assetIs Assets.theCustodian <> ControlledAsset)
          , Remembered FoundTheProcess
          , Remembered DissectedAnOrgan
          , Remembered InterviewedASubject
          , Remembered RealizedWhatYearItIs
          , Remembered ActivatedTheDevice
          ]
      )
      $ Objective
      $ forced AnyWindow

instance RunMessage RestrictedAccess where
  runMessage msg a@(RestrictedAccess attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      addToVictory (toTarget attrs)
      advanceActDeck attrs
      pure a
    _ -> RestrictedAccess <$> liftRunMessage msg attrs
