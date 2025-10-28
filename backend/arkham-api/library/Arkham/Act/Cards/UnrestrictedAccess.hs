module Arkham.Act.Cards.UnrestrictedAccess (unrestrictedAccess) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype UnrestrictedAccess = UnrestrictedAccess ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unrestrictedAccess :: ActCard UnrestrictedAccess
unrestrictedAccess = act (2, A) UnrestrictedAccess Cards.unrestrictedAccess Nothing

instance HasAbilities UnrestrictedAccess where
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
          , Remembered ReadAboutEarth
          , Remembered SawAFamiliarSpecimen
          ]
      )
      $ Objective
      $ forced AnyWindow

instance RunMessage UnrestrictedAccess where
  runMessage msg a@(UnrestrictedAccess attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      card <- flipCard <$> genCard (toCardDef attrs)
      push $ PlaceNextTo ActDeckTarget [card]
      advanceActDeck attrs
      pure a
    _ -> UnrestrictedAccess <$> liftRunMessage msg attrs
