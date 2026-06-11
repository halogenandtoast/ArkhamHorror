module Arkham.Agenda.Cards.TimeMarchesOn (timeMarchesOn) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (AssetDefeated)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Scientist))
import Arkham.Window qualified as Window

newtype TimeMarchesOn = TimeMarchesOn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeMarchesOn :: AgendaCard TimeMarchesOn
timeMarchesOn = agenda (2, A) TimeMarchesOn Cards.timeMarchesOn (Static 9)

instance HasAbilities TimeMarchesOn where
  getAbilities (TimeMarchesOn a) =
    [mkAbility a 1 $ forced $ AssetDefeated #when ByAny (AssetWithTrait Scientist)]

getDefeatedAsset :: [Window.Window] -> Maybe AssetId
getDefeatedAsset [] = Nothing
getDefeatedAsset ((Window.windowType -> Window.AssetDefeated aid _) : _) = Just aid
getDefeatedAsset (_ : xs) = getDefeatedAsset xs

instance RunMessage TimeMarchesOn where
  runMessage msg a@(TimeMarchesOn attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      cancelWindowBatch ws
      for_ (getDefeatedAsset ws) \aid -> do
        isEdwin <- aid <=~> assetIs Assets.edwinBennetAstuteAssociate
        uneasyAllianceInPlay <- selectAny $ storyIs Stories.uneasyAlliance
        if isEdwin && uneasyAllianceInPlay
          then do
            healAllDamageAndHorror attrs aid
            placeDoom attrs attrs 2
            push AdvanceAgendaIfThresholdSatisfied
          else abductById aid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      investigators <- getInvestigators
      for_ investigators \iid -> do
        push $ SufferTrauma iid 0 1
        push $ InvestigatorDefeated (toSource attrs) iid
      pure a
    _ -> TimeMarchesOn <$> liftRunMessage msg attrs
