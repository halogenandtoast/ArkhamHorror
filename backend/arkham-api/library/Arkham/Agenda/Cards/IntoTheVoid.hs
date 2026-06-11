module Arkham.Agenda.Cards.IntoTheVoid (intoTheVoid) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Scientist))
import Arkham.Window qualified as Window

newtype IntoTheVoid = IntoTheVoid AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheVoid :: AgendaCard IntoTheVoid
intoTheVoid = agenda (1, A) IntoTheVoid Cards.intoTheVoid (Static 9)

instance HasAbilities IntoTheVoid where
  getAbilities (IntoTheVoid a) =
    [mkAbility a 1 $ forced $ AssetDefeated #when ByAny (AssetWithTrait Scientist)]

getDefeatedAsset :: [Window.Window] -> Maybe AssetId
getDefeatedAsset [] = Nothing
getDefeatedAsset ((Window.windowType -> Window.AssetDefeated aid _) : _) = Just aid
getDefeatedAsset (_ : xs) = getDefeatedAsset xs

instance RunMessage IntoTheVoid where
  runMessage msg a@(IntoTheVoid attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      cancelWindowBatch ws
      for_ (getDefeatedAsset ws) (abductDefeatedScientist attrs)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenM (selectAny $ storyIs Stories.anomaliesInSpacetime) do
        n <- perPlayer 1
        docks <- select $ LocationWithTitle "River Docks"
        leadChooseOrRunOneM $ targets docks \dock -> placeTokens attrs dock #horror n
        doStep 1 msg
      selectForMaybeM (storyIs Stories.mobTroubles) \mobTroubles -> do
        lead <- getLead
        flipOver lead mobTroubles
      selectForMaybeM (storyIs Stories.unspeakableAbomination) \unspeakable -> do
        lead <- getLead
        flipOver lead unspeakable
      selectForMaybeM (storyIs Stories.aBitterRivalry) \bitterRivalry -> do
        lead <- getLead
        flipOver lead bitterRivalry
      advanceAgendaDeck attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      anomalyLocations <- select $ LocationWithHorror (atLeast 1)
      for_ anomalyLocations \lid -> placeTokens attrs lid #horror 1
      pure a
    _ -> IntoTheVoid <$> liftRunMessage msg attrs

abductDefeatedScientist
  :: (ReverseQueue m, Sourceable source) => source -> AssetId -> m ()
abductDefeatedScientist source aid = do
  isEdwin <- aid <=~> assetIs Assets.edwinBennetAstuteAssociate
  uneasyAllianceInPlay <- selectAny $ storyIs Stories.uneasyAlliance
  if isEdwin && uneasyAllianceInPlay
    then do
      healAllDamageAndHorror source aid
      agenda <- selectJust AnyAgenda
      placeDoom source agenda 2
      push AdvanceAgendaIfThresholdSatisfied
    else abductById aid
