module Arkham.Act.Cards.MistakesOfThePast (mistakesOfThePast) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardCode
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario (getIsReturnTo)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MistakesOfThePast = MistakesOfThePast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mistakesOfThePast :: ActCard MistakesOfThePast
mistakesOfThePast = act (2, A) MistakesOfThePast Cards.mistakesOfThePast (groupClueCost (PerPlayer 2))

instance RunMessage MistakesOfThePast where
  runMessage msg a@(MistakesOfThePast attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      playerCount <- getPlayerCount
      selectEach (RevealedLocation <> "Historical Society") \location ->
        push $ PlaceCluesUpToClueValue location (toSource attrs) playerCount
      mrPeabody <- getSetAsideCard Assets.mrPeabody
      investigators <- getInvestigators
      leadChooseOneM $ targets investigators (`takeControlOfSetAsideAsset` mrPeabody)
      placeSetAsideLocation_ Locations.hiddenLibrary
      advanceActDeck attrs
      whenM getIsReturnTo do
        ok <- selectAny $ EmptyLocation <> "Historical Society"
        when ok do
          lead <- getLead
          leadChooseOneM do
            abilityLabeled
              lead
              (mkAbility (SourceableWithCardCode (CardCode "52028") ScenarioSource) 1 $ forced AnyWindow)
              nothing
      pure a
    _ -> MistakesOfThePast <$> liftRunMessage msg attrs
