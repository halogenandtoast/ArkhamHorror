module Arkham.Act.Cards.SearchForTheBrotherhood (searchForTheBrotherhood) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Hex, Shattered))

newtype SearchForTheBrotherhood = SearchForTheBrotherhood ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheBrotherhood :: ActCard SearchForTheBrotherhood
searchForTheBrotherhood =
  act (2, A) SearchForTheBrotherhood Cards.searchForTheBrotherhood Nothing

instance HasAbilities SearchForTheBrotherhood where
  getAbilities (SearchForTheBrotherhood attrs)
    | onSide A attrs =
        [ mkAbility attrs 1
            $ Objective
            $ forced
            $ Enters #after Anyone
            $ locationIs Locations.aPocketInTime
        ]
  getAbilities _ = []

instance RunMessage SearchForTheBrotherhood where
  runMessage msg a@(SearchForTheBrotherhood attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      shuffleSetAsideIntoScenarioDeck ExplorationDeck (CardWithTrait Shattered)
      eachInvestigator (`forInvestigator` msg)
      relicIsMissing <- getHasRecord TheRelicIsMissing
      mRelic <-
        if relicIsMissing
          then Just <$> getSetAsideCard Assets.relicOfAgesUnleashTheTimestream
          else pure Nothing
      aPocketInTime <- selectJust $ locationIs Locations.aPocketInTime
      for_ mRelic (`createAssetAt_` AttachedToLocation aPocketInTime)

      push $ AdvanceToAct (actDeckId attrs) Acts.theYithianRelic A (toSource attrs)
      pure a

    ForInvestigator iid (AdvanceAct (isSide B attrs -> True) _ _) -> do
      (nonMatch, rest) <- break (`cardMatch` CardWithTrait Hex) <$> scenarioField ScenarioDiscard
      case rest of
        [] -> pure ()
        (x : _) ->
          focusCards (map EncounterCard $ nonMatch <> [x]) \unfocus -> do
            chooseOneM iid do
              targeting x $ shuffleCardsIntoDeck ExplorationDeck [EncounterCard x]
            push unfocus
      pure a
    _ -> SearchForTheBrotherhood <$> liftRunMessage msg attrs
