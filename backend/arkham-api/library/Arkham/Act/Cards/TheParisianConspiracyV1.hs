module Arkham.Act.Cards.TheParisianConspiracyV1 (TheParisianConspiracyV1 (..), theParisianConspiracyV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude

newtype TheParisianConspiracyV1 = TheParisianConspiracyV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theParisianConspiracyV1 :: ActCard TheParisianConspiracyV1
theParisianConspiracyV1 =
  act (1, A) TheParisianConspiracyV1 Cards.theParisianConspiracyV1 $ groupClueCost (PerPlayer 2)

instance HasAbilities TheParisianConspiracyV1 where
  getAbilities (TheParisianConspiracyV1 a) =
    extend1 a $ restricted a 1 (DoomCountIs $ atLeast 3) (Objective $ forced $ RoundEnds #when)

instance RunMessage TheParisianConspiracyV1 where
  runMessage msg a@(TheParisianConspiracyV1 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct (isSide B attrs -> True) _ advanceMode -> do
      theOrganist <-
        fromJustNote "The Organist was not set aside"
          . listToMaybe
          <$> getSetAsideCardsMatching "The Organist"
      case advanceMode of
        AdvancedWithClues -> do
          locationId <- selectJust LeadInvestigatorLocation
          createTheOrganist <- createEnemyAt_ theOrganist locationId Nothing
          pushAll
            [ createTheOrganist
            , advanceActDeck attrs
            ]
        _ -> do
          investigatorIds <- getInvestigators
          locationIds <- select $ FarthestLocationFromAll Anywhere
          lead <- getLeadPlayer

          choices <- for locationIds $ \lid -> do
            createTheOrganist <- createEnemyAt_ theOrganist lid Nothing
            pure $ targetLabel lid [createTheOrganist]

          pushAll
            $ [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
              | iid <- investigatorIds
              ]
            <> [ chooseOrRunOne lead choices
               , advanceActDeck attrs
               ]
      pure a
    _ -> TheParisianConspiracyV1 <$> runMessage msg attrs
