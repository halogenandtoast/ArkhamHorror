module Arkham.Act.Cards.TheParisianConspiracyV2 (
  TheParisianConspiracyV2 (..),
  theParisianConspiracyV2,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher

newtype TheParisianConspiracyV2 = TheParisianConspiracyV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theParisianConspiracyV2 :: ActCard TheParisianConspiracyV2
theParisianConspiracyV2 =
  act (1, A) TheParisianConspiracyV2 Cards.theParisianConspiracyV2 $ groupClueCost (PerPlayer 2)

instance HasAbilities TheParisianConspiracyV2 where
  getAbilities (TheParisianConspiracyV2 a) =
    extend1 a $ restricted a 1 (DoomCountIs $ atLeast 3) (Objective $ forced $ RoundEnds #when)

instance RunMessage TheParisianConspiracyV2 where
  runMessage msg a@(TheParisianConspiracyV2 attrs) = case msg of
    AdvanceAct aid _ advanceMode | aid == actId attrs && onSide B attrs -> do
      theOrganist <-
        fromJustNote "The Organist was not set aside"
          . listToMaybe
          <$> getSetAsideCardsMatching (CardWithTitle "The Organist")
      case advanceMode of
        AdvancedWithClues -> do
          locationIds <- select $ FarthestLocationFromAll Anywhere
          lead <- getLeadPlayer
          choices <- for locationIds $ \lid -> do
            createTheOrganist <- createEnemyAt_ theOrganist lid Nothing
            pure $ targetLabel lid [createTheOrganist]

          pushAll
            [ chooseOrRunOne lead choices
            , AdvanceActDeck (actDeckId attrs) (toSource attrs)
            ]
        _ -> do
          investigatorIds <- getInvestigators
          locationId <- selectJust LeadInvestigatorLocation
          createTheOrganist <- createEnemyAt_ theOrganist locationId Nothing
          pushAll
            $ [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
              | iid <- investigatorIds
              ]
            <> [ createTheOrganist
               , AdvanceActDeck (actDeckId attrs) (toSource attrs)
               ]
      pure a
    _ -> TheParisianConspiracyV2 <$> runMessage msg attrs
