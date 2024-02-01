module Arkham.Act.Cards.SearchForTheBrotherhood (
  SearchForTheBrotherhood (..),
  searchForTheBrotherhood,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Hex, Shattered))

newtype SearchForTheBrotherhood = SearchForTheBrotherhood ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

searchForTheBrotherhood :: ActCard SearchForTheBrotherhood
searchForTheBrotherhood =
  act (2, A) SearchForTheBrotherhood Cards.searchForTheBrotherhood Nothing

instance HasAbilities SearchForTheBrotherhood where
  getAbilities (SearchForTheBrotherhood attrs)
    | onSide A attrs =
        [ mkAbility attrs 1
            $ Objective
            $ ForcedAbility
            $ Enters Timing.After Anyone
            $ locationIs Locations.aPocketInTime
        ]
  getAbilities _ = []

instance RunMessage SearchForTheBrotherhood where
  runMessage msg a@(SearchForTheBrotherhood attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      shattered <- getSetAsideCardsMatching $ CardWithTrait Shattered
      iids <- getInvestigatorIds
      relicIsMissing <- getHasRecord TheRelicIsMissing
      mRelic <-
        if relicIsMissing
          then Just <$> getSetAsideCard Assets.relicOfAgesUnleashTheTimestream
          else pure Nothing
      aPocketInTime <- selectJust $ locationIs Locations.aPocketInTime
      assetId <- getRandom
      pushAll
        $ [ShuffleCardsIntoDeck (ScenarioDeckByKey ExplorationDeck) shattered]
        <> [NextAdvanceActStep (toId attrs) idx | (idx, _) <- zip [1 ..] iids]
        <> [ CreateAssetAt assetId relic $ AttachedToLocation aPocketInTime
           | relic <- maybeToList mRelic
           ]
        <> [ AdvanceToAct
              (actDeckId attrs)
              Acts.theYithianRelic
              A
              (toSource attrs)
           ]
      pure a
    NextAdvanceActStep aid idx | aid == toId attrs -> do
      iids <- getInvestigatorIds
      let miid = iids !!? idx
      for_ miid $ \iid -> do
        discard <- scenarioField ScenarioDiscard
        player <- getPlayer iid
        let (nonMatch, rest) = break (`cardMatch` CardWithTrait Hex) discard
        case rest of
          [] -> pure ()
          (x : _) ->
            pushAll
              [ FocusCards (map EncounterCard $ nonMatch <> [x])
              , chooseOne
                  player
                  [ targetLabel
                      (toCardId x)
                      [ShuffleCardsIntoDeck (ScenarioDeckByKey ExplorationDeck) [EncounterCard x]]
                  ]
              , UnfocusCards
              ]
      pure a
    _ -> SearchForTheBrotherhood <$> runMessage msg attrs
