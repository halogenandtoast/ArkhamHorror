module Arkham.Act.Cards.StrangeRelicsMariasInformation (
  StrangeRelicsMariasInformation (..),
  strangeRelicsMariasInformation,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Name
import Arkham.ScenarioLogKey

newtype StrangeRelicsMariasInformation = StrangeRelicsMariasInformation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeRelicsMariasInformation :: ActCard StrangeRelicsMariasInformation
strangeRelicsMariasInformation =
  act
    (2, E)
    StrangeRelicsMariasInformation
    Cards.strangeRelicsMariasInformation
    Nothing

instance HasAbilities StrangeRelicsMariasInformation where
  getAbilities (StrangeRelicsMariasInformation a) =
    [ restrictedAbility
      a
      1
      ( AssetExists
          $ assetIs Assets.mariaDeSilva
          <> AssetWithClues
            (AtLeast $ PerPlayer 1)
      )
      $ Objective
      $ ForcedAbility AnyWindow
    | onSide E a
    ]

instance RunMessage StrangeRelicsMariasInformation where
  runMessage msg a@(StrangeRelicsMariasInformation attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      downtown <- selectJust $ locationIs Locations.downtownFirstBankOfArkham
      rivertown <- selectJust $ locationIs Locations.rivertown
      iids <- getInvestigatorIds
      pushAll
        $ [ Remember $ IchtacasDestination (Labeled (toName Locations.downtownFirstBankOfArkham) downtown)
          , Remember $ IchtacasDestination (Labeled (toName Locations.rivertown) rivertown)
          ]
        <> [ DiscardTopOfEncounterDeck
            iid
            1
            (toSource attrs)
            (Just $ toTarget attrs)
           | iid <- iids
           ]
        <> [ AdvanceToAct
              (actDeckId attrs)
              Acts.strangeOccurences
              E
              (toSource attrs)
           ]
      pure a
    DiscardedTopOfEncounterDeck iid [card] _ target | isTarget attrs target ->
      do
        when (toCardType card == TreacheryType) $ do
          push $ InvestigatorDrewEncounterCard iid card
        pure a
    _ -> StrangeRelicsMariasInformation <$> runMessage msg attrs
