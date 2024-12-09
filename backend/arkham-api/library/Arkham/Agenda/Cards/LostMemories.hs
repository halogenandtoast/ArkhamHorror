module Arkham.Agenda.Cards.LostMemories (LostMemories (..), lostMemories, lostMemoriesEffect) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype LostMemories = LostMemories AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: AgendaCard LostMemories
lostMemories = agenda (2, A) LostMemories Cards.lostMemories (Static 7)

instance HasModifiersFor LostMemories where
  getModifiersFor (LostMemories attrs) =
    if onSide A attrs
      then modifySelect attrs Anyone [HandSize (-2)]
      else pure mempty

instance RunMessage LostMemories where
  runMessage msg a@(LostMemories attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      hasPendant <- getInvestigatorsWithSupply Pendant
      shouldMoveCustodian <- selectAny $ assetIs Assets.theCustodian <> UncontrolledAsset

      custodianMessages <-
        if shouldMoveCustodian
          then do
            lead <- getLeadPlayer
            custodian <- selectJust $ assetIs Assets.theCustodian
            locationWithMostClues <- select $ LocationWithMostClues Anywhere
            pure
              [ chooseOrRunOne
                  lead
                  [ targetLabel lid [PlaceAsset custodian $ AtLocation lid]
                  | lid <- locationWithMostClues
                  ]
              ]
          else pure []
      let drawing = map (\iid -> drawCards iid attrs 2) hasPendant
      enabled <- createCardEffect Cards.lostMemories Nothing (toSource attrs) ScenarioTarget
      pushAll
        $ ShuffleEncounterDiscardBackIn
        : custodianMessages
          <> drawing
          <> [ enabled
             , advanceAgendaDeck attrs
             ]
      pure a
    _ -> LostMemories <$> runMessage msg attrs

newtype LostMemoriesEffect = LostMemoriesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemoriesEffect :: EffectArgs -> LostMemoriesEffect
lostMemoriesEffect = cardEffect LostMemoriesEffect Cards.lostMemories

instance HasModifiersFor LostMemoriesEffect where
  getModifiersFor (LostMemoriesEffect a) =
    modifySelect a (InvestigatorWithSupply Pendant) [IgnoreHandSizeReduction]

instance RunMessage LostMemoriesEffect where
  runMessage msg (LostMemoriesEffect attrs) = LostMemoriesEffect <$> runMessage msg attrs
