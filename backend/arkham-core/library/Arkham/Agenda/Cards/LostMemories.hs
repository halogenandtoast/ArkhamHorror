module Arkham.Agenda.Cards.LostMemories (
  LostMemories (..),
  lostMemories,
  lostMemoriesEffect,
) where

import Arkham.Prelude

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

newtype LostMemories = LostMemories AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: AgendaCard LostMemories
lostMemories = agenda (2, A) LostMemories Cards.lostMemories (Static 7)

instance HasModifiersFor LostMemories where
  getModifiersFor (InvestigatorTarget _) (LostMemories attrs)
    | onSide A attrs =
        pure $ toModifiers attrs [HandSize (-2)]
  getModifiersFor _ _ = pure []

instance RunMessage LostMemories where
  runMessage msg a@(LostMemories attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      hasPendant <- getInvestigatorsWithSupply Pendant
      shouldMoveCustodian <-
        selectAny $ assetIs Assets.theCustodian <> UncontrolledAsset

      custodianMessages <-
        if shouldMoveCustodian
          then do
            lead <- getLeadInvestigatorId
            custodian <- selectJust $ assetIs Assets.theCustodian
            locationWithMostClues <- selectList $ LocationWithMostClues Anywhere
            pure
              $ [ chooseOrRunOne
                    lead
                    [ targetLabel lid [PlaceAsset custodian $ AtLocation lid]
                    | lid <- locationWithMostClues
                    ]
                ]
          else pure []
      drawing <- for hasPendant $ \iid -> drawCards iid attrs 2
      pushAll
        $ ShuffleEncounterDiscardBackIn
        : custodianMessages
          <> drawing
          <> [ createCardEffect
                Cards.lostMemories
                Nothing
                (toSource attrs)
                ScenarioTarget
             , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
             ]
      pure a
    _ -> LostMemories <$> runMessage msg attrs

newtype LostMemoriesEffect = LostMemoriesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemoriesEffect :: EffectArgs -> LostMemoriesEffect
lostMemoriesEffect = cardEffect LostMemoriesEffect Cards.lostMemories

instance HasModifiersFor LostMemoriesEffect where
  getModifiersFor (InvestigatorTarget iid) (LostMemoriesEffect a) = do
    hasPendant <- getHasSupply iid Pendant
    pure $ toModifiers a [IgnoreHandSizeReduction | hasPendant]
  getModifiersFor _ _ = pure []

instance RunMessage LostMemoriesEffect where
  runMessage msg (LostMemoriesEffect attrs) =
    LostMemoriesEffect <$> runMessage msg attrs
