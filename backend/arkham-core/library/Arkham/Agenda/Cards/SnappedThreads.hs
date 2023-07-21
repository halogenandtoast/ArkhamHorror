module Arkham.Agenda.Cards.SnappedThreads (
  SnappedThreads (..),
  snappedThreads,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message hiding (InvestigatorEliminated)
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype SnappedThreads = SnappedThreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snappedThreads :: AgendaCard SnappedThreads
snappedThreads = agenda (3, A) SnappedThreads Cards.snappedThreads (Static 12)

instance HasAbilities SnappedThreads where
  getAbilities (SnappedThreads a) =
    [ mkAbility a 1 $
        ForcedAbility $
          InvestigatorEliminated Timing.When $
            AnyInvestigator
              [ HandWith (HasCard $ CardWithTitle "Relic of Ages")
              , DiscardWith (HasCard $ CardWithTitle "Relic of Ages")
              , DeckWith (HasCard $ CardWithTitle "Relic of Ages")
              , HasMatchingAsset (AssetWithTitle "Relic of Ages")
              ]
    ]

instance RunMessage SnappedThreads where
  runMessage msg a@(SnappedThreads attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAgenda (toId attrs)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 4
      pure a
    _ -> SnappedThreads <$> runMessage msg attrs
