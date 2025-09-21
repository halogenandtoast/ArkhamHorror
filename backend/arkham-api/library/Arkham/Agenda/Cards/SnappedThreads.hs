module Arkham.Agenda.Cards.SnappedThreads (snappedThreads) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Matcher

newtype SnappedThreads = SnappedThreads AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snappedThreads :: AgendaCard SnappedThreads
snappedThreads = agenda (3, A) SnappedThreads Cards.snappedThreads (Static 12)

instance HasAbilities SnappedThreads where
  getAbilities (SnappedThreads a) =
    [ mkAbility a 1
        $ forced
        $ InvestigatorEliminated #when
        $ oneOf
          [ HandWith (HasCard "Relic of Ages")
          , DiscardWith (HasCard "Relic of Ages")
          , DeckWith (HasCard "Relic of Ages")
          , HasMatchingAsset "Relic of Ages"
          ]
    ]

instance RunMessage SnappedThreads where
  runMessage msg a@(SnappedThreads attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R4
      pure a
    _ -> SnappedThreads <$> liftRunMessage msg attrs
