module Arkham.Treachery.Cards.TheFinalAct (
  theFinalAct,
  TheFinalAct (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheFinalAct = TheFinalAct TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theFinalAct :: TreacheryCard TheFinalAct
theFinalAct = treachery TheFinalAct Cards.theFinalAct

instance RunMessage TheFinalAct where
  runMessage msg t@(TheFinalAct attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      agenda <- selectJust AnyAgenda
      noRemainingSanity <- fieldP InvestigatorRemainingSanity (== 0) iid
      when noRemainingSanity
        $ pushAll
          [PlaceDoom (toSource attrs) (AgendaTarget agenda) 2, AdvanceAgendaIfThresholdSatisfied]
      pure t
    _ -> TheFinalAct <$> runMessage msg attrs
