module Arkham.Treachery.Cards.TheZealotsSeal where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheZealotsSeal = TheZealotsSeal TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theZealotsSeal :: TreacheryCard TheZealotsSeal
theZealotsSeal = treachery TheZealotsSeal Cards.theZealotsSeal

instance RunMessage TheZealotsSeal where
  runMessage msg t@(TheZealotsSeal attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      -- we must unshift this first for other effects happen before
      for_ investigatorIds $ \iid' -> do
        handCardCount <- fieldMap InvestigatorHand length iid'
        push
          $ if handCardCount <= 3
            then assignDamageAndHorror iid' attrs 1 1
            else revelationSkillTest iid' source #willpower 2
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      pushAll
        [ toMessage $ randomDiscard iid attrs
        , toMessage $ randomDiscard iid attrs
        ]
      pure t
    _ -> TheZealotsSeal <$> runMessage msg attrs
