module Arkham.Treachery.Cards.TheZealotsSeal where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discard
import Arkham.Game.Helpers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheZealotsSeal = TheZealotsSeal TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theZealotsSeal :: TreacheryCard TheZealotsSeal
theZealotsSeal = treachery TheZealotsSeal Cards.theZealotsSeal

instance RunMessage TheZealotsSeal where
  runMessage msg t@(TheZealotsSeal attrs@TreacheryAttrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      -- we must unshift this first for other effects happen before
      for_ investigatorIds $ \iid' -> do
        handCardCount <- fieldMap InvestigatorHand length iid'
        push $ if handCardCount <= 3
          then InvestigatorAssignDamage iid' (toSource attrs) DamageAny 1 1
          else RevelationSkillTest iid' source SkillWillpower 2
      pure t
    FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ _
      | tid == treacheryId -> do
        pushAll
          [ toMessage $ randomDiscard iid attrs
          , toMessage $ randomDiscard iid attrs
          ]
        pure t
    _ -> TheZealotsSeal <$> runMessage msg attrs
