module Arkham.Treachery.Cards.WorldsMerge (
  worldsMerge,
  WorldsMerge (..),
) where

import Arkham.Prelude

import Arkham.Agenda.Sequence qualified as AS
import Arkham.Agenda.Types (Field (AgendaSequence))
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WorldsMerge = WorldsMerge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

worldsMerge :: TreacheryCard WorldsMerge
worldsMerge = treachery WorldsMerge Cards.worldsMerge

getStep :: HasGame m => Maybe AgendaId -> m Int
getStep Nothing = pure 3 -- if no agenda than act is 3
getStep (Just agenda) = do
  side <- fieldMap AgendaSequence AS.agendaStep agenda
  case side of
    AS.AgendaStep step -> pure step

instance RunMessage WorldsMerge where
  runMessage msg t@(WorldsMerge attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ RevelationSkillTest iid source SkillWillpower (SkillTestDifficulty $ Fixed 3)
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          n <- getStep =<< selectOne (AgendaWithSide AS.C)
          pushAll
            $ InvestigatorAssignDamage iid source DamageAny 0 n
            : replicate
              n
              (toMessage $ chooseAndDiscardCard iid attrs)
          pure t
    _ -> WorldsMerge <$> runMessage msg attrs
