module Arkham.Types.Treachery.Cards.TwistedToHisWill
  ( twistedToHisWill
  , TwistedToHisWill(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TwistedToHisWill = TwistedToHisWill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedToHisWill :: TreacheryCard TwistedToHisWill
twistedToHisWill = treachery TwistedToHisWill Cards.twistedToHisWill

instance TreacheryRunner env => RunMessage env TwistedToHisWill where
  runMessage msg t@(TwistedToHisWill attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      doomCount <- unDoomCount <$> getCount ()
      t <$ if doomCount > 0
        then push (RevelationSkillTest iid source SkillWillpower doomCount)
        else push (Surge iid source)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t
      <$ pushAll [RandomDiscard iid, RandomDiscard iid]
    _ -> TwistedToHisWill <$> runMessage msg attrs
