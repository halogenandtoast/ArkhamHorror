module Arkham.Treachery.Cards.TwistedToHisWill
  ( twistedToHisWill
  , TwistedToHisWill(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Query
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype TwistedToHisWill = TwistedToHisWill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedToHisWill :: TreacheryCard TwistedToHisWill
twistedToHisWill = treachery TwistedToHisWill Cards.twistedToHisWill

instance TreacheryRunner env => RunMessage TwistedToHisWill where
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
