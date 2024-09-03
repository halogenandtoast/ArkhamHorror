module Arkham.Treachery.Cards.TwistedToHisWill (twistedToHisWill, TwistedToHisWill (..)) where

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TwistedToHisWill = TwistedToHisWill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedToHisWill :: TreacheryCard TwistedToHisWill
twistedToHisWill = treachery TwistedToHisWill Cards.twistedToHisWill

instance RunMessage TwistedToHisWill where
  runMessage msg t@(TwistedToHisWill attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      doomCount <- getDoomCount
      sid <- getRandom
      push
        $ if doomCount > 0
          then revelationSkillTest sid iid attrs #willpower DoomCountCalculation
          else gainSurge attrs
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      pushAll
        [ toMessage $ randomDiscard iid attrs
        , toMessage $ randomDiscard iid attrs
        ]
      pure t
    _ -> TwistedToHisWill <$> runMessage msg attrs
