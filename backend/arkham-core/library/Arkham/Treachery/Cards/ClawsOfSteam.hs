module Arkham.Treachery.Cards.ClawsOfSteam (clawsOfSteam, ClawsOfSteam (..)) where

import Arkham.Classes
import Arkham.Effect.Import
import Arkham.Game.Helpers
import Arkham.Prelude
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ClawsOfSteam = ClawsOfSteam TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clawsOfSteam :: TreacheryCard ClawsOfSteam
clawsOfSteam = treachery ClawsOfSteam Cards.clawsOfSteam

instance RunMessage ClawsOfSteam where
  runMessage msg t@(ClawsOfSteam attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid source #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      pushAll
        [ roundModifier source iid CannotMove
        , InvestigatorAssignDamage iid (TreacherySource treacheryId) DamageAssetsFirst 2 0
        ]
      pure t
    _ -> ClawsOfSteam <$> runMessage msg attrs
