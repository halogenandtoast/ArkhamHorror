module Arkham.Event.Cards.CheapShot (
  cheapShot,
  CheapShot (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Message
import Arkham.SkillType

newtype CheapShot = CheapShot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cheapShot :: EventCard CheapShot
cheapShot = event CheapShot Cards.cheapShot

instance RunMessage CheapShot where
  runMessage msg e@(CheapShot attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier attrs iid (AddSkillValue SkillAgility)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure e
    PassedSkillTest iid _ _ (SkillTestInitiatorTarget (InvestigatorTarget _)) _ n
      | n >= 2 ->
          do
            mSkillTestTarget <- getSkillTestTarget
            case mSkillTestTarget of
              Just (EnemyTarget eid) -> push $ EnemyEvaded iid eid
              _ -> pure ()
            pure e
    _ -> CheapShot <$> runMessage msg attrs
