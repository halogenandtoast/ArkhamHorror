module Arkham.Event.Cards.CheapShot
  ( cheapShot
  , CheapShot(..)
  ) where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
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
      agility <- getSkillValue SkillAgility iid
      e <$ pushAll
        [ CreateEffect
          (cdCardCode $ toCardDef attrs)
          (Just $ EffectInt agility)
          (toSource attrs)
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
    PassedSkillTest iid _ _ (SkillTestInitiatorTarget (InvestigatorTarget _)) _ n
      | n >= 2
      -> do
        mSkillTestTarget <- getSkillTestTarget
        e <$ case mSkillTestTarget of
          Just (EnemyTarget eid) -> push $ EnemyEvaded iid eid
          _ -> pure ()
    _ -> CheapShot <$> runMessage msg attrs
