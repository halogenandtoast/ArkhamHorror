module Arkham.Event.Cards.DecipheredReality5
  ( decipheredReality5
  , DecipheredReality5(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.SkillType
import Arkham.Target

newtype DecipheredReality5 = DecipheredReality5 EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decipheredReality5 :: EventCard DecipheredReality5
decipheredReality5 = event DecipheredReality5 Cards.decipheredReality5

instance EventRunner env => RunMessage DecipheredReality5 where
  runMessage msg e@(DecipheredReality5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- getId iid
      locationIds <- selectList RevealedLocation
      maxShroud <-
        maximum . ncons 0 <$> traverse (fmap unShroud . getCount) locationIds
      e <$ pushAll
        [ skillTestModifier attrs SkillTestTarget (SetDifficulty maxShroud)
        , Investigate
          iid
          lid
          (toSource attrs)
          (Just $ toTarget attrs)
          SkillIntellect
          False
        , Discard (toTarget attrs)
        ]
    Successful (Action.Investigate, actionTarget) iid source target n
      | isTarget attrs target -> do
      -- Deciphered Reality is not a replacement effect; its effect doesn’t use
      -- any form of ‘instead’ or ‘but,’ so its effect is in addition to the
      -- standard rewards for successfully investigating.
        locationIds <- selectList RevealedLocation
        e <$ pushAll
          (Successful (Action.Investigate, actionTarget) iid source target n
          : [ DiscoverCluesAtLocation iid lid' 1 Nothing
            | lid' <- locationIds
            ]
          )
    _ -> DecipheredReality5 <$> runMessage msg attrs
