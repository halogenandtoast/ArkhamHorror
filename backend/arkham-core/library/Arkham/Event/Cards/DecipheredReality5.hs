module Arkham.Event.Cards.DecipheredReality5
  ( decipheredReality5
  , DecipheredReality5(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection

newtype DecipheredReality5 = DecipheredReality5 EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decipheredReality5 :: EventCard DecipheredReality5
decipheredReality5 = event DecipheredReality5 Cards.decipheredReality5

instance RunMessage DecipheredReality5 where
  runMessage msg e@(DecipheredReality5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      locationIds <- selectList RevealedLocation
      maxShroud <-
        maximum . ncons 0 <$> traverse (field LocationShroud) locationIds
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ skillTestModifier attrs SkillTestTarget (SetDifficulty maxShroud)
        , Investigate
          iid
          lid
          (toSource attrs)
          (Just $ toTarget attrs)
          skillType
          False
        ]
      pure e
    Successful (Action.Investigate, actionTarget) iid source target n
      | isTarget attrs target -> do
      -- Deciphered Reality is not a replacement effect; its effect doesn’t use
      -- any form of ‘instead’ or ‘but,’ so its effect is in addition to the
      -- standard rewards for successfully investigating.
        locationIds <- selectList RevealedLocation
        e <$ pushAll
          (Successful (Action.Investigate, actionTarget) iid source target n
          : [ InvestigatorDiscoverClues iid lid' 1 Nothing
            | lid' <- locationIds
            ]
          )
    _ -> DecipheredReality5 <$> runMessage msg attrs
