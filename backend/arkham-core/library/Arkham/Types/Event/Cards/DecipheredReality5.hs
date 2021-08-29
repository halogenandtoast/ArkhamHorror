module Arkham.Types.Event.Cards.DecipheredReality5
  ( decipheredReality5
  , DecipheredReality5(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Event.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype DecipheredReality5 = DecipheredReality5 EventAttrs
  deriving anyclass (IsEvent, HasAbilities env, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decipheredReality5 :: EventCard DecipheredReality5
decipheredReality5 = event DecipheredReality5 Cards.decipheredReality5

instance EventRunner env => RunMessage env DecipheredReality5 where
  runMessage msg e@(DecipheredReality5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
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
    SuccessfulInvestigation iid lid source target | isTarget attrs target -> do
      -- Deciphered Reality is not a replacement effect; its effect doesn’t use
      -- any form of ‘instead’ or ‘but,’ so its effect is in addition to the
      -- standard rewards for successfully investigating.
      locationIds <- selectList RevealedLocation
      e <$ pushAll
        (SuccessfulInvestigation iid lid source (LocationTarget lid)
        : [ DiscoverCluesAtLocation iid lid' 1 Nothing | lid' <- locationIds ]
        )
    _ -> DecipheredReality5 <$> runMessage msg attrs
