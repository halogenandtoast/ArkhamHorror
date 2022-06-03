module Arkham.Classes.RunMessage
  ( module Arkham.Classes.RunMessage
  ) where

import Arkham.Prelude hiding ( to )

import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Classes.HasHistory
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.Query
import Arkham.Message
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillTest
import Arkham.Asset.Attrs (AssetAttrs)
import Arkham.Investigator.Attrs (InvestigatorAttrs)
import GHC.Generics

type RunM env m
  = ( HasCallStack
    , HasQueue env
    , MonadIO m
    , MonadRandom m
    , HasHistory m
    , HasGameLogger env
    , HasModifiersFor m ()
    , HasSkillTest m
    , Query AssetMatcher m
    , Query EnemyMatcher m
    , Query LocationMatcher m
    , Query InvestigatorMatcher m
    , Query DiscardedPlayerCardMatcher m
    , Query ExtendedCardMatcher m
    , Query TreacheryMatcher m
    , Projection m AssetAttrs
    , Projection m InvestigatorAttrs
    )

class RunMessage1 f where
  runMessage1 :: (HasCallStack, MonadReader env m, RunM env m) => Message -> f p -> m (f p)

instance RunMessage1 f => RunMessage1 (M1 i c f) where
  runMessage1 msg (M1 x) = M1 <$> runMessage1 msg x

instance (RunMessage1 l, RunMessage1 r) => RunMessage1 (l :+: r) where
  runMessage1 msg (L1 x) = L1 <$> runMessage1 msg x
  runMessage1 msg (R1 x) = R1 <$> runMessage1 msg x

instance RunMessage p => RunMessage1 (K1 R p) where
  runMessage1 msg (K1 x) = K1 <$> runMessage msg x

class RunMessage a where
  runMessage :: (HasCallStack, MonadReader env m, RunM env m) => Message -> a -> m a

genericRunMessage
  :: ( Generic a
     , RunMessage1 (Rep a)
     , MonadReader env m
     , RunM env m
     )
  => Message
  -> a
  -> m a
genericRunMessage msg = fmap to . runMessage1 msg . from
