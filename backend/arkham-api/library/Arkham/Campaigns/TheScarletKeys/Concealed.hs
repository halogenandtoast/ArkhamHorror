{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaigns.TheScarletKeys.Concealed (
  module Arkham.Campaigns.TheScarletKeys.Concealed.Types,
  module Arkham.Campaigns.TheScarletKeys.Concealed.Query,
  module Arkham.Campaigns.TheScarletKeys.Concealed.Kind,
  module Arkham.Campaigns.TheScarletKeys.Concealed,
  module Arkham.Campaigns.TheScarletKeys.Concealed.Matcher,
) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Concealed.Query
import Arkham.Campaigns.TheScarletKeys.Concealed.Types
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Constants
import Arkham.Placement
import Arkham.Prelude

mkConcealedCard :: MonadRandom m => ConcealedCardKind -> m ConcealedCard
mkConcealedCard kind = do
  cid <- getRandom
  pure
    $ ConcealedCard
      { concealedCardKind = kind
      , concealedCardId = cid
      , concealedCardPlacement = Unplaced
      , concealedCardFlipped = False
      , concealedCardKnown = False
      }

instance HasAbilities ConcealedCard where
  getAbilities a =
    [ basicAbility $ restricted a AbilityAttack OnSameLocation fightAction_
    , basicAbility $ restricted a AbilityEvade OnSameLocation evadeAction_
    ]

instance HasModifiersFor ConcealedCard where
  getModifiersFor _ = pure ()
