{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaigns.TheScarletKeys.Concealed (
  module Arkham.Campaigns.TheScarletKeys.Concealed.Types,
  module Arkham.Campaigns.TheScarletKeys.Concealed.Kind,
  module Arkham.Campaigns.TheScarletKeys.Concealed,
) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Campaigns.TheScarletKeys.Concealed.Types
import Arkham.Card.CardCode
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Constants
import Arkham.Field
import Arkham.Json
import Arkham.Placement
import Arkham.Prelude
import Data.Aeson.TH

mkConcealedCard :: MonadRandom m => ConcealedCardKind -> m ConcealedCard
mkConcealedCard kind = do
  cid <- getRandom
  pure
    $ ConcealedCard
      { concealedCardKind = kind
      , concealedCardId = cid
      , concealedCardPlacement = Unplaced
      , concealedCardFlipped = False
      }

data instance Field ConcealedCard :: Type -> Type where
  ConcealedCardKind :: Field ConcealedCard ConcealedCardKind
  ConcealedCardPlacement :: Field ConcealedCard Placement

instance HasCardCode ConcealedCard where
  toCardCode = const "xconcealed"

instance HasAbilities ConcealedCard where
  getAbilities a =
    [ basicAbility $ restricted a AbilityAttack OnSameLocation fightAction_
    , basicAbility $ restricted a AbilityEvade OnSameLocation evadeAction_
    ]

instance HasModifiersFor ConcealedCard where
  getModifiersFor _ = pure ()

$(deriveJSON (aesonOptions $ Just "concealedCard") ''ConcealedCard)
