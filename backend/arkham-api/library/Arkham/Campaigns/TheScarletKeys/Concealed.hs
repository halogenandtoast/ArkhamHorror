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
import Arkham.Location.Grid
import Arkham.Matcher
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
  getAbilities a = case a.placement of
    InPosition pos ->
      [ basicAbility
          $ restricted
            a
            AbilityAttack
            (youExist $ at_ $ mapOneOf LocationInPosition $ adjacentPositions pos)
            fightAction_
      , basicAbility
          $ restricted
            a
            AbilityEvade
            (youExist $ at_ $ mapOneOf LocationInPosition $ adjacentPositions pos)
            evadeAction_
      ]
    _ ->
      [ basicAbility $ restricted a AbilityAttack OnSameLocation fightAction_
      , basicAbility $ restricted a AbilityEvade OnSameLocation evadeAction_
      ]

instance HasModifiersFor ConcealedCard where
  getModifiersFor _ = pure ()
