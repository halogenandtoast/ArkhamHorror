{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Card.PlayerCardWithBehavior
  ( toPlayerCardWithBehavior
  )
where

import ClassyPrelude

import Arkham.Types.Card.PlayerCard
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.Card.Class
import Arkham.Types.Message
import Arkham.Types.InvestigatorId
import Arkham.Types.Source
import Arkham.Types.Classes

data PlayerCard'
  = DefaultPlayerCard' DefaultPlayerCard
  | DarkMemory' DarkMemory
  deriving stock (Generic)

newtype DefaultPlayerCard = DefaultPlayerCard PlayerCard
newtype DarkMemory = DarkMemory PlayerCard

allPlayerCardsWithBehavior :: HashMap CardCode (PlayerCard -> PlayerCard')
allPlayerCardsWithBehavior = mapFromList [ ("01013", DarkMemory' . DarkMemory) ]

toPlayerCardWithBehavior :: PlayerCard -> PlayerCard'
toPlayerCardWithBehavior pc = builder pc
  where
    builder = findWithDefault defaultCard (getCardCode pc) allPlayerCardsWithBehavior
    defaultCard = DefaultPlayerCard' . DefaultPlayerCard

deriving anyclass instance (HasSet HandCardId InvestigatorId env, HasQueue env) => RunMessage env PlayerCard'

instance HasQueue env => RunMessage env DefaultPlayerCard where
  runMessage _ pc = pure pc

instance (HasSet HandCardId InvestigatorId env, HasQueue env) => RunMessage env DarkMemory where
  runMessage (EndTurn iid) c@(DarkMemory pc) = do
    let cardId = getCardId pc
    cardIds <- map unHandCardId . setToList <$> asks (getSet iid)
    c <$ when (cardId `elem` cardIds) (unshiftMessages
      [ RevealInHand cardId
      , InvestigatorAssignDamage iid (PlayerCardSource cardId) 0 2
      ])
  runMessage _ pc = pure pc
