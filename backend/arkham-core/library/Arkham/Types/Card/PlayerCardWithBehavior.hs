{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Card.PlayerCardWithBehavior
  ( toPlayerCardWithBehavior
  )
where

import ClassyPrelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

data PlayerCard'
  = DefaultPlayerCard' DefaultPlayerCard
  | DarkMemory' DarkMemory
  | CloseCall2' CloseCall2
  deriving stock (Generic, Show)

newtype DefaultPlayerCard = DefaultPlayerCard PlayerCard
  deriving newtype Show
newtype DarkMemory = DarkMemory PlayerCard
  deriving newtype Show
newtype CloseCall2 = CloseCall2 PlayerCard
  deriving newtype Show

allPlayerCardsWithBehavior :: HashMap CardCode (PlayerCard -> PlayerCard')
allPlayerCardsWithBehavior = mapFromList
  [("01013", DarkMemory' . DarkMemory), ("01083", CloseCall2' . CloseCall2)]

toPlayerCardWithBehavior :: PlayerCard -> PlayerCard'
toPlayerCardWithBehavior pc = builder pc
 where
  builder =
    findWithDefault defaultCard (getCardCode pc) allPlayerCardsWithBehavior
  defaultCard = DefaultPlayerCard' . DefaultPlayerCard

deriving anyclass instance (HasSet HandCardId InvestigatorId env, HasQueue env) => RunMessage env PlayerCard'
deriving anyclass instance (HasId CardCode EnemyId env, HasSet Trait EnemyId env, IsInvestigator investigator) => HasActions env investigator PlayerCard'

instance HasQueue env => RunMessage env DefaultPlayerCard where
  runMessage _ pc = pure pc

instance HasActions env investigator DefaultPlayerCard where
  getActions _ _ _ = pure []

instance (HasSet HandCardId InvestigatorId env, HasQueue env) => RunMessage env DarkMemory where
  runMessage (EndTurn iid) c@(DarkMemory pc) = do
    let cardId = getCardId pc
    cardIds <- map unHandCardId . setToList <$> asks (getSet iid)
    c <$ when
      (cardId `elem` cardIds)
      (unshiftMessages
        [ RevealInHand cardId
        , InvestigatorAssignDamage iid (PlayerCardSource cardId) 0 2
        ]
      )
  runMessage msg (DarkMemory pc) = do
    DefaultPlayerCard pc' <- runMessage msg (DefaultPlayerCard pc)
    pure $ DarkMemory pc'

instance HasActions env investigator DarkMemory where
  getActions i window (DarkMemory pc) =
    getActions i window (DefaultPlayerCard pc)

instance (HasQueue env) => RunMessage env CloseCall2 where
  runMessage msg (CloseCall2 pc) = do
    DefaultPlayerCard pc' <- runMessage msg (DefaultPlayerCard pc)
    pure $ CloseCall2 pc'

instance (HasId CardCode EnemyId env, HasSet Trait EnemyId env, IsInvestigator investigator) => HasActions env investigator CloseCall2 where
  getActions i (AfterEnemyEvaded You eid) (CloseCall2 pc) = do
    traits' <- asks (getSet eid)
    cardCode <- asks (getId eid)
    pure
      [ PlayCard (getId () i) (getCardId pc) (Just $ EnemyTarget eid) False
      | Elite `notMember` traits' && cardCode `elem` keys allEncounterCards
      ]
  getActions i window (CloseCall2 pc) =
    getActions i window (DefaultPlayerCard pc)
