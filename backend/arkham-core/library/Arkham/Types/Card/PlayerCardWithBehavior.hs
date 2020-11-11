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
  | TheNecronomicon' TheNecronomicon
  | DarkMemory' DarkMemory
  | SureGamble3' SureGamble3
  | CloseCall2' CloseCall2
  | LetMeHandleThis' LetMeHandleThis
  | AstoundingRevelation' AstoundingRevelation
  | TheNecronomiconAdvanced' TheNecronomiconAdvanced
  deriving stock (Generic, Show)

newtype DefaultPlayerCard = DefaultPlayerCard PlayerCard
  deriving newtype Show
newtype TheNecronomicon = TheNecronomicon PlayerCard
  deriving newtype Show
newtype DarkMemory = DarkMemory PlayerCard
  deriving newtype Show
newtype SureGamble3 = SureGamble3 PlayerCard
  deriving newtype Show
newtype CloseCall2 = CloseCall2 PlayerCard
  deriving newtype Show
newtype LetMeHandleThis = LetMeHandleThis PlayerCard
  deriving newtype Show
newtype AstoundingRevelation = AstoundingRevelation PlayerCard
  deriving newtype Show
newtype TheNecronomiconAdvanced = TheNecronomiconAdvanced PlayerCard
  deriving newtype Show

allPlayerCardsWithBehavior :: HashMap CardCode (PlayerCard -> PlayerCard')
allPlayerCardsWithBehavior = mapFromList
  [ ("01009", TheNecronomicon' . TheNecronomicon)
  , ("01013", DarkMemory' . DarkMemory)
  , ("01056", SureGamble3' . SureGamble3)
  , ("01083", CloseCall2' . CloseCall2)
  , ("03022", LetMeHandleThis' . LetMeHandleThis)
  , ("06023", AstoundingRevelation' . AstoundingRevelation)
  , ("90003", TheNecronomiconAdvanced' . TheNecronomiconAdvanced)
  ]

toPlayerCardWithBehavior :: PlayerCard -> PlayerCard'
toPlayerCardWithBehavior pc = builder pc
 where
  builder =
    findWithDefault defaultCard (getCardCode pc) allPlayerCardsWithBehavior
  defaultCard = DefaultPlayerCard' . DefaultPlayerCard

deriving anyclass instance (HasSet HandCardId InvestigatorId env, HasQueue env) => RunMessage env PlayerCard'
deriving anyclass instance (HasId CardCode EnemyId env, HasSet Trait EnemyId env) => HasActions env PlayerCard'

instance HasQueue env => RunMessage env DefaultPlayerCard where
  runMessage _ pc = pure pc

instance HasActions env DefaultPlayerCard where
  getActions _ _ _ = pure []

instance HasQueue env => RunMessage env TheNecronomicon where
  runMessage (Revelation iid (PlayerCardSource cid)) c@(TheNecronomicon pc)
    | getCardId pc == cid = c <$ unshiftMessage (PlayCard iid cid Nothing False)
  runMessage msg (TheNecronomicon pc) = do
    DefaultPlayerCard pc' <- runMessage msg (DefaultPlayerCard pc)
    pure $ TheNecronomicon pc'

instance HasActions env TheNecronomicon where
  getActions i window (TheNecronomicon pc) =
    getActions i window (DefaultPlayerCard pc)

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

instance HasActions env DarkMemory where
  getActions i window (DarkMemory pc) =
    getActions i window (DefaultPlayerCard pc)

instance (HasQueue env) => RunMessage env SureGamble3 where
  runMessage msg (SureGamble3 pc) = do
    DefaultPlayerCard pc' <- runMessage msg (DefaultPlayerCard pc)
    pure $ SureGamble3 pc'

instance HasActions env SureGamble3 where
  getActions iid (WhenRevealTokenWithNegativeModifier You tid) (SureGamble3 pc)
    = pure [PlayCard iid (getCardId pc) (Just $ TokenTarget tid) False]
  getActions i window (SureGamble3 pc) =
    getActions i window (DefaultPlayerCard pc)

instance (HasQueue env) => RunMessage env CloseCall2 where
  runMessage msg (CloseCall2 pc) = do
    DefaultPlayerCard pc' <- runMessage msg (DefaultPlayerCard pc)
    pure $ CloseCall2 pc'

instance (HasId CardCode EnemyId env, HasSet Trait EnemyId env) => HasActions env CloseCall2 where
  getActions iid (AfterEnemyEvaded You eid) (CloseCall2 pc) = do
    traits' <- asks (getSet eid)
    cardCode <- asks (getId eid)
    pure
      [ PlayCard iid (getCardId pc) (Just $ EnemyTarget eid) False
      | Elite `notMember` traits' && cardCode `elem` keys allEncounterCards
      ]
  getActions i window (CloseCall2 pc) =
    getActions i window (DefaultPlayerCard pc)

instance (HasQueue env) => RunMessage env LetMeHandleThis where
  runMessage msg (LetMeHandleThis pc) = do
    DefaultPlayerCard pc' <- runMessage msg (DefaultPlayerCard pc)
    pure $ LetMeHandleThis pc'

instance HasActions env LetMeHandleThis where
  getActions iid (WhenDrawNonPerilTreachery who tid) (LetMeHandleThis pc)
    | who /= You = pure
      [PlayCard iid (getCardId pc) (Just $ TreacheryTarget tid) False]
  getActions i window (LetMeHandleThis pc) =
    getActions i window (DefaultPlayerCard pc)

instance (HasQueue env) => RunMessage env AstoundingRevelation where
  runMessage msg (AstoundingRevelation pc) = do
    DefaultPlayerCard pc' <- runMessage msg (DefaultPlayerCard pc)
    pure $ AstoundingRevelation pc'

instance HasActions env AstoundingRevelation where
  getActions iid (WhenAmongSearchedCards You) (AstoundingRevelation pc) = pure
    [ Run
        [ Discard (SearchedCardIdTarget $ getCardId pc)
        , chooseOne iid [TakeResources iid 2 False]
        ]
    ]
  getActions i window (AstoundingRevelation pc) =
    getActions i window (DefaultPlayerCard pc)

instance HasQueue env => RunMessage env TheNecronomiconAdvanced where
  runMessage (Revelation iid (PlayerCardSource cid)) c@(TheNecronomiconAdvanced pc)
    | getCardId pc == cid
    = c <$ unshiftMessage (PlayCard iid cid Nothing False)
  runMessage msg (TheNecronomiconAdvanced pc) = do
    DefaultPlayerCard pc' <- runMessage msg (DefaultPlayerCard pc)
    pure $ TheNecronomiconAdvanced pc'

instance HasActions env TheNecronomiconAdvanced where
  getActions i window (TheNecronomiconAdvanced pc) =
    getActions i window (DefaultPlayerCard pc)
