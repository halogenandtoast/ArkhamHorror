module Arkham.Types.Card.PlayerCardWithBehavior
  ( toPlayerCardWithBehavior
  )
where

import ClassyPrelude

import Arkham.Types.Asset.Uses (UseType(..))
import Arkham.Types.AssetId
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

data PlayerCard'
  = DefaultPlayerCard' DefaultPlayerCard
  | TheNecronomicon' TheNecronomicon
  | DarkMemory' DarkMemory
  | SureGamble3' SureGamble3
  | StrayCat' StrayCat
  | CloseCall2' CloseCall2
  | LetMeHandleThis' LetMeHandleThis
  | SecondWind' SecondWind
  | AstoundingRevelation' AstoundingRevelation
  | TheNecronomiconAdvanced' TheNecronomiconAdvanced
  deriving stock (Generic, Show)

newtype DefaultPlayerCard = DefaultPlayerCard { unDefaultPlayerCard :: PlayerCard }
  deriving newtype Show
newtype TheNecronomicon = TheNecronomicon PlayerCard
  deriving newtype Show
newtype DarkMemory = DarkMemory PlayerCard
  deriving newtype Show
newtype SureGamble3 = SureGamble3 PlayerCard
  deriving newtype Show
newtype StrayCat = StrayCat PlayerCard
  deriving newtype Show
newtype CloseCall2 = CloseCall2 PlayerCard
  deriving newtype Show
newtype LetMeHandleThis = LetMeHandleThis PlayerCard
  deriving newtype Show
newtype SecondWind = SecondWind PlayerCard
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
  , ("01076", StrayCat' . StrayCat)
  , ("01083", CloseCall2' . CloseCall2)
  , ("03022", LetMeHandleThis' . LetMeHandleThis)
  , ("04149", SecondWind' . SecondWind)
  , ("06023", AstoundingRevelation' . AstoundingRevelation)
  , ("90003", TheNecronomiconAdvanced' . TheNecronomiconAdvanced)
  ]

toPlayerCardWithBehavior :: PlayerCard -> PlayerCard'
toPlayerCardWithBehavior pc = builder pc
 where
  builder =
    findWithDefault defaultCard (getCardCode pc) allPlayerCardsWithBehavior
  defaultCard = DefaultPlayerCard' . DefaultPlayerCard

deriving anyclass instance
  ( HasSet HandCardId env InvestigatorId
  , HasQueue env
  , HasSet Trait env EnemyId
  , HasSet EnemyId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env PlayerCard'

deriving anyclass instance
  ( HasId CardCode env EnemyId
  , HasSet Trait env EnemyId
  , HasSet AssetId env (InvestigatorId, UseType)
  , HasCount ActionTakenCount env InvestigatorId
  )
  => HasActions env PlayerCard'

instance HasQueue env => RunMessage env DefaultPlayerCard where
  runMessage _ pc = pure pc

instance HasActions env DefaultPlayerCard where
  getActions _ _ _ = pure []

instance HasQueue env => RunMessage env TheNecronomicon where
  runMessage (InHand (Revelation iid (PlayerCardSource cid))) c@(TheNecronomicon pc)
    | getCardId pc == cid
    = c <$ unshiftMessage (PlayCard iid cid Nothing False)
  runMessage msg (TheNecronomicon pc) =
    TheNecronomicon . unDefaultPlayerCard <$> runMessage
      msg
      (DefaultPlayerCard pc)

instance HasActions env TheNecronomicon where
  getActions i window (TheNecronomicon pc) =
    getActions i window (DefaultPlayerCard pc)

instance HasQueue env => RunMessage env DarkMemory where
  runMessage (InHand (EndTurn iid)) c@(DarkMemory pc) = do
    let cardId = getCardId pc
    c <$ unshiftMessages
      [ RevealInHand cardId
      , InvestigatorAssignDamage iid (PlayerCardSource cardId) 0 2
      ]
  runMessage msg (DarkMemory pc) =
    DarkMemory . unDefaultPlayerCard <$> runMessage msg (DefaultPlayerCard pc)

instance HasActions env DarkMemory where
  getActions i window (DarkMemory pc) =
    getActions i window (DefaultPlayerCard pc)

instance HasQueue env => RunMessage env SureGamble3 where
  runMessage msg (SureGamble3 pc) =
    SureGamble3 . unDefaultPlayerCard <$> runMessage msg (DefaultPlayerCard pc)

instance HasActions env SureGamble3 where
  getActions iid (WhenRevealTokenWithNegativeModifier You tid) (SureGamble3 pc)
    = pure [InitiatePlayCard iid (getCardId pc) (Just $ TokenTarget tid) False]
  getActions i window (SureGamble3 pc) =
    getActions i window (DefaultPlayerCard pc)

instance HasActions env StrayCat where
  getActions i window (StrayCat pc) =
    getActions i window (DefaultPlayerCard pc)

instance
  ( HasQueue env
  , HasSet Trait env EnemyId
  , HasSet EnemyId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env StrayCat where
  runMessage (InDiscard (UseCardAbility iid (AssetSource aid) _ 1 _)) c@(StrayCat pc)
    | unAssetId aid == unCardId (getCardId pc)
    = do
      locationId <- getId @LocationId iid
      locationEnemyIds <- getSetList locationId
      nonEliteEnemyIds <- filterM
        ((notMember Elite <$>) . getSet)
        locationEnemyIds

      c <$ unshiftMessage
        (chooseOne iid [ EnemyEvaded iid enemyId | enemyId <- nonEliteEnemyIds ]
        )
  runMessage msg (StrayCat pc) =
    StrayCat . unDefaultPlayerCard <$> runMessage msg (DefaultPlayerCard pc)

instance (HasQueue env) => RunMessage env CloseCall2 where
  runMessage msg (CloseCall2 pc) =
    CloseCall2 . unDefaultPlayerCard <$> runMessage msg (DefaultPlayerCard pc)

instance (HasId CardCode env EnemyId, HasSet Trait env EnemyId) => HasActions env CloseCall2 where
  getActions iid (AfterEnemyEvaded You eid) (CloseCall2 pc) = do
    traits' <- getSet eid
    cardCode <- getId eid
    pure
      [ InitiatePlayCard iid (getCardId pc) (Just $ EnemyTarget eid) False
      | Elite `notMember` traits' && cardCode `elem` keys allEncounterCards
      ]
  getActions i window (CloseCall2 pc) =
    getActions i window (DefaultPlayerCard pc)

instance (HasQueue env) => RunMessage env LetMeHandleThis where
  runMessage msg (LetMeHandleThis pc) =
    LetMeHandleThis . unDefaultPlayerCard <$> runMessage
      msg
      (DefaultPlayerCard pc)

instance HasActions env LetMeHandleThis where
  getActions iid (WhenDrawNonPerilTreachery who tid) (LetMeHandleThis pc)
    | who /= You = pure
      [InitiatePlayCard iid (getCardId pc) (Just $ TreacheryTarget tid) False]
  getActions i window (LetMeHandleThis pc) =
    getActions i window (DefaultPlayerCard pc)

instance HasQueue env => RunMessage env SecondWind where
  runMessage msg (SecondWind pc) =
    SecondWind . unDefaultPlayerCard <$> runMessage msg (DefaultPlayerCard pc)

instance HasCount ActionTakenCount env InvestigatorId => HasActions env SecondWind where
  getActions iid (DuringTurn You) (SecondWind pc) = do
    actionsTaken <- unActionTakenCount <$> getCount iid
    pure
      [ InitiatePlayCard iid (getCardId pc) Nothing True | actionsTaken == 0 ]
  getActions i window (SecondWind pc) =
    getActions i window (DefaultPlayerCard pc)

instance HasQueue env => RunMessage env AstoundingRevelation where
  runMessage msg (AstoundingRevelation pc) =
    AstoundingRevelation . unDefaultPlayerCard <$> runMessage
      msg
      (DefaultPlayerCard pc)

instance HasSet AssetId env (InvestigatorId, UseType) => HasActions env AstoundingRevelation where
  getActions iid (WhenAmongSearchedCards You) (AstoundingRevelation pc) = do
    secretAssetIds <- getSetList (iid, Secret)
    pure
      [ Run
          [ Discard (SearchedCardTarget iid $ getCardId pc)
          , chooseOne
            iid
            (TakeResources iid 2 False
            : [ AddUses (AssetTarget aid) Secret 1 | aid <- secretAssetIds ]
            )
          ]
      ]
  getActions i window (AstoundingRevelation pc) =
    getActions i window (DefaultPlayerCard pc)

instance HasQueue env => RunMessage env TheNecronomiconAdvanced where
  runMessage (InHand (Revelation iid (PlayerCardSource cid))) c@(TheNecronomiconAdvanced pc)
    | getCardId pc == cid
    = c <$ unshiftMessage (PlayCard iid cid Nothing False)
  runMessage msg (TheNecronomiconAdvanced pc) =
    TheNecronomiconAdvanced . unDefaultPlayerCard <$> runMessage
      msg
      (DefaultPlayerCard pc)

instance HasActions env TheNecronomiconAdvanced where
  getActions i window (TheNecronomiconAdvanced pc) =
    getActions i window (DefaultPlayerCard pc)
