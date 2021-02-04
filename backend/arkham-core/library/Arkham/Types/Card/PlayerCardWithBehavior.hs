module Arkham.Types.Card.PlayerCardWithBehavior
  ( toPlayerCardWithBehavior
  ) where

import ClassyPrelude

import Arkham.Types.Asset.Uses (UseType(..))
import Arkham.Types.AssetId
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

data PlayerCard'
  = DefaultPlayerCard' DefaultPlayerCard
  | SureGamble3' SureGamble3
  | CloseCall2' CloseCall2
  | LetMeHandleThis' LetMeHandleThis
  | SecondWind' SecondWind
  | AstoundingRevelation' AstoundingRevelation
  deriving stock (Generic, Show)

newtype DefaultPlayerCard = DefaultPlayerCard PlayerCard
  deriving newtype Show
newtype SureGamble3 = SureGamble3 PlayerCard
  deriving newtype Show
newtype CloseCall2 = CloseCall2 PlayerCard
  deriving newtype Show
newtype LetMeHandleThis = LetMeHandleThis PlayerCard
  deriving newtype Show
newtype SecondWind = SecondWind PlayerCard
  deriving newtype Show
newtype AstoundingRevelation = AstoundingRevelation PlayerCard
  deriving newtype Show

allPlayerCardsWithBehavior :: HashMap CardCode (PlayerCard -> PlayerCard')
allPlayerCardsWithBehavior = mapFromList
  [ ("01056", SureGamble3' . SureGamble3)
  , ("01083", CloseCall2' . CloseCall2)
  , ("03022", LetMeHandleThis' . LetMeHandleThis)
  , ("04149", SecondWind' . SecondWind)
  , ("06023", AstoundingRevelation' . AstoundingRevelation)
  ]

toPlayerCardWithBehavior :: PlayerCard -> PlayerCard'
toPlayerCardWithBehavior pc = builder pc
 where
  builder =
    findWithDefault defaultCard (getCardCode pc) allPlayerCardsWithBehavior
  defaultCard = DefaultPlayerCard' . DefaultPlayerCard

deriving anyclass instance
  ( HasId CardCode env EnemyId
  , HasSet Trait env EnemyId
  , HasSet AssetId env (InvestigatorId, UseType)
  , HasCount ActionTakenCount env InvestigatorId
  )
  => HasActions env PlayerCard'

instance HasActions env DefaultPlayerCard where
  getActions _ _ _ = pure []

instance HasActions env SureGamble3 where
  getActions iid (WhenRevealTokenWithNegativeModifier You tid) (SureGamble3 pc)
    = pure [InitiatePlayCard iid (getCardId pc) (Just $ TokenTarget tid) False]
  getActions i window (SureGamble3 pc) =
    getActions i window (DefaultPlayerCard pc)

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

instance HasActions env LetMeHandleThis where
  getActions iid (WhenDrawNonPerilTreachery who tid) (LetMeHandleThis pc)
    | who /= You = pure
      [InitiatePlayCard iid (getCardId pc) (Just $ TreacheryTarget tid) False]
  getActions i window (LetMeHandleThis pc) =
    getActions i window (DefaultPlayerCard pc)

instance HasCount ActionTakenCount env InvestigatorId => HasActions env SecondWind where
  getActions iid (DuringTurn You) (SecondWind pc) = do
    actionsTaken <- unActionTakenCount <$> getCount iid
    pure
      [ InitiatePlayCard iid (getCardId pc) Nothing True | actionsTaken == 0 ]
  getActions i window (SecondWind pc) =
    getActions i window (DefaultPlayerCard pc)

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
