{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda
  ( Agenda(..)
  , lookupAgenda
  )
where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Trait
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

lookupAgenda :: AgendaId -> Agenda
lookupAgenda = fromJustNote "Unknown agenda" . flip HashMap.lookup allAgendas

allAgendas :: HashMap AgendaId Agenda
allAgendas = HashMap.fromList $ map
  (\a -> (agendaId $ agendaAttrs a, a))
  [whatsGoingOn, riseOfTheGhouls, theyreGettingOut]

data Attrs = Attrs
  { agendaDoom          :: Int
  , agendaDoomThreshold :: GameValue
  , agendaId            :: AgendaId
  , agendaName          :: Text
  , agendaSequence      :: Text
  , agendaAbilities :: [Ability]
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "agenda"
  toEncoding = genericToEncoding $ aesonOptions $ Just "agenda"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "agenda"

instance HasAbilities Agenda where
  getAbilities = agendaAbilities . agendaAttrs

doom :: Lens' Attrs Int
doom = lens agendaDoom $ \m x -> m { agendaDoom = x }

doomThreshold :: Lens' Attrs GameValue
doomThreshold =
  lens agendaDoomThreshold $ \m x -> m { agendaDoomThreshold = x }

baseAttrs :: AgendaId -> Text -> Text -> GameValue -> Attrs
baseAttrs aid name seq' threshold = Attrs
  { agendaDoom = 0
  , agendaDoomThreshold = threshold
  , agendaId = aid
  , agendaName = name
  , agendaSequence = seq'
  , agendaAbilities = mempty
  }

data Agenda
  = WhatsGoingOn WhatsGoingOnI
  | RiseOfTheGhouls RiseOfTheGhoulsI
  | TheyreGettingOut TheyreGettingOutI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

agendaAttrs :: Agenda -> Attrs
agendaAttrs = \case
  WhatsGoingOn attrs -> coerce attrs
  RiseOfTheGhouls attrs -> coerce attrs
  TheyreGettingOut attrs -> coerce attrs

newtype WhatsGoingOnI = WhatsGoingOnI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

whatsGoingOn :: Agenda
whatsGoingOn = WhatsGoingOn . WhatsGoingOnI $ baseAttrs
  "01105"
  "What's Going On?!"
  "Agenda 1a"
  (Static 3)
newtype RiseOfTheGhoulsI = RiseOfTheGhoulsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

riseOfTheGhouls :: Agenda
riseOfTheGhouls = RiseOfTheGhouls . RiseOfTheGhoulsI $ baseAttrs
  "01106"
  "Rise of the Ghouls"
  "Agenda 2a"
  (Static 7)

newtype TheyreGettingOutI = TheyreGettingOutI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theyreGettingOut :: Agenda
theyreGettingOut = TheyreGettingOut . TheyreGettingOutI $ baseAttrs
  "01107"
  "They're Getting Out!"
  "Agenda 3a"
  (Static 10)

type AgendaRunner env
  = ( HasId LeadInvestigatorId () env
    , HasCount PlayerCount () env
    , HasQueue env
    , HasSet UnengagedEnemyId () env
    , HasSet EnemyId Trait env
    , HasSet EnemyId LocationId env
    , HasSet ClosestLocationId (LocationId, LocationId) env
    , HasId LocationId EnemyId env
    , HasCount EnemyCount (LocationId, [Trait]) env
    , HasSet ActId () env
    )

instance (AgendaRunner env) => RunMessage env Agenda where
  runMessage msg = \case
    WhatsGoingOn x -> WhatsGoingOn <$> runMessage msg x
    RiseOfTheGhouls x -> RiseOfTheGhouls <$> runMessage msg x
    TheyreGettingOut x -> TheyreGettingOut <$> runMessage msg x

instance (AgendaRunner env) => RunMessage env WhatsGoingOnI where
  runMessage msg a@(WhatsGoingOnI attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
      a <$ unshiftMessages
        [ Ask $ ChooseOneFromSource $ MkChooseOneFromSource
          { chooseOneSource = AgendaSource aid
          , chooseOneChoices =
            [ label
              "Each investigator discards 1 card at random from his or her hand"
              AllRandomDiscard
            , label
              "The lead investigator takes 2 horror"
              (InvestigatorDamage leadInvestigatorId (AgendaSource aid) 0 2)
            ]
          }
        , NextAgenda aid "01106"
        ]
    _ -> WhatsGoingOnI <$> runMessage msg attrs

instance (AgendaRunner env) => RunMessage env RiseOfTheGhoulsI where
  runMessage msg a@(RiseOfTheGhoulsI attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId -> a <$ unshiftMessages
      [ ShuffleEncounterDiscardBackIn
      , DiscardEncounterUntilFirst (AgendaSource aid) (EnemyType, Ghoul)
      ]
    RequestedEncounterCard (AgendaSource aid) mcard | aid == agendaId ->
      case mcard of
        Nothing -> a <$ unshiftMessage (NextAgenda aid "01107")
        Just card -> do
          leadInvestigatorId <- unLeadInvestigatorId <$> asks (getId ())
          a <$ unshiftMessages
            [ InvestigatorDrewEncounterCard leadInvestigatorId card
            , NextAgenda aid "01107"
            ]
    _ -> RiseOfTheGhoulsI <$> runMessage msg attrs

instance (AgendaRunner env) => RunMessage env TheyreGettingOutI where
  runMessage msg a@(TheyreGettingOutI attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId -> do
      actIds <- asks (getSet @ActId ())
      if ActId "01108" `elem` actIds || ActId "01109" `elem` actIds
        then a <$ unshiftMessage (Resolution 3)
        else a <$ unshiftMessage NoResolution -- TODO: defeated and suffer trauma
    EndEnemy -> do
      unengagedEnemyIds <- HashSet.map unUnengagedEnemyId <$> asks (getSet ())
      ghoulEnemyIds <- asks (getSet Ghoul)
      parlorEnemyIds <- asks (getSet (LocationId "01115"))
      let
        enemiesToMove =
          (ghoulEnemyIds `intersection` unengagedEnemyIds)
            `difference` parlorEnemyIds
      messages <- for (HashSet.toList enemiesToMove) $ \eid -> do
        locationId <- asks (getId eid)
        closestLocationIds <-
          HashSet.toList . HashSet.map unClosestLocationId <$> asks
            (getSet (locationId, LocationId "01115"))
        case closestLocationIds of
          [] -> pure Nothing
          [x] -> pure $ Just $ EnemyMove eid locationId x
          xs ->
            pure $ Just $ Ask $ ChooseOne $ map (EnemyMove eid locationId) xs
      a <$ unshiftMessage (Ask $ ChooseOneAtATime (catMaybes messages))
    EndRoundWindow -> do
      parlorGhoulsCount <- unEnemyCount
        <$> asks (getCount (LocationId "01115", [Ghoul]))
      hallwayGhoulsCount <- unEnemyCount
        <$> asks (getCount (LocationId "01112", [Ghoul]))
      a <$ unshiftMessages
        (replicate (parlorGhoulsCount + hallwayGhoulsCount) PlaceDoomOnAgenda)
    _ -> TheyreGettingOutI <$> runMessage msg attrs

instance (AgendaRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    PlaceDoomOnAgenda -> pure $ a & doom +~ 1
    AdvanceAgendaIfThresholdSatisfied -> do
      pc <- unPlayerCount <$> asks (getCount ())
      when
        (a ^. doom + 1 > fromGameValue (a ^. doomThreshold) pc)
        (unshiftMessage (AdvanceAgenda agendaId))
      pure a
    _ -> pure a
