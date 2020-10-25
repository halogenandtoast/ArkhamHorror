{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Elusive where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import qualified Data.HashSet as HashSet

newtype Elusive = Elusive Attrs
  deriving newtype (Show, ToJSON, FromJSON)

elusive :: InvestigatorId -> EventId -> Elusive
elusive iid uuid = Elusive $ baseAttrs iid uuid "01050"

instance HasModifiersFor env Elusive where
  getModifiersFor _ _ _ = pure []

instance HasActions env Elusive where
  getActions i window (Elusive attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Elusive where
  runMessage msg (Elusive attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      enemyIds <- asks $ setToList . getSet iid
      emptyLocations <- asks $ HashSet.map unEmptyLocationId . getSet ()
      revealedLocations <- asks $ HashSet.map unRevealedLocationId . getSet ()
      let
        candidateLocations =
          setToList $ emptyLocations `intersection` revealedLocations

      unshiftMessages
        $ [ DisengageEnemy iid enemyId | enemyId <- enemyIds ]
        <> [ Ask iid $ ChooseOne [ MoveTo iid lid | lid <- candidateLocations ]
           | not (null candidateLocations)
           ]
        <> [Discard (EventTarget eventId)]

      Elusive <$> runMessage msg (attrs & resolved .~ True)
    _ -> Elusive <$> runMessage msg attrs
