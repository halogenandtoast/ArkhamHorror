{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.MysteriousChanting where

import Arkham.Json
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype MysteriousChanting = MysteriousChanting Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mysteriousChanting :: TreacheryId -> a -> MysteriousChanting
mysteriousChanting uuid _ = MysteriousChanting $ baseAttrs uuid "01171"

instance HasActions env investigator MysteriousChanting where
  getActions i window (MysteriousChanting attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env MysteriousChanting where
  runMessage msg (MysteriousChanting attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      lid <- asks (getId @LocationId iid)
      enemies <- map unClosestEnemyId . HashSet.toList <$> asks
        (getSet (lid, [Cultist]))
      case enemies of
        [] -> unshiftMessage
          (FindAndDrawEncounterCard iid (EnemyType, Just Cultist))
        xs -> unshiftMessage
          (Ask iid $ ChooseOne [ PlaceDoom (EnemyTarget eid) 2 | eid <- xs ])
      MysteriousChanting <$> runMessage msg (attrs & resolved .~ True)
    _ -> MysteriousChanting <$> runMessage msg attrs
