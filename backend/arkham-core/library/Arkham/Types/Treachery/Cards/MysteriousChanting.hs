{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.MysteriousChanting where

import Arkham.Import hiding (Cultist)

import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype MysteriousChanting = MysteriousChanting Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mysteriousChanting :: TreacheryId -> a -> MysteriousChanting
mysteriousChanting uuid _ = MysteriousChanting $ baseAttrs uuid "01171"

instance HasModifiersFor env MysteriousChanting where
  getModifiersFor = noModifiersFor

instance HasActions env MysteriousChanting where
  getActions i window (MysteriousChanting attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env MysteriousChanting where
  runMessage msg (MysteriousChanting attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      enemies <- map unClosestEnemyId <$> getSetList (lid, [Cultist])
      case enemies of
        [] -> unshiftMessage
          (FindAndDrawEncounterCard
            iid
            (EncounterCardMatchByType (EnemyType, Just Cultist))
          )
        xs -> unshiftMessage
          (Ask iid $ ChooseOne [ PlaceDoom (EnemyTarget eid) 2 | eid <- xs ])
      MysteriousChanting <$> runMessage msg (attrs & resolved .~ True)
    _ -> MysteriousChanting <$> runMessage msg attrs
