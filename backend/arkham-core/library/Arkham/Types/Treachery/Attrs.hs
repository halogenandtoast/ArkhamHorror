{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Attrs where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

data Attrs = Attrs
  { treacheryName :: Text
  , treacheryId :: TreacheryId
  , treacheryCardCode :: CardCode
  , treacheryTraits :: HashSet Trait
  , treacheryAttachedLocation :: Maybe LocationId
  , treacheryAttachedInvestigator :: Maybe InvestigatorId
  , treacheryWeakness :: Bool
  , treacheryResolved :: Bool
  , treacheryDoom :: Int
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "treachery"
  toEncoding = genericToEncoding $ aesonOptions $ Just "treachery"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "treachery"

resolved :: Lens' Attrs Bool
resolved = lens treacheryResolved $ \m x -> m { treacheryResolved = x }

attachedLocation :: Lens' Attrs (Maybe LocationId)
attachedLocation =
  lens treacheryAttachedLocation $ \m x -> m { treacheryAttachedLocation = x }

attachedInvestigator :: Lens' Attrs (Maybe InvestigatorId)
attachedInvestigator = lens treacheryAttachedInvestigator
  $ \m x -> m { treacheryAttachedInvestigator = x }


baseAttrs :: TreacheryId -> CardCode -> Attrs
baseAttrs tid cardCode =
  let
    MkEncounterCard {..} =
      fromJustNote
          "missing encounter card"
          (HashMap.lookup cardCode allEncounterCards)
        $ CardId (unTreacheryId tid)
  in
    Attrs
      { treacheryName = ecName
      , treacheryId = tid
      , treacheryCardCode = ecCardCode
      , treacheryTraits = HashSet.fromList ecTraits
      , treacheryAttachedLocation = Nothing
      , treacheryAttachedInvestigator = Nothing
      , treacheryWeakness = False
      , treacheryResolved = False
      , treacheryDoom = 0
      }

weaknessAttrs :: TreacheryId -> CardCode -> Attrs
weaknessAttrs tid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          "missing weakness card"
          (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unTreacheryId tid)
  in
    Attrs
      { treacheryName = pcName
      , treacheryId = tid
      , treacheryCardCode = pcCardCode
      , treacheryTraits = HashSet.fromList pcTraits
      , treacheryAttachedLocation = Nothing
      , treacheryAttachedInvestigator = Nothing
      , treacheryWeakness = True
      , treacheryResolved = False
      , treacheryDoom = 0
      }

instance HasActions env investigator Attrs where
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    InvestigatorEliminated iid | treacheryAttachedInvestigator == Just iid ->
      a <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    Revelation{} -> pure $ a & resolved .~ True
    AfterRevelation{} -> a <$ unless
      treacheryResolved
      (unshiftMessage (Discard (TreacheryTarget treacheryId)))
    _ -> pure a
