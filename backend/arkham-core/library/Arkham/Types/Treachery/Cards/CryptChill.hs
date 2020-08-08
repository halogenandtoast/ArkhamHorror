{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.CryptChill where

import Arkham.Json
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype CryptChill = CryptChill Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cryptChill :: TreacheryId -> CryptChill
cryptChill uuid = CryptChill $ baseAttrs uuid "01167"

instance (TreacheryRunner env) => RunMessage env CryptChill where
  runMessage msg t@(CryptChill attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> t <$ unshiftMessages
      [ RevelationSkillTest
        iid
        (TreacherySource tid)
        SkillWillpower
        4
        []
        [TreacheryFailure iid tid]
      , Discard (TreacheryTarget tid)
      ]
    TreacheryFailure iid tid | tid == treacheryId -> do
      assetCount <- HashSet.size <$> asks (getSet @AssetId iid)
      if assetCount > 0
        then t <$ unshiftMessage (ChooseAndDiscardAsset iid)
        else
          t <$ unshiftMessage
            (InvestigatorDamage iid (TreacherySource treacheryId) 2 0)
    _ -> CryptChill <$> runMessage msg attrs
