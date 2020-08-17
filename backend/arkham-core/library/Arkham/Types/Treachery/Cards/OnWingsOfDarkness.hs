{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.OnWingsOfDarkness where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype OnWingsOfDarkness = OnWingsOfDarkness Attrs
  deriving newtype (Show, ToJSON, FromJSON)

onWingsOfDarkness :: TreacheryId -> OnWingsOfDarkness
onWingsOfDarkness uuid = OnWingsOfDarkness $ baseAttrs uuid "01173"

instance (TreacheryRunner env) => RunMessage env OnWingsOfDarkness where
  runMessage msg (OnWingsOfDarkness attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      centralLocations <- HashSet.toList <$> asks (getSet [Central])
      unshiftMessages
        [ RevelationSkillTest
          iid
          (TreacherySource treacheryId)
          SkillAgility
          4
          []
          ([ InvestigatorAssignDamage iid (TreacherySource tid) 1 1
           , UnengageNonMatching iid [Nightgaunt]
           ]
          <> [ Ask iid $ ChooseOne
                 [ MoveTo iid lid | lid <- centralLocations ]
             ]
          )
        , Discard (TreacheryTarget tid)
        ]
      OnWingsOfDarkness <$> runMessage msg attrs
    _ -> OnWingsOfDarkness <$> runMessage msg attrs
