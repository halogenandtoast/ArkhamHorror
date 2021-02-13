module Arkham.Types.Act.Cards.SaracenicScript
  ( SaracenicScript(..)
  , saracenicScript
  )
where

import Arkham.Prelude

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype SaracenicScript = SaracenicScript ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saracenicScript :: SaracenicScript
saracenicScript = SaracenicScript $ baseAttrs
  "02240"
  "Saracenic Script"
  (Act 1 A)
  (Just
  $ RequiredClues (PerPlayer 2) (Just $ LocationWithTitle "Whateley Ruins")
  )

instance ActionRunner env => HasActions env SaracenicScript where
  getActions i window (SaracenicScript x) = getActions i window x

instance ActRunner env => RunMessage env SaracenicScript where
  runMessage msg (SaracenicScript attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      whateleyRuins <- fromJustNote "must exist"
        <$> getLocationIdWithTitle "Whateley Ruins"
      investigatorIds <- getSetList whateleyRuins
      requiredClueCount <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ chooseOne iid [AdvanceAct aid (toSource attrs)]
          | iid <- investigatorIds
          ]
        )
      pure $ SaracenicScript $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> error "TODO"
    _ -> SaracenicScript <$> runMessage msg attrs
