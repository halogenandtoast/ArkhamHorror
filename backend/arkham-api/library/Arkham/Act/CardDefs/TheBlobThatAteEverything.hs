module Arkham.Act.CardDefs.TheBlobThatAteEverything where

import Arkham.Act.CardDefs.Import

exposeTheAnomalyEpicMultiplayer :: CardDef
exposeTheAnomalyEpicMultiplayer = act "85005" "Expose the Anomaly" 1 BlobEpicMultiplayer

exposeTheAnomaly :: CardDef
exposeTheAnomaly = act "85006" "Expose the Anomaly" 1 BlobSingleGroup

extraterrestrialPhysiology :: CardDef
extraterrestrialPhysiology =
  act "85007" "Extraterrestrial Physiology" 2 TheBlobThatAteEverything

blackwatersBaneEpicMultiplayer :: CardDef
blackwatersBaneEpicMultiplayer = act "85008" "Blackwater's Bane" 3 BlobEpicMultiplayer

blackwatersBane :: CardDef
blackwatersBane = act "85009" "Blackwater's Bane" 3 BlobSingleGroup
