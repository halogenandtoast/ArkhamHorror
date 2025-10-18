module Arkham.Campaign.Import.Lifted (module X, module Arkham.Campaign.Import.Lifted) where

import Arkham.Calculation as X
import Arkham.Campaign.Runner as X (
  CampaignAttrs,
  IsCampaign (..),
  campaign,
  campaignMeta,
  campaignStep,
  campaignWith,
  defaultCampaignRunner,
  logL,
  metaL,
  modifiersL,
  push,
  pushAll,
  pushWhen,
 )
import Arkham.CampaignStep as X
import Arkham.Classes as X
import Arkham.Difficulty as X
import Arkham.Helpers.Message as X (targetLabel)
import Arkham.Id as X
import Arkham.Message as X (Message (..), ShuffleIn (..), UI (..), pattern PlaceClues)
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Text as X

import Arkham.Message

nextCampaignStep :: ReverseQueue m => m ()
nextCampaignStep = push $ NextCampaignStep Nothing

setNextCampaignStep :: ReverseQueue m => CampaignStep -> m ()
setNextCampaignStep = push . NextCampaignStep . Just

interludeStepPart :: ReverseQueue m => Int -> Maybe InterludeKey -> Int -> m ()
interludeStepPart n mKey part = campaignStep_ (InterludeStepPart n mKey part)

prologueStepPart :: ReverseQueue m => Int -> m ()
prologueStepPart part = campaignStep_ (PrologueStepPart part)

campaignStep_ :: ReverseQueue m => CampaignStep -> m ()
campaignStep_ s = setNextCampaignStep s

overMeta :: forall a. (ToJSON a, FromJSON a) => (a -> a) -> (CampaignAttrs -> CampaignAttrs)
overMeta f = metaL %~ \m -> toJSON $ f $ toResult @a m
