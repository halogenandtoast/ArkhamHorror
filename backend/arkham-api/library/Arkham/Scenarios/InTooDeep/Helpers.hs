module Arkham.Scenarios.InTooDeep.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.ChaosToken
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (select)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (story)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.SortedPair
import Data.Map.Strict qualified as Map

newtype Meta = Meta {barriers :: Map (SortedPair LocationId) Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

removeBarrierBetweenConnected :: ReverseQueue m => InvestigatorId -> LocationId -> m ()
removeBarrierBetweenConnected iid lid = do
  mods <- getModifiers lid
  let barriers = concat [ls | Barricades ls <- mods]
  chooseTargetM iid barriers \lid' -> push $ ScenarioCountDecrementBy (Barriers lid lid') 1

placeBarrier :: ReverseQueue m => LocationId -> LocationId -> m ()
placeBarrier l1 l2 = push $ ScenarioCountIncrementBy (Barriers l1 l2) 1

insertBarrier :: LocationId -> LocationId -> Meta -> Meta
insertBarrier = incrementBarriers 1

removeBarrier :: LocationId -> LocationId -> Meta -> Meta
removeBarrier = decrementBarriers 1

incrementBarriers :: Int -> LocationId -> LocationId -> Meta -> Meta
incrementBarriers n a b (Meta barriers) =
  Meta $ Map.insertWith (+) (sortedPair a b) n barriers

decrementBarriers :: Int -> LocationId -> LocationId -> Meta -> Meta
decrementBarriers n a b (Meta barriers) =
  Meta $ Map.insertWith ((max 0 .) . subtract) (sortedPair a b) n barriers

setBarriers :: LocationId -> LocationId -> Int -> Meta -> Meta
setBarriers a b n (Meta barriers) =
  Meta $ Map.insert (sortedPair a b) n barriers

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "inTooDeep" a

data Flashback = Flashback5 | Flashback6 | Flashback7 | Flashback8

flashback :: ReverseQueue m => InvestigatorId -> Flashback -> m ()
flashback iid f = case f of
  Flashback5 -> do
    scenarioI18n $ story $ i18nWithTitle "flashback5"
    recoverMemory ADealWithJoeSargent
    iids <- select $ InvestigatorAt "Innsmouth Square"
    joe <- getSetAsideCard Assets.joeSargentRattletrapBusDriver
    chooseTargetM iid iids (`takeControlOfSetAsideAsset` joe)
  Flashback6 -> do
    scenarioI18n $ story $ i18nWithTitle "flashback6"
    recoverMemory AFollowedLead
    iids <- select $ InvestigatorAt "The Little Bookshop"
    teachings <- getSetAsideCard Assets.teachingsOfTheOrder
    chooseOrRunOneM iid do
      labeled "Do not take Teachings of the Order" nothing
      targets iids \iid' -> do
        addCampaignCardToDeck iid' Assets.teachingsOfTheOrder
        takeControlOfSetAsideAsset iid' teachings
  Flashback7 -> do
    scenarioI18n $ story $ i18nWithTitle "flashback7"
    recoverMemory AnIntervention
    addChaosToken PlusOne
  Flashback8 -> do
    scenarioI18n $ story $ i18nWithTitle "flashback8"
    recoverMemory AJailbreak
    chooseOneM iid do
      for_ [Cultist, Tablet, ElderThing] \face ->
        labeled ("Remove " <> toDisplay face) $ removeChaosToken face
