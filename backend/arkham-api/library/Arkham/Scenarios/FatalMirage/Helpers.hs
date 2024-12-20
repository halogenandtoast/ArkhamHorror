module Arkham.Scenarios.FatalMirage.Helpers where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.RunMessage
import Arkham.Constants
import Arkham.GameT
import Arkham.GameValue
import Arkham.Helpers.Query (getLead, getSetAsideCardMaybe)
import Arkham.Helpers.Story
import Arkham.I18n
import Arkham.Location.Base (revealCluesL)
import Arkham.Location.Runner ()
import Arkham.Location.Types (LocationAttrs)
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message (Message (Flip, RevealLocation), is, pattern UseThisAbility)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Queue
import Arkham.Source
import Arkham.Target

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "fatalMirage" a

mirage
  :: (HasCardCode a, Sourceable a, HasCardCode location) => a -> Int -> [location] -> Ability
mirage a clues locations =
  restricted a MirageAbility (Here <> SetAsideCardExists (mapOneOf cardIs locations))
    $ FastAbility
    $ GroupClueCost (PerPlayer clues) YourLocation

mirageRunner
  :: CardDef -> [CardDef] -> Message -> LocationAttrs -> QueueT Message GameT LocationAttrs
mirageRunner storyCard mirageCards msg attrs = case msg of
  UseThisAbility iid (isSource attrs -> True) MirageAbility -> do
    flipOverBy iid (attrs.ability MirageAbility) attrs
    pure attrs
  Flip iid _ (isTarget attrs -> True) -> do
    readStory iid attrs storyCard
    pure attrs
  RevealLocation _ (is attrs -> True) | attrs.unrevealed -> do
    n <- length <$> mapMaybeM getSetAsideCardMaybe mirageCards
    liftRunMessage msg (attrs & revealCluesL .~ PerPlayer n)
  _ -> liftRunMessage msg attrs

mayAdvance :: (ReverseQueue m, Sourceable source) => source -> m ()
mayAdvance source = do
  lead <- getLead
  chooseOneM lead do
    labeled "Advance the current act" $ advanceCurrentAct source
    labeled "Keep playing" nothing
