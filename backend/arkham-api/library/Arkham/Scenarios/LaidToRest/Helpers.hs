module Arkham.Scenarios.LaidToRest.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Helpers.Scenario (standaloneI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message (Message (ScenarioSpecific))
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "laidToRest" a

jimCulver :: InvestigatorMatcher
jimCulver = InvestigatorWithTitle "Jim Culver"

theBeyond :: AssetMatcher
theBeyond = assetIs Assets.theBeyondBleakNetherworld

cardsAttachedToTheBeyond :: (HasGame m, Tracing m) => m Int
cardsAttachedToTheBeyond =
  (+)
    <$> selectCount (AssetAttachedToAsset theBeyond)
    <*> selectCount (EnemyAttachedToAsset theBeyond)

hereticsStillInTheBeyond :: (HasGame m, Tracing m) => m Int
hereticsStillInTheBeyond = do
  drawnOut <- selectCount $ EnemyWithTitle "Heretic" <> not_ (EnemyAttachedToAsset theBeyond)
  unfinishedBusiness <- selectCount $ StoryWithTitle "Unfinished Business"
  banished <- selectCount $ VictoryDisplayCardMatch $ basic $ CardWithTitle "Unfinished Business"
  pure $ max 0 (4 - drawnOut - unfinishedBusiness - banished)

addToSpiritDeck :: ReverseQueue m => [Card] -> m ()
addToSpiritDeck cards = push $ ScenarioSpecific "theBeyond:addToSpiritDeck" (toJSON cards)

banishTopOfSpiritDeck :: ReverseQueue m => m ()
banishTopOfSpiritDeck = push $ ScenarioSpecific "theBeyond:banishTop" Null

returnSpiritToSpiritDeck :: ReverseQueue m => Card -> m ()
returnSpiritToSpiritDeck card = push $ ScenarioSpecific "theBeyond:returnSpirit" (toJSON card)
