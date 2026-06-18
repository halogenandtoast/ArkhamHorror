module Arkham.Scenarios.FateOfTheVale.Helpers where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (ActCard))
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act (getCurrentAct)
import Arkham.Helpers.Window (wouldDo)
import Arkham.I18n
import Arkham.Id (InvestigatorId)
import Arkham.Message (Message)
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Projection
import Arkham.Window qualified as Window

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "fateOfTheVale" a

residentEnemyDef :: Resident -> CardDef
residentEnemyDef = \case
  WilliamHemlock -> Enemies.williamHemlock
  RiverHawthorne -> Enemies.riverHawthorne
  MotherRachel -> Enemies.motherRachelStarbornHerald
  SimeonAtwood -> Enemies.simeonAtwood
  LeahAtwood -> Enemies.leahAtwood
  TheoPeters -> Enemies.theoPeters
  GideonMizrah -> Enemies.gideonMizrah
  JudithPark -> Enemies.judithPark

whenFateOfTheValeV4 :: ReverseQueue m => m () -> m ()
whenFateOfTheValeV4 body = do
  act <- getCurrentAct
  actCard <- field ActCard act
  when (toCardCode actCard == toCardCode Acts.fateOfTheValeV4) body

{- | Route a card being drawn or revealed from The Abyss through a cancellable
window so cards such as Old Memory can react before it resolves. @resolveMsg@ is
performed unless the batch is cancelled; because the generic @DoBatch@ runner
only unwraps 'Run' and message-specific handlers, @resolveMsg@ must be a 'Run'
or a message with a matching @DoBatch _ msg@ handler. @tag@ distinguishes the
draw context (e.g. "mythos" vs "scenario") so reactors can resolve a redirect
the right way.
-}
abyssDrawWindow :: ReverseQueue m => Text -> InvestigatorId -> Card -> Message -> m ()
abyssDrawWindow tag iid card resolveMsg =
  wouldDo
    resolveMsg
    (Window.ScenarioEvent ("wouldDrawFromAbyss:" <> tag) (Just iid) (toJSON card))
    (Window.ScenarioEvent ("drewFromAbyss:" <> tag) (Just iid) (toJSON card))
