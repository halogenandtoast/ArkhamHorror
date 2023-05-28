module Arkham.Scenarios.UndimensionedAndUnseen.Helpers where

import Arkham.Prelude

import Arkham.Card (Card)
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.Store

broodTitle :: Text
broodTitle = nameTitle . toName $ Cards.broodOfYogSothoth

getBroodOfYogSothoth :: (Store m Card, HasGame m) => m [EnemyId]
getBroodOfYogSothoth = selectList $ EnemyWithTitle broodTitle

getSetAsideBroodOfYogSothoth :: (Store m Card, HasGame m) => m [Card]
getSetAsideBroodOfYogSothoth =
  getSetAsideCardsMatching $ CardWithTitle broodTitle
