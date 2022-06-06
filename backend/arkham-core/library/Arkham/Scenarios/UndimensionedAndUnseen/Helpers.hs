module Arkham.Scenarios.UndimensionedAndUnseen.Helpers where

import Arkham.Prelude

import Arkham.Card ( Card )
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Matcher
import Arkham.Name

broodTitle :: Text
broodTitle = nameTitle . toName $ Cards.broodOfYogSothoth

getBroodOfYogSothoth :: GameT [EnemyId]
getBroodOfYogSothoth = selectList $ EnemyWithTitle broodTitle

getSetAsideBroodOfYogSothoth :: GameT [Card]
getSetAsideBroodOfYogSothoth =
  getSetAsideCardsMatching $ CardWithTitle broodTitle
