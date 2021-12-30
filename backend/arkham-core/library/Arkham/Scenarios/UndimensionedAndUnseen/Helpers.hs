module Arkham.Scenarios.UndimensionedAndUnseen.Helpers where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Card (Card)
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Name

broodTitle :: Text
broodTitle = nameTitle . toName $ Cards.broodOfYogSothoth

getBroodOfYogSothoth
  :: (MonadReader env m, Query EnemyMatcher env) => m [EnemyId]
getBroodOfYogSothoth = selectList $ EnemyWithTitle broodTitle

getSetAsideBroodOfYogSothoth
  :: (MonadReader env m, Query ExtendedCardMatcher env) => m [Card]
getSetAsideBroodOfYogSothoth =
  getSetAsideCardsMatching $ CardWithTitle broodTitle
