module Arkham.Scenarios.UndimensionedAndUnseen.Helpers where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Card (Card)
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Name

broodTitle :: Text
broodTitle = nameTitle . toName $ Cards.broodOfYogSothoth

getBroodOfYogSothoth
  :: (MonadReader env m, Query EnemyMatcher env) => m [EnemyId]
getBroodOfYogSothoth = selectList $ EnemyWithTitle broodTitle

getSetAsideBroodOfYogSothoth
  :: (MonadReader env m, Query ExtendedCardMatcher env) => m [Card]
getSetAsideBroodOfYogSothoth =
  getSetAsideCardsMatching $ CardWithTitle broodTitle
