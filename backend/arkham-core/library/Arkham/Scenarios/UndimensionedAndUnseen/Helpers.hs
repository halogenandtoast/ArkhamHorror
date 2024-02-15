module Arkham.Scenarios.UndimensionedAndUnseen.Helpers where

import Arkham.Prelude

import Arkham.Card (Card)
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Matcher
import Arkham.Name

broodTitle :: Text
broodTitle = nameTitle . toName $ Cards.broodOfYogSothoth

getBroodOfYogSothoth :: HasGame m => m [EnemyId]
getBroodOfYogSothoth = select $ EnemyWithTitle broodTitle

getSetAsideBroodOfYogSothoth :: HasGame m => m [Card]
getSetAsideBroodOfYogSothoth = getSetAsideCardsMatching $ CardWithTitle broodTitle
