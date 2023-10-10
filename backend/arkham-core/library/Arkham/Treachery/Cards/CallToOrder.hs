module Arkham.Treachery.Cards.CallToOrder (
  callToOrder,
  CallToOrder (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Creation
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CallToOrder = CallToOrder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callToOrder :: TreacheryCard CallToOrder
callToOrder = treachery CallToOrder Cards.callToOrder

instance RunMessage CallToOrder where
  runMessage msg t@(CallToOrder attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      discards <- scenarioField ScenarioDiscard
      let cultists = take 2 $ filter (`cardMatch` CardWithTrait Cultist) discards
      if null cultists
        then push $ gainSurge attrs
        else do
          locations <- selectList $ LocationWithMostClues EmptyLocation
          choices <- for locations $ \location -> do
            creations <- traverse (\cultist -> createEnemy cultist (SpawnAtLocation location)) cultists
            pure $ targetLabel location $ map toMessage creations
          player <- getPlayer iid
          push $ chooseOrRunOne player choices
      pure t
    _ -> CallToOrder <$> runMessage msg attrs
