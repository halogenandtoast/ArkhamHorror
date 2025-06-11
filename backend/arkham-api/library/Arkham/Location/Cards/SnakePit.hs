module Arkham.Location.Cards.SnakePit (snakePit) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDoomOfEztli.Helpers

newtype SnakePit = SnakePit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snakePit :: LocationCard SnakePit
snakePit =
  location SnakePit Cards.snakePit 1 (PerPlayer 1)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities SnakePit where
  getAbilities (SnakePit a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ LocationEntersPlay #after (be a)

instance RunMessage SnakePit where
  runMessage msg l@(SnakePit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- min 3 <$> perPlayer 1
      vipers <- take n <$> getSetAsideCardsMatching (cardIs Enemies.pitViper)
      for_ vipers (`createEnemyAt` attrs.id)
      hasBinoculars <- selectAny $ InvestigatorWithSupply Binoculars <> at_ (locationWithInvestigator iid)

      when hasBinoculars do
        chooseOneM iid $ scenarioI18n do
          labeled' "snakePit.doNotMove" $ cancelMovement (attrs.ability 1) iid
          labeled' "snakePit.move" nothing

      pure l
    _ -> SnakePit <$> liftRunMessage msg attrs
