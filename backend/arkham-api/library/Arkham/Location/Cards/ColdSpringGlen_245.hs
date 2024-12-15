module Arkham.Location.Cards.ColdSpringGlen_245 (coldSpringGlen_245, ColdSpringGlen_245 (..)) where

import Arkham.Ability
import Arkham.Classes.HasQueue (replaceMessageMatching)
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (coldSpringGlen_245)
import Arkham.Location.Import.Lifted hiding (ChosenRandomLocation)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose

newtype ColdSpringGlen_245 = ColdSpringGlen_245 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpringGlen_245 :: LocationCard ColdSpringGlen_245
coldSpringGlen_245 = location ColdSpringGlen_245 Cards.coldSpringGlen_245 2 (Static 0)

instance HasModifiersFor ColdSpringGlen_245 where
  getModifiersFor (ColdSpringGlen_245 attrs) = do
    modifySelect attrs (enemyAt attrs) [EnemyEvade (-1)]

instance HasAbilities ColdSpringGlen_245 where
  getAbilities (ColdSpringGlen_245 attrs) =
    extendRevealed1 attrs
      $ skillTestAbility
      $ groupLimit PerWindow
      $ restricted attrs 1 (exists $ investigatorAt attrs)
      $ freeReaction
      $ ChosenRandomLocation #after (be attrs)

instance RunMessage ColdSpringGlen_245 where
  runMessage msg l@(ColdSpringGlen_245 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ investigatorAt attrs
      sid <- getRandom
      chooseOneM iid do
        targets investigators \iid' ->
          beginSkillTest sid iid' (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      lift $ replaceMessageMatching
        \case
          Msg.ChosenRandomLocation _ lid -> lid == toId attrs
          _ -> False
        \case
          Msg.ChosenRandomLocation target lid
            | lid == toId attrs -> [ChooseRandomLocation target (singleton lid)]
          _ -> error "should be the matching message"
      pure l
    _ -> ColdSpringGlen_245 <$> liftRunMessage msg attrs
