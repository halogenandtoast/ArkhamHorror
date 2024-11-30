module Arkham.Location.Cards.ColdSpringGlen_245 (coldSpringGlen_245, ColdSpringGlen_245 (..)) where

import Arkham.Ability
import Arkham.Classes.HasQueue (replaceMessageMatching)
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (coldSpringGlen_245)
import Arkham.Location.Import.Lifted hiding (ChosenRandomLocation)
import Arkham.Location.Runner (enemyAtLocation)
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype ColdSpringGlen_245 = ColdSpringGlen_245 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldSpringGlen_245 :: LocationCard ColdSpringGlen_245
coldSpringGlen_245 = location ColdSpringGlen_245 Cards.coldSpringGlen_245 2 (Static 0)

instance HasModifiersFor ColdSpringGlen_245 where
  getModifiersFor (EnemyTarget eid) (ColdSpringGlen_245 attrs) = do
    atLocation <- enemyAtLocation eid attrs
    toModifiers attrs [EnemyEvade (-1) | atLocation]
  getModifiersFor _ _ = pure []

instance HasAbilities ColdSpringGlen_245 where
  getAbilities (ColdSpringGlen_245 attrs) =
    extendRevealed1 attrs
      $ skillTestAbility
      $ groupLimit PerWindow
      $ mkAbility attrs 1
      $ freeReaction
      $ ChosenRandomLocation #after (be attrs)

instance RunMessage ColdSpringGlen_245 where
  runMessage msg l@(ColdSpringGlen_245 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- genId
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
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
