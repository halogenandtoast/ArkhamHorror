module Arkham.Location.Cards.BurialGround (burialGround, BurialGround (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey (ScenarioLogKey (NoticedTheMissingBones))
import Arkham.Trait (Trait (Ghoul))

newtype BurialGround = BurialGround LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burialGround :: LocationCard BurialGround
burialGround = location BurialGround Cards.burialGround 4 (PerPlayer 1)

instance HasModifiersFor BurialGround where
  getModifiersFor (EnemyTarget eid) (BurialGround attrs) = do
    isGhoul <- eid <=~> EnemyWithTrait Ghoul
    pure $ toModifiers attrs [ForceSpawnLocation (LocationWithId $ toId attrs) | isGhoul]
  getModifiersFor _ _ = pure []

instance HasAbilities BurialGround where
  getAbilities (BurialGround attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 Here
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)
      ]

instance RunMessage BurialGround where
  runMessage msg l@(BurialGround attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Remember NoticedTheMissingBones
      pure l
    _ -> BurialGround <$> runMessage msg attrs
