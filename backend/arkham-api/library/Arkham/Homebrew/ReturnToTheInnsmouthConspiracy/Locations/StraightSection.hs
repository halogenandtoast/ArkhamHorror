module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.StraightSection (straightSection) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Helpers.Window
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheInnsmouthConspiracy.HorrorInHighGear.Helpers
import Arkham.Trait (Trait (Vehicle))

newtype StraightSection = StraightSection LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

straightSection :: LocationCard StraightSection
straightSection =
  locationWith StraightSection Cards.straightSection 1 (Static 0)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities StraightSection where
  getAbilities (StraightSection a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ EnemyEnters #after (be a) (EnemyWithTrait Vehicle)
      ]

{- | Unlike Mud Track, this one does use the bold **Vehicle** trait, so it works on
enemy vehicles (designer's FAQ).
-}
instance RunMessage StraightSection where
  runMessage msg l@(StraightSection attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      road 1 attrs
      -- "Reveal the location ahead of this one."
      select (LocationInDirection RightOf (be attrs)) >>= traverse_ reveal
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 (enteringEnemy -> eid) _ -> do
      whenM (eid <=~> UnengagedEnemy) $ push $ HunterMove eid
      pure l
    _ -> StraightSection <$> liftRunMessage msg attrs
