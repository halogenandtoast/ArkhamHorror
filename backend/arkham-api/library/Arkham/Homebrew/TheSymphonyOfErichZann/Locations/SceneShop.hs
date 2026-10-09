module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.SceneShop (sceneShop) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SceneShop = SceneShop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sceneShop :: LocationCard SceneShop
sceneShop =
  locationWith SceneShop Cards.sceneShop 3 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 1) Anywhere

instance HasAbilities SceneShop where
  getAbilities (SceneShop a) =
    extend
      a
      [ -- "After you enter Scene Shop, you cannot move for the remainder of the turn."
        mkAbility a 1 $ forced $ Moves #after You AnySource Anywhere (be a)
      , {- "After you successfully evade an enemy at this location: Do not ready
        that enemy during the upkeep phase this round." -}
        restricted a 2 Here $ freeReaction $ EnemyEvaded #after You (enemyAt a.id)
      ]

instance RunMessage SceneShop where
  runMessage msg l@(SceneShop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier iid (attrs.ability 1) iid CannotMove
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach (enemyAt attrs.id) \eid ->
        roundModifier (attrs.ability 2) eid DoesNotReadyDuringUpkeep
      pure l
    _ -> SceneShop <$> liftRunMessage msg attrs
