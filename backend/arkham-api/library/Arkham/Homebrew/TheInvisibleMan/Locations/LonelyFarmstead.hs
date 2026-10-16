{- | Lonely Farmstead -- the farmers take you in, and you stay put.

Everything after the disengage is one round-long bundle of modifiers on the
investigator who parleyed. "Card effects" is read to include an ability that
targets an enemy, which is why 'CannotTriggerAbilityMatching' is in the bundle;
evading is neither an attack nor a card effect, so it stays legal.
-}
module Arkham.Homebrew.TheInvisibleMan.Locations.LonelyFarmstead (lonelyFarmstead) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LonelyFarmstead = LonelyFarmstead LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lonelyFarmstead :: LocationCard LonelyFarmstead
lonelyFarmstead = location LonelyFarmstead Cards.lonelyFarmstead 2 (PerPlayer 1)

instance HasAbilities LonelyFarmstead where
  getAbilities (LonelyFarmstead a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "lonelyFarmstead.shelter"
      $ restricted a 1 Here #parley

instance RunMessage LonelyFarmstead where
  runMessage msg l@(LonelyFarmstead attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifiers
        (attrs.ability 1)
        iid
        [ CannotBeEngaged
        , CannotFight AnyEnemy
        , CannotTriggerAbilityMatching (AbilityOnEnemy AnyEnemy)
        , CannotMove
        , CannotBeMoved
        ]
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      pure l
    _ -> LonelyFarmstead <$> liftRunMessage msg attrs
