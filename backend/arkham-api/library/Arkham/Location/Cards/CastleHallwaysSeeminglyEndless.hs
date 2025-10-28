module Arkham.Location.Cards.CastleHallwaysSeeminglyEndless (castleHallwaysSeeminglyEndless) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Trait (Trait (Castle))

newtype CastleHallwaysSeeminglyEndless = CastleHallwaysSeeminglyEndless LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

castleHallwaysSeeminglyEndless :: LocationCard CastleHallwaysSeeminglyEndless
castleHallwaysSeeminglyEndless = location CastleHallwaysSeeminglyEndless Cards.castleHallwaysSeeminglyEndless 4 (PerPlayer 1)

instance HasAbilities CastleHallwaysSeeminglyEndless where
  getAbilities (CastleHallwaysSeeminglyEndless a) =
    extend a
      $ if a.revealed
        then
          [ scenarioI18n $ hauntedI "castleHallways.haunted" a 2
          , restricted
              a
              3
              (Here <> exists (LocationWithTrait Castle <> RevealedLocation <> not_ (be a)) <> youExist can.move)
              $ FastAbility (ResourceCost 1)
          ]
        else
          [ restricted a 1 (exists $ enemyIs Enemies.theContessaNeedlesslySmug <> EnemyCanMove)
              $ forced
              $ UnrevealedRevealLocation #when You (be a)
          ]

instance RunMessage CastleHallwaysSeeminglyEndless where
  runMessage msg l@(CastleHallwaysSeeminglyEndless attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      moveContessa (attrs.ability 1) attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hasCards <-
        selectAny
          $ inHandOf NotForPlay iid
          <> basic (DiscardableCard <> mapOneOf CardWithSkillIcon [#intellect, #wild])
      if hasCards
        then chooseAndDiscardCardEdit iid (attrs.ability 2) \d -> d {discardFilter = mapOneOf CardWithSkillIcon [#intellect, #wild]}
        else moveContessa (attrs.ability 2) attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      locations <- select (LocationWithTrait Castle <> not_ (be attrs) <> RevealedLocation)
      chooseTargetM iid locations $ moveTo (attrs.ability 3) iid
      pure l
    _ -> CastleHallwaysSeeminglyEndless <$> liftRunMessage msg attrs
