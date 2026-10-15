module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Acts.AncestralCorruption (ancestralCorruption) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Geist))

newtype AncestralCorruption = AncestralCorruption ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancestralCorruption :: ActCard AncestralCorruption
ancestralCorruption = act (3, A) AncestralCorruption Cards.ancestralCorruption Nothing

{- | "After a [[Geist]] card enters the encounter discard pile".

A defeated enemy only raises @EntityDiscarded@; a treachery, an encounter asset
and a milled card all raise @Discarded@, so the two halves do not overlap.
-}
geistEntersEncounterDiscard :: WindowMatcher
geistEntersEncounterDiscard =
  oneOf
    [ EnemyDiscarded #after AnySource (EnemyWithTrait Geist)
    , Discarded #after Nothing AnySource (basic $ CardWithTrait Geist <> IsEncounterCard)
    ]

instance HasAbilities AncestralCorruption where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (exists $ You <> at_ (locationIs Locations.theTarn))
        $ parleyAction (clueCost 1)
    , restricted a 2 (exists $ enemyIs Enemies.theThingFromTheTarn)
        $ freeReaction geistEntersEncounterDiscard
    , mkAbility a 3 $ Objective $ forced $ ifEnemyDefeated Enemies.theThingFromTheTarn
    ]

instance RunMessage AncestralCorruption where
  runMessage msg a@(AncestralCorruption attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfEncounterDeck iid (attrs.ability 1) 3
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectEach (enemyIs Enemies.theThingFromTheTarn)
        $ nonAttackEnemyDamage (Just iid) (attrs.ability 2) 2
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> AncestralCorruption <$> liftRunMessage msg attrs
