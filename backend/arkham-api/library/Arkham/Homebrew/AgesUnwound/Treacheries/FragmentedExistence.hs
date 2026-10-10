module Arkham.Homebrew.AgesUnwound.Treacheries.FragmentedExistence (fragmentedExistence) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype FragmentedExistence = FragmentedExistence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Marked waiting for the same reason /Idle Hands/ is: the action ability pays
for itself with a discard, and the entity has to outlive that payment. The
revelation therefore discards the card itself on the paths where it does not
stay in play.
-}
fragmentedExistence :: TreacheryCard FragmentedExistence
fragmentedExistence = treacheryWith FragmentedExistence Cards.fragmentedExistence (waitingL .~ True)

{- | "[action] If Fragmented Existence is in your threat area, discard it: Evade.
Draw the top card of the encounter deck. Then, automatically evade each enemy
engaged with you."
-}
instance HasAbilities FragmentedExistence where
  getAbilities (FragmentedExistence a) =
    [restricted a 1 InYourThreatArea $ evadeAction $ DiscardCost FromPlay (toTarget a)]

instance RunMessage FragmentedExistence where
  runMessage msg t@(FragmentedExistence attrs) = runQueueT $ case msg of
    {- "Revelation - Test [willpower] (3). If you fail, take 2 damage. If you
    succeed and there is no copy of Fragmented Existence in your threat area, put
    Fragmented Existence into play in your threat area." -}
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      beginSkillTest sid iid attrs iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      toDiscardBy iid attrs attrs
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      alreadyHasOne <-
        selectAny $ treacheryInThreatAreaOf iid <> treacheryIs Cards.fragmentedExistence
      if alreadyHasOne
        then toDiscardBy iid attrs attrs
        else placeInThreatArea attrs iid
      pure t
    Do (AfterRevelation _ tid) | tid == attrs.id -> pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- The evades are counted after the draw resolves: the drawn card can engage
      -- another enemy, and "each enemy engaged with you" is read then.
      drawEncounterCard iid (attrs.ability 1)
      do_ msg
      pure $ overAttrs (waitingL .~ False) t
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      enemies <- select $ enemyEngagedWith iid <> not_ IsSwarm
      for_ enemies $ automaticallyEvadeEnemy iid
      pure t
    _ -> FragmentedExistence <$> liftRunMessage msg attrs
