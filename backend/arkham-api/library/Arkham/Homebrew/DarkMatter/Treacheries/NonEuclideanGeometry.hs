module Arkham.Homebrew.DarkMatter.Treacheries.NonEuclideanGeometry (nonEuclideanGeometry) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (switchedEventForInvestigator)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype NonEuclideanGeometry = NonEuclideanGeometry TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nonEuclideanGeometry :: TreacheryCard NonEuclideanGeometry
nonEuclideanGeometry = treachery NonEuclideanGeometry Cards.nonEuclideanGeometry

{- | "Revelation - Put this card into play in your threat area.
Forced - After your location is switched with another location: Take 1 damage.
[action]: Test [agility] (3) or [willpower] (3). If you succeed, discard
Non-Euclidean Geometry."
-}
instance HasAbilities NonEuclideanGeometry where
  getAbilities (NonEuclideanGeometry a) =
    -- the bearer's location is not knowable here, so this matches the
    -- per-investigator window the scenario fires for everyone standing at
    -- either of the two switched locations
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ ScenarioEvent #after (Just You) switchedEventForInvestigator
    , skillTestAbility $ restricted a 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage NonEuclideanGeometry where
  runMessage msg t@(NonEuclideanGeometry attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid $ for_ [#agility, #willpower] \skill ->
        skillLabeled skill $ beginSkillTest sid iid (attrs.ability 2) iid skill (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> NonEuclideanGeometry <$> liftRunMessage msg attrs
