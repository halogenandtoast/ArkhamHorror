module Arkham.Homebrew.AgesUnwound.Treacheries.Dystopia (dystopia) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Ally))
import Arkham.Treachery.Import.Lifted

newtype Dystopia = Dystopia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dystopia :: TreacheryCard Dystopia
dystopia = treachery Dystopia Cards.dystopia

{- | "Forced - At the end of your turn: Test [willpower] (X), where X is the
amount of doom in play. If you fail, take 1 horror, which must be assigned to an
[[Ally]] asset if able."
-}
instance HasAbilities Dystopia where
  getAbilities (Dystopia a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage Dystopia where
  runMessage msg t@(Dystopia attrs) = runQueueT $ case msg of
    -- "Revelation - Put Dystopia into play in your threat area."
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower DoomCountCalculation
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- "which must be assigned to an [[Ally]] asset if able": the assignment
      -- strategy, not a choice, so an investigator with an Ally cannot soak it
      -- themselves.
      push
        $ InvestigatorAssignDamage
          iid
          (toSource $ attrs.ability 1)
          (DamageAssetsFirst $ AssetWithTrait Ally)
          0
          1
      pure t
    _ -> Dystopia <$> liftRunMessage msg attrs
