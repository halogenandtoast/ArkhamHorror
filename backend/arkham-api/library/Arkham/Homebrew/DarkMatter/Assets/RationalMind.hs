module Arkham.Homebrew.DarkMatter.Assets.RationalMind (rationalMind) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Trait (Trait (Science))

newtype RationalMind = RationalMind AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rationalMind :: AssetCard RationalMind
rationalMind = asset RationalMind Cards.rationalMind

{- | "While Rational Mind has... 1 or more evidence, you get +1 [willpower] ...2
or more evidence, you get +1 [intellect] ...3 or more evidence, you get +1
sanity."

"Evidence" here is resource tokens placed by ability 1, not uses.
-}
instance HasModifiersFor RationalMind where
  getModifiersFor (RationalMind a) =
    controllerGets a
      $ [SkillModifier #willpower 1 | evidence >= 1]
      <> [SkillModifier #intellect 1 | evidence >= 2]
      <> [SanityModifier 1 | evidence >= 3]
   where
    evidence = a.token #resource

{- | "[reaction] When you play a Science card: Place 1 resource (from the token
pool) on this card, as evidence."
-}
instance HasAbilities RationalMind where
  getAbilities (RationalMind a) =
    [controlled_ a 1 $ freeReaction (Matcher.PlayCard #when You $ basic $ withTrait Science)]

instance RunMessage RationalMind where
  runMessage msg a@(RationalMind attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      pure a
    _ -> RationalMind <$> liftRunMessage msg attrs
