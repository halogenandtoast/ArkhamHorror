module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.GrandFoyer_011 (grandFoyer_011) where

import Arkham.Ability
import Arkham.Effect.Builder
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GrandFoyer_011 = GrandFoyer_011 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandFoyer_011 :: LocationCard GrandFoyer_011
grandFoyer_011 = symbolLabel $ location GrandFoyer_011 Cards.grandFoyer_011 3 (Static 0)

instance HasAbilities GrandFoyer_011 where
  getAbilities (GrandFoyer_011 a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "grandFoyer.resign" $ locationResignAction a
      , scenarioI18n
          $ withI18nTooltip "grandFoyer_011.parley"
          $ restricted a 1 Here parleyAction_
      ]

instance RunMessage GrandFoyer_011 where
  runMessage msg l@(GrandFoyer_011 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      effectWithSource (attrs.ability 1) attrs do
        enableOn
          $ EffectSkillTestMatchingWindow
          $ oneOf [#investigation, WhileAttacking, WhileEvading]
        removeOn EffectRoundWindow
        apply $ ShroudModifier (-2)
      pure l
    _ -> GrandFoyer_011 <$> liftRunMessage msg attrs
