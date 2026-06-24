module Arkham.Location.Cards.SlimyStreets (slimyStreets) where

import Arkham.Ability
import Arkham.Helpers.Window.Clue
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SlimyStreets = SlimyStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slimyStreets :: LocationCard SlimyStreets
slimyStreets = locationWith SlimyStreets Cards.slimyStreets 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities SlimyStreets where
  getAbilities (SlimyStreets a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage SlimyStreets where
  runMessage msg l@(SlimyStreets attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      requestChaosTokens iid (attrs.ability 1) n
      pure l
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) (map (.face) -> tokens) -> do
      let triggering = count (`elem` [#skull, #autofail]) tokens
      continue_ iid
      replicateM_ triggering
        $ withI18n
        $ chooseOrRunOneM iid do
          countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
          countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      resetChaosTokens (attrs.ability 1)
      pure l
    _ -> SlimyStreets <$> liftRunMessage msg attrs
