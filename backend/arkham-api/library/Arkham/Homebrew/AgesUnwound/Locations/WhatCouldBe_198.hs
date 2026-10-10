module Arkham.Homebrew.AgesUnwound.Locations.WhatCouldBe_198 (whatCouldBe_198) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (ConnectedToWhen))
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher

newtype WhatCouldBe_198 = WhatCouldBe_198 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatCouldBe_198 :: LocationCard WhatCouldBe_198
whatCouldBe_198 =
  symbolLabel
    $ locationWith WhatCouldBe_198 Cards.whatCouldBe_198 5 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "[action]: Until the end of the round, What Could Be is connected to each
other location, and vice versa. Investigators at any location can activate this
ability."

"Investigators at any location" is why there is no @Here@ on the ability.
"And vice versa" is the second half of the connection: 'ConnectedToWhen' is read
off the location it modifies, so one modifier on this place and one on every
other is what makes the edge two-way.
-}
instance HasAbilities WhatCouldBe_198 where
  getAbilities (WhatCouldBe_198 a) = extendRevealed1 a $ mkAbility a 1 actionAbility

instance RunMessage WhatCouldBe_198 where
  runMessage msg l@(WhatCouldBe_198 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs (ConnectedToWhen (be attrs) Anywhere)
      selectEach (Anywhere <> not_ (be attrs)) \lid ->
        roundModifier (attrs.ability 1) lid (ConnectedToWhen Anywhere (be attrs))
      pure l
    _ -> WhatCouldBe_198 <$> liftRunMessage msg attrs
