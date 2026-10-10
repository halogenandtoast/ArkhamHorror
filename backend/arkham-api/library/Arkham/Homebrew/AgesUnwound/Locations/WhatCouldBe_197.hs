module Arkham.Homebrew.AgesUnwound.Locations.WhatCouldBe_197 (whatCouldBe_197) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (ConnectedToWhen))
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher

newtype WhatCouldBe_197 = WhatCouldBe_197 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatCouldBe_197 :: LocationCard WhatCouldBe_197
whatCouldBe_197 =
  symbolLabel
    $ locationWith WhatCouldBe_197 Cards.whatCouldBe_197 5 (PerPlayer 1)
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
instance HasAbilities WhatCouldBe_197 where
  getAbilities (WhatCouldBe_197 a) = extendRevealed1 a $ mkAbility a 1 actionAbility

instance RunMessage WhatCouldBe_197 where
  runMessage msg l@(WhatCouldBe_197 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs (ConnectedToWhen (be attrs) Anywhere)
      selectEach (Anywhere <> not_ (be attrs)) \lid ->
        roundModifier (attrs.ability 1) lid (ConnectedToWhen Anywhere (be attrs))
      pure l
    _ -> WhatCouldBe_197 <$> liftRunMessage msg attrs
