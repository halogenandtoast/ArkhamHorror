module Arkham.Homebrew.DarkMatter.Locations.Airlocks (airlocks) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Runner (pattern UseResign)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Crew))

newtype Airlocks = Airlocks LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

airlocks :: LocationCard Airlocks
airlocks = location Airlocks Cards.airlocks 1 (PerPlayer 2)

{- | "[free] 'Get to the Tatterdemalion!': Add a [[Crew]] story asset you control
to the victory display.
[action]: Resign. You should never have come. Remove all [[Crew]] story assets
you control from the game."
-}
instance HasAbilities Airlocks where
  getAbilities (Airlocks a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> exists (AssetWithTrait Crew <> AssetControlledBy You))
          $ FastAbility Free
      , locationResignAction a
      ]

instance RunMessage Airlocks where
  runMessage msg l@(Airlocks attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      crew <- select $ AssetWithTrait Crew <> assetControlledBy iid
      chooseOrRunOneM iid $ targets crew (addToVictory iid)
      pure l
    -- "Resign. You should never have come. Remove all [[Crew]] story assets you
    -- control from the game."
    UseResign iid (isSource attrs -> True) -> do
      crew <- select $ AssetWithTrait Crew <> assetControlledBy iid
      for_ crew \aid -> push $ RemoveFromGame (AssetTarget aid)
      Airlocks <$> liftRunMessage msg attrs
    _ -> Airlocks <$> liftRunMessage msg attrs
