module Arkham.Homebrew.AgesUnwound.Locations.Sydney (sydney) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype Sydney = Sydney LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Sydney/ (@:ages-unwound:122@). Prints no location symbol and no
connections, so the only ways in are San Francisco's and Shanghai's move
abilities; the only way out is its own. It carries a fixed 4 clues, which its
ability puts back once they have all been taken.
-}
sydney :: LocationCard Sydney
sydney = location Sydney Cards.sydney 3 (Static 4)

{- | "[action][action]: __Move__ to San Francisco or Shanghai. / [action] If
there are no clues on Sydney: Place 4 clues on Sydney. Draw 2 cards and gain 2
resources. __Move__ to any other location."
-}
destinations :: LocationMatcher
destinations = mapOneOf locationIs [Cards.sanFrancisco, Cards.shanghai]

instance HasAbilities Sydney where
  getAbilities (Sydney a) =
    extendRevealed
      a
      [ campaignI18n
          $ withI18nTooltip "sydney.move"
          $ restricted a 1 (Here <> exists destinations)
          $ ActionAbility #move Nothing (ActionCost 2)
      , campaignI18n
          $ withI18nTooltip "sydney.replenish"
          $ restricted a 2 (Here <> thisExists a LocationWithoutClues) actionAbility
      ]

instance RunMessage Sydney where
  runMessage msg l@(Sydney attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lids <- select destinations
      chooseTargetM iid lids $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      placeClues (attrs.ability 2) attrs 4
      drawCards iid (attrs.ability 2) 2
      gainResources iid (attrs.ability 2) 2
      others <- select $ NotLocation (be attrs)
      chooseTargetM iid others $ moveTo (attrs.ability 2) iid
      pure l
    _ -> Sydney <$> liftRunMessage msg attrs
