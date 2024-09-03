module Arkham.Location.Cards.ChapultepecHill_178 (
  chapultepecHill_178,
  ChapultepecHill_178 (..),
) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype ChapultepecHill_178 = ChapultepecHill_178 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecHill_178 :: LocationCard ChapultepecHill_178
chapultepecHill_178 =
  locationWith
    ChapultepecHill_178
    Cards.chapultepecHill_178
    2
    (PerPlayer 2)
    (labelL .~ "triangle")

instance HasAbilities ChapultepecHill_178 where
  getAbilities (ChapultepecHill_178 attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (exists $ investigatorAt attrs.id)
          $ forced
          $ PutLocationIntoPlay #after Anyone (be attrs)
      , groupLimit PerRound
          $ restrictedAbility
            attrs
            2
            ( Here
                <> CluesOnThis (atLeast 1)
                <> CanDiscoverCluesAt (LocationWithId attrs.id)
                <> youExist (HandWith $ LengthIs $ atLeast 3)
            )
          $ actionAbilityWithCost DiscardHandCost
      ]

instance RunMessage ChapultepecHill_178 where
  runMessage msg l@(ChapultepecHill_178 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- select $ investigatorAt attrs.id
      for_ iids $ \iid ->
        pushAll
          [ toMessage $ randomDiscard iid (toAbilitySource attrs 1)
          , toMessage $ randomDiscard iid (toAbilitySource attrs 1)
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 2) 2
      pure l
    _ -> ChapultepecHill_178 <$> runMessage msg attrs
