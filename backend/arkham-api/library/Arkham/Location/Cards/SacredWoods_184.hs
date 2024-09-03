module Arkham.Location.Cards.SacredWoods_184 (sacredWoods_184, SacredWoods_184 (..)) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection

newtype SacredWoods_184 = SacredWoods_184 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacredWoods_184 :: LocationCard SacredWoods_184
sacredWoods_184 = locationWith SacredWoods_184 Cards.sacredWoods_184 4 (PerPlayer 1) (labelL .~ "star")

instance HasAbilities SacredWoods_184 where
  getAbilities (SacredWoods_184 attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (exists $ investigatorAt attrs.id)
          $ forced
          $ PutLocationIntoPlay #after Anyone (be attrs)
      , restrictedAbility
          attrs
          2
          ( Here
              <> youExist DeckIsEmpty
              <> CluesOnThis (atLeast 1)
              <> CanDiscoverCluesAt (LocationWithId attrs.id)
          )
          actionAbility
      ]

instance RunMessage SacredWoods_184 where
  runMessage msg l@(SacredWoods_184 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- select $ investigatorAt attrs
      pushAll [DiscardTopOfDeck iid 10 (attrs.ability 1) Nothing | iid <- iids]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- field LocationClues attrs.id
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 1) n
      pure l
    _ -> SacredWoods_184 <$> runMessage msg attrs
