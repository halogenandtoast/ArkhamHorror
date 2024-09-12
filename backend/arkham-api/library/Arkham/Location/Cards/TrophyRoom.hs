module Arkham.Location.Cards.TrophyRoom (trophyRoom, TrophyRoom (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TrophyRoom = TrophyRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trophyRoom :: LocationCard TrophyRoom
trophyRoom = location TrophyRoom Cards.trophyRoom 2 (Static 0)

instance HasAbilities TrophyRoom where
  getAbilities (TrophyRoom a) =
    extendRevealed
      a
      [ playerLimit PerRound
          $ restrictedAbility
            a
            1
            (Here <> youExist (oneOf [can.gain.resources, investigatorWithSpendableResources 2]))
            actionAbility
      ]

instance RunMessage TrophyRoom where
  runMessage msg l@(TrophyRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canGainResources <- can.gain.resources iid
      hasTwoResources <- (>= 2) <$> getSpendableResources iid
      chooseOrRunOne iid
        $ [ Label "Gain 2 Resources" [TakeResources iid 2 (attrs.ability 1) False]
          | canGainResources
          ]
        <> [ Label "Spend 2 Resources to gain 1 clue" [SpendResources iid 2, GainClues iid (attrs.ability 1) 1]
           | hasTwoResources
           ]
      pure l
    _ -> TrophyRoom <$> liftRunMessage msg attrs
