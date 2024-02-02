module Arkham.Location.Cards.MerchantDistrict_300 (
  merchantDistrict_300,
  MerchantDistrict_300 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.BreachStatus
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype MerchantDistrict_300 = MerchantDistrict_300 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

merchantDistrict_300 :: LocationCard MerchantDistrict_300
merchantDistrict_300 = location MerchantDistrict_300 Cards.merchantDistrict_300 2 (Static 0)

instance HasAbilities MerchantDistrict_300 where
  getAbilities (MerchantDistrict_300 attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here
          $ ActionAbility []
          $ ActionCost 1
          <> OrCost [DiscardTopOfDeckCost n | n <- [5, 10, 15]]
      ]

getCardCount :: Payment -> [Card]
getCardCount = \case
  DiscardCardPayment xs -> xs
  Payments ps -> concatMap getCardCount ps
  _ -> []

instance RunMessage MerchantDistrict_300 where
  runMessage msg l@(MerchantDistrict_300 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getCardCount -> cards) -> do
      let weaknesses = filter (`cardMatch` WeaknessCard) cards
      let n = min (maybe 0 countBreaches $ locationBreaches attrs) (length cards `div` 5)
      act <- selectJust AnyAct
      pushAll
        [ RemoveBreaches (toTarget attrs) n
        , PlaceBreaches (toTarget act) n
        , AddToHand iid weaknesses
        ]
      pure l
    _ -> MerchantDistrict_300 <$> runMessage msg attrs
