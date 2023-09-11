module Arkham.Location.Cards.DowntownFirstBankOfArkham (
  DowntownFirstBankOfArkham (..),
  downtownFirstBankOfArkham,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (downtownFirstBankOfArkham)
import Arkham.Location.Runner

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownFirstBankOfArkham :: LocationCard DowntownFirstBankOfArkham
downtownFirstBankOfArkham = location DowntownFirstBankOfArkham Cards.downtownFirstBankOfArkham 3 (PerPlayer 1)

instance HasAbilities DowntownFirstBankOfArkham where
  getAbilities (DowntownFirstBankOfArkham attrs) =
    withRevealedAbilities attrs
      $ [ limitedAbility (PlayerLimit PerGame 1)
            $ restrictedAbility attrs 1 (Here <> CanGainResources)
            $ ActionAbility Nothing (ActionCost 1)
        ]

instance RunMessage DowntownFirstBankOfArkham where
  runMessage msg l@(DowntownFirstBankOfArkham attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ TakeResources iid 3 (toAbilitySource attrs 1) False
      pure l
    _ -> DowntownFirstBankOfArkham <$> runMessage msg attrs
